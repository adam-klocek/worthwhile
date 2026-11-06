# Additive schema 5. Existing task/log/role columns and identifiers are unchanged.
v2_tables <- function()c('categories','ratings','chores','chore_checks','session_details','role_reviews','attendance','reflections')
v2_templates <- function()list(
 categories=data.frame(id=c('social','career'),label=c('Social impact','Career relevance'),enabled=TRUE,weight=c(.05,.1),stringsAsFactors=FALSE),
 ratings=data.frame(task_id=character(),category_id=character(),value=numeric()),
 chores=data.frame(id=character(),title=character(),period=character(),minutes=numeric(),start_date=character(),active=logical()),
 chore_checks=data.frame(chore_id=character(),period=character(),date=character()),
 session_details=data.frame(log_id=character(),start=numeric(),units=numeric(),rate=numeric(),pay_type=character(),billable=logical()),
 role_reviews=data.frame(job_id=character(),week=character(),complete=logical(),target=numeric(),rate=numeric(),salary=numeric(),pay_type=character(),note=character()),
 attendance=data.frame(id=character(),source_id=character(),job_id=character(),date=character(),title=character(),start=numeric(),end=numeric()),
 reflections=data.frame(period=character(),date=character(),note=character()))
v2_upgrade <- function(s) {
 for(n in v2_tables())if(is.null(s[[n]]))s[[n]]<-v2_templates()[[n]]
 defaults<-list(reserve_pct=20,timesheet_start=7,timesheet_end=17,timesheet_lunch_start=12,timesheet_lunch_end=13)
 for(n in names(defaults))if(is.null(s$settings[[n]]))s$settings[[n]]<-defaults[[n]]
 if(identical(s$settings$plan_strategy,'Social impact'))s$settings$plan_strategy<-'Category:social'
 if(identical(s$settings$plan_strategy,'Career'))s$settings$plan_strategy<-'Category:career'
 s$schema<-5L;s
}
category_scores <- function(s,t,id) {
 if(id%in%c('social','career'))return(t[[id]])
 r<-s$ratings[s$ratings$category_id==id,,drop=FALSE];v<-r$value[match(t$id,r$task_id)];v[is.na(v)]<-0;v
}
category_blend <- function(s,t) {
 out<-rep(0,nrow(t));d<-s$categories[s$categories$enabled,,drop=FALSE]
 for(i in seq_len(nrow(d)))out<-out+d$weight[i]*category_scores(s,t,d$id[i])/5
 out
}
decision_lenses <- function(s) {
 base<-setdiff(strategies,c('Social impact','Career'))
 d<-s$categories[s$categories$enabled,,drop=FALSE]
 c(setNames(base,base),setNames(paste0('Category:',d$id),d$label))
}
clock_hours <- function(x) {
 if(length(x)!=1||is.na(x)||!grepl('^([01][0-9]|2[0-3]):[0-5][0-9]$',x))stop('Use a valid time in HH:MM format.')
 a<-as.numeric(strsplit(x,':',fixed=TRUE)[[1]]);a[1]+a[2]/60
}
valid_date <- function(x) {d<-suppressWarnings(as.Date(x,format='%Y-%m-%d'));!is.na(d)&format(d,'%Y-%m-%d')==x}
validate_v2 <- function(s) {
 fail<-function(m)stop(m,call.=FALSE)
 for(n in v2_tables()) {
  d<-s[[n]];ref<-v2_templates()[[n]]
  if(!is.data.frame(d)||!identical(names(d),names(ref))||anyNA(d)||nrow(d)>20000)fail(paste('Invalid version 2 table:',n))
  for(c in names(ref))if((is.numeric(ref[[c]])&&(!is.numeric(d[[c]])||any(!is.finite(d[[c]]))))||(is.logical(ref[[c]])&&!is.logical(d[[c]]))||(is.character(ref[[c]])&&!is.character(d[[c]])))fail(paste('Invalid',n,c))
 }
 c<-s$categories
 if(anyDuplicated(c$id)||any(!grepl('^[a-z][a-z0-9_]*$',c$id))||any(!nzchar(trimws(c$label)))||anyDuplicated(tolower(c$label))||any(c$weight<0|c$weight>1))fail('Use unique category names and weights between 0 and 1.')
 if(any(c$id%in%c('personal','consequence')))fail('Personal meaning and consequences are permanent categories.')
 r<-s$ratings;if(any(!r$task_id%in%record_ids(s,'tasks'))||any(!r$category_id%in%c$id)||any(r$value<0|r$value>5)||anyDuplicated(paste(r$task_id,r$category_id)))fail('Invalid task category rating.')
 c<-s$chores
 if(anyDuplicated(c$id)||any(!grepl('^[A-Za-z][A-Za-z0-9_]*$',c$id))||any(!nzchar(trimws(c$title)))||any(!c$period%in%c('Daily','Weekly'))||any(c$minutes<0|c$minutes>1440)||any(!valid_date(c$start_date)))fail('Invalid chore.')
 q<-s$chore_checks;if(any(!q$chore_id%in%c$id)||any(!q$period%in%c('Daily','Weekly'))||any(!valid_date(q$date))||anyDuplicated(paste(q$chore_id,q$period,q$date)))fail('Invalid chore completion.')
 d<-s$session_details;l<-all_records(s,'logs')
 if(anyDuplicated(d$log_id)||any(!d$log_id%in%l$id)||any(d$start< -1|d$start>=24|d$units<0|d$rate<0)||any(!d$pay_type%in%c('Salary','Hourly','Piece')))fail('Invalid session detail.')
 z<-s$role_reviews
 if(any(!z$job_id%in%record_ids(s,'jobs'))||any(!valid_date(z$week))||any(z$week!=as.character(vapply(z$week,function(x)as.character(week_start(x)),character(1))))||any(z$target<0|z$rate<0|z$salary<0)||any(!z$pay_type%in%c('Salary','Hourly','Piece'))||anyDuplicated(paste(z$job_id,z$week)))fail('Invalid weekly role review.')
 a<-s$attendance
 if(anyDuplicated(a$id)||any(!a$job_id%in%record_ids(s,'jobs'))||any(!valid_date(a$date))||any(a$start<0|a$end>24|a$start>=a$end))fail('Invalid confirmed meeting.')
 f<-s$reflections;if(any(!f$period%in%c('Weekly','Monthly'))||any(!valid_date(f$date))||anyDuplicated(paste(f$period,f$date)))fail('Invalid reflection note.')
 st<-s$settings
 for(n in c('reserve_pct','timesheet_start','timesheet_end','timesheet_lunch_start','timesheet_lunch_end'))if(length(st[[n]])!=1||!is.numeric(st[[n]])||!is.finite(st[[n]]))fail('Invalid reflection preference.')
 if(st$reserve_pct<0||st$reserve_pct>70||st$timesheet_start<0||st$timesheet_end>24||st$timesheet_start>=st$timesheet_lunch_start||st$timesheet_lunch_start>=st$timesheet_lunch_end||st$timesheet_lunch_end>=st$timesheet_end)fail('Keep the lunch break inside the working window; reserve must be 0–70%.')
 TRUE
}
set_session_details <- function(s,log_id,start=-1,units=0) {
 l<-all_records(s,'logs');i<-match(log_id,l$id);if(is.na(i))stop('Choose an existing work session.')
 t<-all_records(s,'tasks');j<-all_records(s,'jobs');r<-j[match(l$job_id[i],j$id),];task<-t[match(l$task_id[i],t$id),]
 if(length(start)!=1||!is.finite(start)||start< -1||start>=24||length(units)!=1||!is.finite(units)||units<0)stop('Enter a valid session time and nonnegative paid items.')
 old<-match(log_id,s$session_details$log_id)
 row<-data.frame(log_id=log_id,start=start,units=units,rate=r$rate,pay_type=r$pay_type,billable=task$billable)
 old_units<-if(is.na(old))0 else s$session_details$units[old]
 if(is.na(old))s$session_details<-rbind(s$session_details,row)else{row$rate<-s$session_details$rate[old];row$pay_type<-s$session_details$pay_type[old];row$billable<-s$session_details$billable[old];s$session_details[old,]<-row}
 ti<-match(l$task_id[i],s$tasks$id);if(!is.na(ti)&&!s$tasks$done[ti]&&row$pay_type=='Piece'&&row$billable)s$tasks$units[ti]<-max(0,s$tasks$units[ti]-(units-old_units))
 s
}
chore_period <- function(period,date)as.character(if(period=='Weekly')week_start(date)else as.Date(date))
chore_done <- function(s,id,date) {
 i<-match(id,s$chores$id);if(is.na(i))return(FALSE)
 p<-s$chores$period[i];any(s$chore_checks$chore_id==id&s$chore_checks$period==p&s$chore_checks$date==chore_period(p,date))
}
toggle_chore <- function(s,id,date) {
 i<-match(id,s$chores$id);if(is.na(i)||!s$chores$active[i])stop('Choose an active chore.')
 p<-s$chores$period[i];d<-chore_period(p,date);hit<-s$chore_checks$chore_id==id&s$chore_checks$period==p&s$chore_checks$date==d
 if(any(hit))s$chore_checks<-s$chore_checks[!hit,,drop=FALSE]else s$chore_checks<-rbind(s$chore_checks,data.frame(chore_id=id,period=p,date=d));s
}
# Expand wall-clock commitments to dated, same-day intervals, including overnight tails.
commitment_events <- function(s,from,to,kinds=c('Essential','Protected','Meeting')) {
 out<-data.frame(id=character(),source_id=character(),date=character(),title=character(),kind=character(),job_id=character(),start=numeric(),end=numeric())
 from<-as.Date(from);to<-as.Date(to)
 if(to<from)return(out)
 add<-function(id,source,date,title,kind,job,start,end){if(date>=from&&date<=to&&end>start)out<<-rbind(out,data.frame(id=id,source_id=source,date=as.character(date),title=title,kind=kind,job_id=job,start=start,end=end))}
 for(ds in as.character(seq(from-1,to,by='day'))) {
  d<-as.Date(ds);dow<-as.integer(format(d,'%u'));b<-s$blocks[s$blocks$day==dow&s$blocks$kind%in%kinds,,drop=FALSE]
  for(i in seq_len(nrow(b))) {
   r<-b[i,];id<-paste0(r$id,'_',ds)
   if(r$end>r$start)add(id,r$id,d,r$title,r$kind,r$job_id,r$start,r$end)else{add(id,r$id,d,r$title,r$kind,r$job_id,r$start,24);add(paste0(id,'_tail'),r$id,d+1,r$title,r$kind,r$job_id,0,r$end)}
  }
 }
 x<-s$exceptions;for(i in seq_len(nrow(x)))if(x$kind[i]!='Available') {
  kind<-if(x$kind[i]=='Meeting')'Meeting'else'Essential'
  if(kind%in%kinds)add(x$id[i],x$id[i],as.Date(x$date[i]),x$title[i],kind,x$job_id[i],x$start[i],x$end[i])
 }
 out
}
save_role_review <- function(s,job_id,week,complete,note='') {
 w<-as.character(week_start(week));j<-s$jobs[match(job_id,s$jobs$id),];if(!nrow(j)||is.na(j$id))stop('Choose a current work role.')
 i<-which(s$role_reviews$job_id==job_id&s$role_reviews$week==w)
 row<-data.frame(job_id=job_id,week=w,complete=isTRUE(complete),target=job_targets(s,s$settings$fulltime)[match(job_id,s$jobs$id)],rate=j$rate,salary=j$weekly_income,pay_type=j$pay_type,note=note)
 if(length(i)){row$target<-s$role_reviews$target[i];row$rate<-s$role_reviews$rate[i];row$salary<-s$role_reviews$salary[i];row$pay_type<-s$role_reviews$pay_type[i];s$role_reviews[i,]<-row}else s$role_reviews<-rbind(s$role_reviews,row)
 s
}
role_week_report <- function(s,week,from=NULL,to=NULL) {
 w<-week_start(week);a<-if(is.null(from))w else max(w,as.Date(from));b<-if(is.null(to))w+6 else min(w+6,as.Date(to))
 j<-all_records(s,'jobs');l<-dated_work_logs(s,a,b);at<-s$attendance;at<-at[at$date>=as.character(a)&at$date<=as.character(b),,drop=FALSE]
 planned<-commitment_events(s,a,b,'Meeting');reviews<-s$role_reviews
 out<-data.frame()
 for(i in seq_len(nrow(j))) {
  r<-j[i,];rv<-reviews[reviews$job_id==r$id&reviews$week==as.character(w),,drop=FALSE];saved<-nrow(rv)==1
  if(!r$id%in%s$jobs$id&&!saved&&!r$id%in%l$job_id&&!r$id%in%at$job_id)next
  target<-if(saved)rv$target else if(r$fte>0)r$fte*s$settings$fulltime else r$target
  rate<-if(saved)rv$rate else r$rate;pay<-if(saved)rv$pay_type else r$pay_type
  # Partial weeks use weekdays proportion; whole weeks keep the entered commitment.
  fraction<-sum(as.integer(format(seq(a,b,by='day'),'%u'))<=5)/5
  target<-target*fraction;complete<-saved&&rv$complete
  z<-l[l$job_id==r$id,,drop=FALSE];m<-at[at$job_id==r$id,,drop=FALSE]
  # Confirmed meetings do not double-count overlapping timed work sessions.
  meeting_h<-sum(m$end-m$start)
  if(nrow(m))for(k in seq_len(nrow(m))) {
   d<-s$session_details[match(z$id,s$session_details$log_id),,drop=FALSE];valid<-z$clock_start>=0&z$date==m$date[k]
   if(any(valid)) {segments<-lapply(which(valid),function(q)c(max(m$start[k],z$clock_start[q]),min(m$end[k],z$clock_start[q]+z$hours[q])));ticks<-seq(m$start[k],m$end[k],by=1/60);ticks<-head(ticks,-1);overlap<-vapply(ticks,function(t)any(vapply(segments,function(v)t>=v[1]&&t<v[2],logical(1))),logical(1));meeting_h<-meeting_h-sum(overlap)/60}
  }
  actual<-sum(z$hours)+max(0,meeting_h);details<-s$session_details[match(z$id,s$session_details$log_id),,drop=FALSE]
  units<-sum(z$part_units,na.rm=TRUE);earnings<-0;unknown<-0
  for(k in seq_len(nrow(z))) {
   known<-!is.na(details$rate[k]);rr<-if(known)details$rate[k]else r$rate;pp<-if(known)details$pay_type[k]else r$pay_type
   task<-all_records(s,'tasks');bill<-if(known)details$billable[k]else task$billable[match(z$task_id[k],task$id)]
   if(pp=='Hourly'&&isTRUE(bill))earnings<-earnings+z$hours[k]*rr
   if(pp=='Piece'&&known&&isTRUE(bill))earnings<-earnings+z$part_units[k]*rr
   if(pp=='Piece'&&!known)unknown<-unknown+1
  }
  t<-all_records(s,'tasks');finished<-t[t$job_id==r$id&t$done&t$completed>=as.character(a)&t$completed<=as.character(b),,drop=FALSE]
  earnings<-earnings+sum(finished$bonus)
  saved_h<-if(pay=='Salary'&&complete)max(0,target-actual)else 0;over<-if(pay=='Salary')max(0,actual-target)else 0
  out<-rbind(out,data.frame(Job_id=r$id,Role=r$name,Week=as.character(w),Pay=pay,Target_h=target,Logged_h=sum(z$hours),Confirmed_meeting_h=max(0,meeting_h),Scheduled_meeting_h=sum(planned$end[planned$job_id==r$id]-planned$start[planned$job_id==r$id]),Actual_h=actual,Still_needed_h=if(complete)0 else max(0,target-actual),Complete=complete,Saved_h=saved_h,Over_h=over,Time_value=(saved_h-over)*rate,Rate=rate,Units=units,Gross_extra=earnings,Unknown_item_sessions=unknown,Snapshot=saved))
 }
 out
}
period_capacity <- function(s,from,to,today=Sys.Date(),reserve=s$settings$reserve_pct) {
 from<-as.Date(from);to<-as.Date(to);total<-0;period_plan<-plan_period(s,from,to,today=today);planned<-sum(period_plan$hours)
 for(ws in as.character(seq(week_start(from),week_start(to),by='7 days'))) {
  w<-as.Date(ws)
  g<-remaining_grid(capacity_grid(s,s$settings$plan_protect %||% TRUE,reserve,s$settings$plan_cap %||% 8,w),w,today,s$settings$timezone);date<-w+g$day-1;total<-total+sum(g$usable&date>=from&date<=to)/4
 }
 # Flexible chores consume capacity, even though they have no fixed slot.
 chores<-0;seen<-character()
 for(ds in as.character(seq(max(from,as.Date(today)),max(max(from,as.Date(today)),to),by='day')))if(as.Date(ds)<=to) {
  d<-as.Date(ds);c<-s$chores[s$chores$active&as.Date(s$chores$start_date)<=d,,drop=FALSE]
  for(i in seq_len(nrow(c)))if(!chore_done(s,c$id[i],d)){key<-paste(c$id[i],chore_period(c$period[i],d));if(!key%in%seen){chores<-chores+c$minutes[i]/60;seen<-c(seen,key)}}
 }
 list(capacity=total,planned=planned,chores=chores,free=max(0,total-planned-chores),over=max(0,planned+chores-total))
}
# Traceable actual ledger. Legacy sessions have no invented start/end time.
actual_timesheet <- function(s,from,to) {
 out<-data.frame(Entry_id=character(),Date=character(),Role_id=character(),Role=character(),Activity=character(),Start=character(),End=character(),Hours=numeric(),Items=numeric(),Type=character(),Status=character())
 l<-all_records(s,'logs');t<-all_records(s,'tasks');j<-all_records(s,'jobs');l<-l[l$date<=as.character(to),,drop=FALSE]
 fmt<-function(h)if(h<0)''else sprintf('%02d:%02d',floor(h),round((h-floor(h))*60))
 append<-function(id,date,role,title,start,end,hours,items,type,status,role_id='')out<<-rbind(out,data.frame(Entry_id=id,Date=date,Role_id=role_id,Role=role,Activity=title,Start=start,End=end,Hours=hours,Items=items,Type=type,Status=status))
 for(i in seq_len(nrow(l))) {
  d<-s$session_details[match(l$id[i],s$session_details$log_id),,drop=FALSE];a<-if(nrow(d)&&!is.na(d$start))d$start else -1;h<-l$hours[i];units<-if(nrow(d)&&!is.na(d$units))d$units else 0
  if(a<0&&as.Date(l$date[i])>=as.Date(from))append(l$id[i],l$date[i],j$name[match(l$job_id[i],j$id)],t$title[match(l$task_id[i],t$id)],'','',h,units,'Task','Recorded duration; clock time unknown',l$job_id[i])else if(a>=0) {
   # Split overnight work across actual dates, keeping a single item quantity.
   left<-h;date<-as.Date(l$date[i]);pos<-a;part<-0
   while(left>1e-8){take<-min(left,24-pos);if(date>=as.Date(from)&&date<=as.Date(to))append(paste0(l$id[i],if(part)paste0('_',part)else''),as.character(date),j$name[match(l$job_id[i],j$id)],t$title[match(l$task_id[i],t$id)],fmt(pos),fmt(pos+take),take,if(part)0 else units,'Task','Recorded',l$job_id[i]);left<-left-take;date<-date+1;pos<-0;part<-part+1}
  }
 }
 a<-s$attendance;a<-a[a$date>=as.character(from)&a$date<=as.character(to),,drop=FALSE]
 for(i in seq_len(nrow(a)))append(a$id[i],a$date[i],j$name[match(a$job_id[i],j$id)],a$title[i],fmt(a$start[i]),fmt(a$end[i]),a$end[i]-a$start[i],0,'Meeting','Attendance confirmed',a$job_id[i])
 p<-commitment_events(s,from,to,'Meeting');p<-p[!p$id%in%a$id,,drop=FALSE]
 for(i in seq_len(nrow(p)))append(p$id[i],p$date[i],j$name[match(p$job_id[i],j$id)],p$title[i],fmt(p$start[i]),fmt(p$end[i]),p$end[i]-p$start[i],0,'Meeting','Scheduled; attendance unconfirmed',p$job_id[i])
 out[order(out$Date,out$Start,out$Entry_id),,drop=FALSE]
}
timesheet_flags <- function(d,start=7,end=17,lunch_start=12,lunch_end=13) {
 d$Review<-''
 for(i in seq_len(nrow(d))) {
  reasons<-character();weekend<-as.integer(format(as.Date(d$Date[i]),'%u'))>5
  if(weekend)reasons<-c(reasons,'Weekend')
  if(!nzchar(d$Start[i]))reasons<-c(reasons,'Clock time unknown')else {
   a<-clock_hours(d$Start[i]);b<-if(d$End[i]=='24:00')24 else clock_hours(d$End[i]);if(a<start||b>end)reasons<-c(reasons,'Outside chosen window');if(a<lunch_end&&b>lunch_start)reasons<-c(reasons,'Overlaps lunch window')
   for(k in seq_len(nrow(d)))if(k!=i&&d$Date[k]==d$Date[i]&&nzchar(d$Start[k])){x<-clock_hours(d$Start[k]);y<-if(d$End[k]=='24:00')24 else clock_hours(d$End[k]);if(max(a,x)<min(b,y)){reasons<-c(reasons,'Overlapping entries');break}}
  }
  if(d$Status[i]=='Scheduled; attendance unconfirmed')reasons<-c(reasons,'Confirm attendance')
  d$Review[i]<-paste(unique(reasons),collapse='; ')
 }
 d
}
# A separately labelled prospective allocation, never an altered attendance record.
working_window_draft <- function(s,from,to,start=7,end=17,lunch_start=12,lunch_end=13) {
 actual<-actual_timesheet(s,from,to);actual<-actual[actual$Status!='Scheduled; attendance unconfirmed',,drop=FALSE]
 out<-data.frame(Source_entry=character(),Source_date=character(),Planned_date=character(),Role_id=character(),Role=character(),Activity=character(),Start=character(),End=character(),Hours=numeric(),Record_type=character())
 dates<-seq(as.Date(from),as.Date(to),by='day');dates<-dates[as.integer(format(dates,'%u'))<=5]
 slots<-data.frame(date=character(),minute=numeric(),used=logical())
 for(ds in as.character(dates)){mins<-seq(ceiling(start*60),floor(end*60)-1);mins<-mins[mins<lunch_start*60|mins>=lunch_end*60];slots<-rbind(slots,data.frame(date=ds,minute=mins,used=FALSE))
 out<-rbind(out,data.frame(Source_entry='',Source_date='',Planned_date=ds,Role_id='',Role='',Activity='Lunch break — planning placeholder',Start=sprintf('%02d:%02d',floor(lunch_start),round(lunch_start%%1*60)),End=sprintf('%02d:%02d',floor(lunch_end),round(lunch_end%%1*60)),Hours=lunch_end-lunch_start,Record_type='Planned break; not proof of attendance'))}
 fmt<-function(m)sprintf('%02d:%02d',floor(m/60),m%%60)
 # Preserve fitting, non-overlapping recorded entries first; allocate all other durations around them.
 kept<-rep(FALSE,nrow(actual));flag<-timesheet_flags(actual,start,end,lunch_start,lunch_end)
 for(i in seq_len(nrow(actual)))if(!nzchar(flag$Review[i])) {
  a<-clock_hours(actual$Start[i])*60;b<-if(actual$End[i]=='24:00')1440 else clock_hours(actual$End[i])*60;ix<-which(slots$date==actual$Date[i]&slots$minute>=round(a)&slots$minute<round(b))
  if(length(ix)==round(actual$Hours[i]*60)&&!any(slots$used[ix])){slots$used[ix]<-TRUE;kept[i]<-TRUE;out<-rbind(out,data.frame(Source_entry=actual$Entry_id[i],Source_date=actual$Date[i],Planned_date=actual$Date[i],Role_id=actual$Role_id[i],Role=actual$Role[i],Activity=actual$Activity[i],Start=actual$Start[i],End=actual$End[i],Hours=actual$Hours[i],Record_type='Recorded entry retained in planning draft'))}
 }
 for(i in which(!kept)) {
  need<-round(actual$Hours[i]*60);free<-which(!slots$used);free<-free[order(slots$date[free]!=actual$Date[i],slots$date[free],slots$minute[free])];take<-head(free,need)
  if(length(take)) {
   take<-sort(take);slots$used[take]<-TRUE;runs<-split(take,cumsum(c(TRUE,diff(take)!=1|diff(slots$minute[take])!=1|head(slots$date[take],-1)!=tail(slots$date[take],-1))))
   for(run in runs)out<-rbind(out,data.frame(Source_entry=actual$Entry_id[i],Source_date=actual$Date[i],Planned_date=slots$date[run[1]],Role_id=actual$Role_id[i],Role=actual$Role[i],Activity=actual$Activity[i],Start=fmt(slots$minute[run[1]]),End=fmt(slots$minute[tail(run,1)]+1),Hours=length(run)/60,Record_type='Hypothetical allocation; NOT actual work time'))
  }
  if(length(take)<need)out<-rbind(out,data.frame(Source_entry=actual$Entry_id[i],Source_date=actual$Date[i],Planned_date='',Role_id=actual$Role_id[i],Role=actual$Role[i],Activity=actual$Activity[i],Start='',End='',Hours=(need-length(take))/60,Record_type='Unallocated: insufficient weekday capacity'))
 }
 out[order(out$Planned_date,out$Start),,drop=FALSE]
}

# Split exact overnight sessions by actual local date for weekly/monthly totals.
dated_work_logs <- function(s,from,to) {
 l<-all_records(s,'logs');out<-l[FALSE,,drop=FALSE];out$clock_start<-numeric();out$part_units<-numeric()
 for(i in seq_len(nrow(l))) {
  d<-s$session_details[match(l$id[i],s$session_details$log_id),,drop=FALSE];start<-if(nrow(d)&&!is.na(d$start))d$start else -1;units<-if(nrow(d)&&!is.na(d$units))d$units else 0
  if(start<0){if(as.Date(l$date[i])>=as.Date(from)&&as.Date(l$date[i])<=as.Date(to)){r<-l[i,,drop=FALSE];r$clock_start<- -1;r$part_units<-units;out<-rbind(out,r)};next}
  left<-l$hours[i];date<-as.Date(l$date[i]);a<-start;first<-TRUE
  while(left>1e-8){h<-min(left,24-a);if(date>=as.Date(from)&&date<=as.Date(to)){r<-l[i,,drop=FALSE];r$date<-as.character(date);r$hours<-h;r$clock_start<-a;r$part_units<-if(first)units else 0;out<-rbind(out,r)};left<-left-h;a<-0;date<-date+1;first<-FALSE}
 }
 out
}
delete_work_session <- function(s,id) {
 d<-s$session_details[s$session_details$log_id==id,,drop=FALSE];l<-all_records(s,'logs');i<-match(id,l$id)
 if(is.na(i))stop('Choose an existing work session.')
 ti<-match(l$task_id[i],s$tasks$id)
 if(nrow(d)&&!is.na(ti)&&!s$tasks$done[ti]&&d$pay_type=='Piece'&&d$billable)s$tasks$units[ti]<-s$tasks$units[ti]+d$units
 s$logs<-s$logs[s$logs$id!=id,,drop=FALSE];s$archive$logs<-s$archive$logs[s$archive$logs$id!=id,,drop=FALSE];s$session_details<-s$session_details[s$session_details$log_id!=id,,drop=FALSE];s
}

needs_guided_setup <- function(s)!isTRUE(s$settings$setup_complete)&&!isTRUE(s$settings$example_mode)&&nrow(s$jobs)==0

save_personal_commitment <- function(s,id='',title,days,start,end) {
 if(!nzchar(trimws(title))||!length(days))stop('Name the activity and choose its weekdays.')
 if(nzchar(id)&&(!id%in%s$blocks$id||s$blocks$kind[match(id,s$blocks$id)]!='Protected'))stop('Choose a current personal activity.')
 s$blocks<-s$blocks[s$blocks$id!=id,,drop=FALSE]
 for(k in seq_along(days)){bid<-if(k==1&&nzchar(id))id else next_id('b',record_ids(s,'blocks'));s$blocks<-rbind(s$blocks,data.frame(id=bid,day=num(days[k]),title=trimws(title),kind='Protected',start=start,end=end,job_id=''))}
 validate_state(s);s
}

# A multi-week projection must not plan the same remaining task effort afresh each week.
# This is a copy: it never completes tasks, advances goals or alters the saved plan.
plan_period <- function(s,from,to,strategy=s$settings$plan_strategy %||% 'Balanced',protect=s$settings$plan_protect %||% TRUE,buffer=s$settings$plan_buffer %||% 15,cap=s$settings$plan_cap %||% 8,seed=1,today=Sys.Date()) {
 from<-as.Date(from);to<-as.Date(to);out<-empty_plan();if(to<from)return(out)
 work<-s;work$locks<-work$locks[work$locks$date>=as.character(max(from,as.Date(today)))&work$locks$date<=as.character(to),,drop=FALSE]
 for(id in unique(work$locks$task_id)){i<-match(id,work$tasks$id);if(!is.na(i)&&!work$tasks$done[i]&&sum(work$locks$end[work$locks$task_id==id]-work$locks$start[work$locks$task_id==id])>work$tasks$remaining[i]+1e-8)stop('Manual blocks across this period exceed a task’s remaining effort. Adjust or release those blocks in Calendar.')}
 for(ws in as.character(seq(week_start(from),week_start(to),by='7 days'))) {
  w<-as.Date(ws);temp<-work;future<-work$locks[work$locks$date>as.character(w+6),,drop=FALSE]
  if(nrow(future))for(id in unique(future$task_id)){i<-match(id,temp$tasks$id);if(!is.na(i))temp$tasks$remaining[i]<-max(0,temp$tasks$remaining[i]-sum(future$end[future$task_id==id]-future$start[future$task_id==id]))}
  p<-plan_work(temp,strategy,w,protect,buffer,cap,seed,max(as.Date(today),from))$plan
  p<-p[p$date>=as.character(from)&p$date<=as.character(to),,drop=FALSE];out<-rbind(out,p)
  for(id in unique(p$task_id)){i<-match(id,work$tasks$id);work$tasks$remaining[i]<-max(0,work$tasks$remaining[i]-sum(p$hours[p$task_id==id]))}
 }
 out
}
