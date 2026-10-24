# Dated overlays around the original greedy scheduler; no separate ranking algorithm.
empty_exceptions <- function()data.frame(id=character(),date=character(),start=numeric(),end=numeric(),kind=character(),title=character(),job_id=character(),stringsAsFactors=FALSE)
empty_locks <- function()data.frame(id=character(),task_id=character(),date=character(),start=numeric(),end=numeric(),stringsAsFactors=FALSE)
add_exception <- function(s,date,start,end,kind='Unavailable',title='Availability change',job_id='') {
  s$exceptions<-rbind(s$exceptions,data.frame(id=next_id('x',s$exceptions$id),date=as.character(date),start=start,end=end,kind=kind,title=title,job_id=job_id,stringsAsFactors=FALSE));s
}
apply_dated_grid <- function(g,s,anchor) {
  if(is.null(anchor)||!nrow(s$exceptions))return(g)
  e<-s$exceptions; e<-e[as.Date(e$date)>=as.Date(anchor)&as.Date(e$date)<=as.Date(anchor)+6,,drop=FALSE]
  for(date in unique(e$date[e$kind=='Available'])) {
    d<-as.integer(as.Date(date)-as.Date(anchor))+1L;g$available[g$day==d]<-FALSE
  }
  # Availability cannot remove fixed commitments or protected life.
  for(i in seq_len(nrow(e))) {
    d<-as.integer(as.Date(e$date[i])-as.Date(anchor))+1L;ix<-interval_indices(d,e$start[i],e$end[i])
    if(e$kind[i]=='Available')g$available[ix]<-TRUE else {
      g$blocked[ix]<-TRUE;g$label[ix]<-ifelse(nzchar(g$label[ix]),paste(g$label[ix],e$title[i],sep=' / '),e$title[i]);g$kind[ix]<-if(e$kind[i]=='Meeting')'Meeting'else'Essential';g$job_id[ix]<-e$job_id[i]
    }
  }
  g
}
locks_for_week <- function(s,anchor,today) {
  out<-empty_plan();l<-s$locks
  if(!nrow(l))return(out)
  for(i in seq_len(nrow(l))) {
    r<-l[i,];t<-s$tasks[match(r$task_id,s$tasks$id),,drop=FALSE]
    if(!nrow(t)||is.na(t$id)||t$done||as.Date(r$date)<as.Date(today)||as.Date(r$date)<as.Date(anchor)||as.Date(r$date)>as.Date(anchor)+6)next
    a<-r$start
    local_date<-as.Date(format(Sys.time(),tz=s$settings$timezone,format='%Y-%m-%d'))
    if(as.Date(r$date)==as.Date(today)&&as.Date(today)==local_date){now<-as.POSIXlt(Sys.time(),tz=s$settings$timezone);a<-max(a,ceiling((now$hour+now$min/60)*4)/4)}
    if(a>=r$end)next
    out<-rbind(out,data.frame(task_id=t$id,job_id=t$job_id,title=paste0('Locked · ',t$title),date=r$date,day=as.integer(as.Date(r$date)-as.Date(anchor))+1L,start=a,end=r$end,hours=r$end-a,stringsAsFactors=FALSE))
  }
  out
}
lock_conflicts <- function(s,p,anchor,today) {
  locks<-locks_for_week(s,anchor,today);out<-character();used<-integer()
  ranked<-rank_tasks(s,today=today)
  for(i in seq_len(nrow(locks))) {
    r<-locks[i,];ix<-interval_indices(r$day,r$start,r$end);t<-s$tasks[match(r$task_id,s$tasks$id),]
    if(any(!p$grid$usable[ix]))out<-c(out,paste(t$title,'has a locked block outside usable capacity.'))
    if(any(ix%in%used))out<-c(out,'Locked work blocks overlap.')
    if(isTRUE(ranked$blocked[match(t$id,ranked$id)]))out<-c(out,paste(t$title,'is locked but has an unfinished or unresolved dependency.'))
    if(t$hard && as.Date(r$date)>effective_due(t))out<-c(out,paste(t$title,'is locked after its fixed deadline.'))
    used<-union(used,ix)
  }
  for(id in unique(locks$task_id))if(sum(locks$hours[locks$task_id==id])>s$tasks$remaining[match(id,s$tasks$id)]+1e-8)out<-c(out,paste('Locked hours exceed remaining effort for',s$tasks$title[match(id,s$tasks$id)]))
  unique(out)
}
plan_work <- function(s,strategy='Balanced',anchor=week_start(),protect=TRUE,buffer=15,daily_cap=8,seed=1,today=Sys.Date()) {
  locks<-locks_for_week(s,anchor,today)
  p<-schedule_week(s,strategy,anchor,protect,buffer,daily_cap,seed,today,locked=locks)
  p$conflicts<-lock_conflicts(s,p,anchor,today);p
}
plan_comparison <- function(s,before,after) {
  ids<-unique(c(before$plan$task_id,after$plan$task_id))
  if(!length(ids))return(data.frame(Task=character(),Before_h=numeric(),Proposed_h=numeric(),Change_h=numeric()))
  data.frame(Task=s$tasks$title[match(ids,s$tasks$id)],Before_h=vapply(ids,function(id)sum(before$plan$hours[before$plan$task_id==id]),numeric(1)),Proposed_h=vapply(ids,function(id)sum(after$plan$hours[after$plan$task_id==id]),numeric(1))) |> transform(Change_h=Proposed_h-Before_h)
}
planned_cash <- function(s,p) {
  e<-task_economics(s);sum(vapply(unique(p$plan$task_id),function(id){i<-match(id,s$tasks$id);if(is.na(i)||s$tasks$remaining[i]<=0)return(0);e$net[i]*min(1,sum(p$plan$hours[p$plan$task_id==id])/s$tasks$remaining[i])},numeric(1)))
}
remove_focus_hours <- function(s,hours,anchor,protect,buffer,cap,today) {
  if(!is.finite(hours)||hours<0||abs(hours*4-round(hours*4))>1e-8)stop('Use nonnegative quarter-hour reductions.')
  g<-remaining_grid(capacity_grid(s,protect,buffer,cap,anchor),anchor,today,s$settings$timezone)
  target<-max(0,sum(g$usable)-round(hours*4));cuts<-integer();base<-s
  while(sum(g$usable)>target) {
    ix<-tail(which(g$usable),1);if(!length(ix))break
    cuts<-c(cuts,ix);s<-add_exception(s,as.Date(anchor)+g$day[ix]-1,g$start[ix],g$start[ix]+.25,'Unavailable','Scenario: reduced availability')
    g<-remaining_grid(capacity_grid(s,protect,buffer,cap,anchor),anchor,today,s$settings$timezone)
  }
  if(length(cuts)) {
    s<-base;cuts<-sort(unique(cuts));runs<-split(cuts,cumsum(c(TRUE,diff(cuts)!=1|diff(g$day[cuts])!=0)))
    for(run in runs)s<-add_exception(s,as.Date(anchor)+g$day[run[1]]-1,g$start[run[1]],g$start[tail(run,1)]+.25,'Unavailable','Scenario: reduced availability')
  }
  s
}
validate_overlays <- function(s) {
  fail<-function(m)stop(m,call.=FALSE)
  for(nm in c('exceptions','locks')) {
    d<-s[[nm]];ref<-if(nm=='exceptions')empty_exceptions()else empty_locks()
    if(!is.data.frame(d)||!identical(names(d),names(ref))||anyNA(d)||nrow(d)>10000)fail(paste('Invalid',nm))
    if(anyDuplicated(d$id)||any(!grepl('^[A-Za-z][A-Za-z0-9_]*$',d$id)))fail('Invalid dated-record IDs.')
    if(!is.numeric(d$start)||!is.numeric(d$end)||any(!is.finite(d$start)|!is.finite(d$end))||any(d$start<0|d$end>24|d$start>=d$end|d$start*4!=round(d$start*4)|d$end*4!=round(d$end*4)))fail('Dated intervals must be same-day quarter-hour ranges. Split overnight exceptions across two dates.')
    date<-suppressWarnings(as.Date(d$date,format='%Y-%m-%d'));if(anyNA(date)||any(format(date,'%Y-%m-%d')!=d$date))fail('Invalid exception or locked-block date.')
  }
  if(any(!s$exceptions$kind%in%c('Available','Unavailable','Meeting'))||any(s$exceptions$kind=='Meeting' & !s$exceptions$job_id%in%s$jobs$id))fail('Invalid dated meeting or availability type.')
  if(any(!s$locks$task_id%in%s$tasks$id))fail('Locked work references a missing task.')
  for(i in seq_len(nrow(s$locks)))for(j in seq_len(nrow(s$locks)))if(j>i && s$locks$date[i]==s$locks$date[j]&&max(s$locks$start[i],s$locks$start[j])<min(s$locks$end[i],s$locks$end[j]))fail('Manual locked blocks overlap.')
  for(l in list(s$logs,s$archive$logs))for(col in c('note','created_at','updated_at'))if(!is.character(l[[col]]))fail('Invalid work-session metadata.')
  if(length(s$revision)!=1||!is.finite(s$revision)||s$revision<0)fail('Invalid revision number.')
  if(length(s$log_sequence)!=1||!is.finite(s$log_sequence)||s$log_sequence<0||s$log_sequence!=round(s$log_sequence))fail('Invalid work-session sequence.')
  st<-s$settings
  if(!is.null(st$plan_strategy)&&(!is.character(st$plan_strategy)||length(st$plan_strategy)!=1||!st$plan_strategy%in%strategies))fail('Invalid saved decision lens.')
  for(col in c('plan_buffer','plan_cap'))if(!is.null(st[[col]])) {
    x<-st[[col]];lim<-if(col=='plan_buffer')c(0,70)else c(.25,16)
    if(!is.numeric(x)||length(x)!=1||!is.finite(x)||x<lim[1]||x>lim[2])fail('Invalid saved planning preference.')
  }
  if(!is.null(st$plan_protect)&&(!is.logical(st$plan_protect)||length(st$plan_protect)!=1||is.na(st$plan_protect)))fail('Invalid saved protection preference.')
  invisible(TRUE)
}

remaining_grid <- function(g,anchor,today,timezone,hour=NULL) {
  date<-as.Date(anchor)+g$day-1
  if(is.null(hour)) {
    local_today<-as.Date(format(Sys.time(),tz=timezone,format='%Y-%m-%d'))
    if(as.Date(today)==local_today){now<-as.POSIXlt(Sys.time(),tz=timezone);hour<-ceiling((now$hour+now$min/60)*4)/4}else hour<-0
  }
  elapsed<-date<as.Date(today)|(date==as.Date(today)&g$start<hour)
  for(col in c('usable','free','reserved'))g[[col]][elapsed]<-FALSE
  g
}
