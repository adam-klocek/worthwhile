job_icons <- c('graduation-cap','flask','book','broom','laptop-code','briefcase','palette','heart','leaf','wrench','music','globe')
fmt_hour <- function(x) sprintf('%02d:%02d',floor(x),round((x-floor(x))*60))
metric <- function(label,value,note='') tags$div(class='metric',tags$span(class='eyebrow',label),tags$strong(value),tags$small(note))
section <- function(title,...,class='panel') tags$section(class=class,tags$h3(title),...)
form_row <- function(...) fluidRow(...)
choices_id <- function(d,label='title',blank=FALSE) {
  x <- setNames(d$id,d[[label]])
  if(blank) c('None'='',x) else x
}
notify_error <- function(e) showNotification(conditionMessage(e),type='error',duration=8)
commit_safely <- function(commit,s) tryCatch(commit(s),error=notify_error)
select_event <- function(ns,event,id,label,cls='btn btn-default') tags$button(type='button',class=cls,'aria-label'=if(startsWith(event,'remove_'))paste('Remove',sub('remove_','',event))else NULL,onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",ns(event),id),label)
progress_bar <- function(fraction) tags$div(class='progress-track',tags$div(class='progress-fill',style=paste0('width:',100*clamp(fraction),'%')))
read_backup <- function(path) decode_state(jsonlite::fromJSON(path,simplifyVector=TRUE))
decode_state <- function(s) {
  template<-demo_state()
  for(nm in record_tables) {
    d<-s[[nm]]
    if(is.null(d)&&nm=='opportunities')d<-template[[nm]][FALSE,,drop=FALSE]
    if(!is.list(d))stop(paste('Invalid table',nm))
    if(!length(d)||!length(d[[1]]))s[[nm]]<-template[[nm]][FALSE,,drop=FALSE]
    else s[[nm]]<-as.data.frame(d,stringsAsFactors=FALSE)
  }
  for(nm in c('exceptions','locks'))if(!is.null(s[[nm]])){d<-s[[nm]];s[[nm]]<-if(!length(d)||!length(d[[1]]))template[[nm]][FALSE,,drop=FALSE]else as.data.frame(d,stringsAsFactors=FALSE)}
  if(!is.null(s$archive))for(nm in record_tables) {
    d<-s$archive[[nm]]
    if(is.null(d)||!length(d)||!length(d[[1]]))s$archive[[nm]]<-template[[nm]][FALSE,,drop=FALSE]
    else s$archive[[nm]]<-as.data.frame(d,stringsAsFactors=FALSE)
  }
  s<-migrate_state(s);validate_state(s);s
}
svg_tag <- function(name,...) htmltools::tag(name,list(...))
job_palette <- role_palette
job_colour <- function(s,id) job_palette[((match(id,s$jobs$id)-1L)%%length(job_palette))+1L]
accordion <- function(title,...) tags$details(class='advanced',tags$summary(title),...)
week_bars <- function(s,p,anchor) {
  cap<-vapply(1:7,function(d)sum(p$grid$usable[p$grid$day==d])/4,numeric(1));ceiling_h<-max(1,cap,tapply(p$plan$hours,factor(p$plan$day,levels=1:7),sum),na.rm=TRUE)
  tags$div(class='week-bars',lapply(1:7,function(d){day_plan<-p$plan[p$plan$day==d,,drop=FALSE];booked<-sum(day_plan$hours)
    tags$div(class='week-bar-day',tags$span(class='day-hours',paste0(booked,'h')),tags$div(class='day-bar-space',tags$div(class='day-capacity',style=paste0('height:',100*cap[d]/ceiling_h,'%'),title=paste(cap[d],'usable hours')),tags$div(class='day-segments',lapply(s$jobs$id,function(j){h<-sum(day_plan$hours[day_plan$job_id==j]);if(h<=0)return(NULL);tags$span(class='day-segment',style=paste0('height:',100*h/ceiling_h,'%;background:',job_colour(s,j)),title=paste(s$jobs$name[match(j,s$jobs$id)],h,'h'))}))),tags$strong(c('M','T','W','T','F','S','S')[d]),tags$small(format(as.Date(anchor)+d-1,'%d')))
  }))
}
job_legend <- function(s) tags$div(class='job-legend',lapply(seq_len(nrow(s$jobs)),function(i)tags$span(tags$i(style=paste0('background:',job_colour(s,s$jobs$id[i]))),s$jobs$name[i])))
capacity_ring <- function(g,plan) {
  usable<-sum(g$usable)/4;reserved<-sum(g$reserved)/4;booked<-sum(plan$hours);free<-max(0,usable-booked);total<-max(.25,usable+reserved)
  values<-c(booked,free,reserved);colours<-c(design_tokens$accent,design_tokens$lavender,design_tokens$clay);circumference<-2*pi*45;offset<-0
  arcs<-lapply(seq_along(values),function(i){length<-values[i]/total*circumference;tag<-svg_tag('circle',cx=60,cy=60,r=45,fill='none',stroke=colours[i],'stroke-width'=12,'stroke-dasharray'=paste(length,circumference-length),'stroke-dashoffset'=-offset,transform='rotate(-90 60 60)');offset<<-offset+length;tag})
  tags$div(class='capacity-ring-wrap',tags$div(class='capacity-ring',svg_tag('svg',viewBox='0 0 120 120',role='img','aria-label'=paste(usable,'usable hours;',booked,'planned;',free,'open;',reserved,'buffer hours'),arcs),tags$div(class='ring-label',tags$strong(paste0(usable,'h')),tags$small('remaining capacity'))),tags$div(class='ring-legend',lapply(1:3,function(i)tags$div(tags$i(style=paste0('background:',colours[i])),tags$span(c('Planned focus','Still open','Buffer & daily limit')[i]),tags$strong(paste0(values[i],'h'))))))
}
effort_rows <- function(s,p,anchor) {
  log_rows<-all_records(s,'logs');logs<-log_rows[log_rows$date>=as.character(anchor)&log_rows$date<=as.character(anchor+6),,drop=FALSE];target<-job_targets(s,s$settings$fulltime);meet<-job_meeting_hours(s);planned<-vapply(s$jobs$id,function(j)sum(p$plan$hours[p$plan$job_id==j]),numeric(1));actual<-vapply(s$jobs$id,function(j)sum(logs$hours[logs$job_id==j]),numeric(1));top<-max(1,target,meet+planned+actual)
  tags$div(class='effort-rows',lapply(seq_len(nrow(s$jobs)),function(i)tags$div(class='effort-row',tags$div(class='effort-label',tags$span(icon(s$jobs$icon[i]),s$jobs$name[i]),tags$small(paste0(round(actual[i]+meet[i]+planned[i],1),' / ',target[i],'h'))),tags$div(class='effort-track',tags$div(class='effort-fill',style=paste0('width:',100*(actual[i]+meet[i]+planned[i])/top,'%;background:',job_colour(s,s$jobs$id[i]))),tags$span(class='effort-target',style=paste0('left:',100*target[i]/top,'%'),title=paste('Target',target[i],'hours'))))))
}
calendar_html <- function(s,p,anchor,start_hour=6,end_hour=23,days=1:7,today=as.Date(format(Sys.time(),tz=s$settings$timezone,format='%Y-%m-%d'))) {
  span<-end_hour-start_hour
  top_style<-function(a,b)paste0('top:',100*(a-start_hour)/span,'%;height:',100*(b-a)/span,'%')
  tags$div(class='calendar-scroll',tags$div(class='visual-calendar',style=paste0('--calendar-days:',length(days),';--calendar-height:',span*46,'px;--hour-size:',100/span,'%;'),
    tags$div(class='calendar-hours',tags$div(class='calendar-day-head'),tags$div(class='calendar-deadlines hour-label','Due'),tags$div(class='calendar-day-body',lapply(seq(start_hour,end_hour),function(h)tags$span(style=paste0('top:',100*(h-start_hour)/span,'%'),fmt_hour(h))))),
    lapply(days,function(d){g<-p$grid[p$grid$day==d,,drop=FALSE];task<-p$plan[p$plan$day==d,,drop=FALSE];groups<-rle(paste(g$blocked,g$reserved,g$available,g$kind,g$label));ends<-cumsum(groups$lengths);starts<-c(1,head(ends,-1)+1);date<-as.Date(anchor)+d-1
      tags$div(class=paste('calendar-day',if(date==today)'is-today'else''),tags$div(class='calendar-day-head',tags$span(c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')[d]),tags$strong(format(date,'%d'))),tags$div(class='calendar-deadlines',{due<-s$tasks[!s$tasks$done & effective_due(s$tasks)==date,,drop=FALSE];tagList(lapply(seq_len(min(2,nrow(due))),function(i)tags$button(type='button',class=paste('deadline-chip',task_urgency(due[i,],today)),title=due$title[i],onclick=sprintf("Shiny.setInputValue('calendar_pick','%s',{priority:'event'})",due$id[i]),paste(if(due$hard[i])'Due'else'Aim',due$title[i]))),if(nrow(due)>2)tags$small(paste('+',nrow(due)-2,'deadlines')))}),
        tags$div(class='calendar-day-body',lapply(seq_along(starts),function(k){a<-starts[k];b<-ends[k];from<-max(start_hour,g$start[a]);to<-min(end_hour,g$start[b]+.25);if(to<=from||(!g$blocked[a]&&!g$reserved[a]&&!g$available[a]))return(NULL);kind<-if(g$blocked[a])g$kind[a]else if(g$reserved[a])'buffer'else'open';title<-if(g$blocked[a])g$label[a]else if(g$reserved[a])'Breathing room'else''
          tags$div(class=paste('calendar-block',tolower(kind)),style=top_style(from,to),title=paste(title,fmt_hour(g$start[a]),'–',fmt_hour(g$start[b]+.25)),if(to-from>=.75)tags$small(title))}),
          lapply(seq_len(nrow(task)),function(i){r<-task[i,];a<-max(start_hour,r$start);b<-min(end_hour,r$end);if(b<=a)return(NULL);colour<-job_colour(s,r$job_id);t<-s$tasks[match(r$task_id,s$tasks$id),];urgency<-task_urgency(t,today)
            tags$button(type='button',class=paste('calendar-task',urgency),style=paste0(top_style(a,b),';--job-colour:',colour,';background:',colour,'22'),title=paste(r$title,fmt_hour(r$start),'–',fmt_hour(r$end),if(urgency=='urgent')'· Needs attention'else'',if(nzchar(t$collaborators))paste('·',t$collaborators)else''),'aria-label'=paste(r$title,fmt_hour(r$start),'to',fmt_hour(r$end),if(urgency=='urgent')'Needs attention'else''),onclick=sprintf("Shiny.setInputValue('calendar_pick','%s',{priority:'event'})",r$task_id),if(b-a>=.5)tags$strong(r$title),if(b-a>=1)tags$small(paste(fmt_hour(r$start),'–',fmt_hour(r$end),if(urgency=='urgent')'· Attention'else'')))}),
          if(date==today){now<-as.POSIXlt(Sys.time(),tz=s$settings$timezone);h<-now$hour+now$min/60;if(h>=start_hour&&h<=end_hour)tags$div(class='calendar-now',style=paste0('top:',100*(h-start_hour)/span,'%'),title='Now')}
        )
      )
    })
  ))
}
month_calendar <- function(s,p,anchor) {
  first<-as.Date(paste0(format(as.Date(anchor)+3,'%Y-%m'),'-01'));last<-seq(first,by='month',length.out=2)[2]-1
  dates<-seq(week_start(first),week_start(last)+6,by='day')
  tags$div(class='month-calendar',tags$div(class='month-weekdays',lapply(c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'),tags$strong)),tags$div(class='month-grid',lapply(as.character(dates),function(date){d<-as.Date(date);z<-p$plan[p$plan$date==date,,drop=FALSE]
    tags$div(class=paste('month-day',if(format(d,'%m')!=format(first,'%m'))'other-month'else''),tags$button(type='button',class='month-date',onclick=sprintf("Shiny.setInputValue('calendar_date','%s',{priority:'event'})",date),format(d,'%d')),
      lapply(seq_len(min(3,nrow(z))),function(i){t<-s$tasks[match(z$task_id[i],s$tasks$id),];tags$button(type='button',class=paste('month-task',task_urgency(t,as.Date(format(Sys.time(),tz=s$settings$timezone,format='%Y-%m-%d')))),style=paste0('--job-colour:',job_colour(s,z$job_id[i])),onclick=sprintf("Shiny.setInputValue('calendar_pick','%s',{priority:'event'})",z$task_id[i]),paste(fmt_hour(z$start[i]),z$title[i]))}),
      if(nrow(z)>3)tags$small(paste('+',nrow(z)-3,'more blocks')),{due<-s$tasks[!s$tasks$done & effective_due(s$tasks)==d,,drop=FALSE];lapply(seq_len(min(2,nrow(due))),function(i)tags$button(type='button',class=paste('deadline-chip',task_urgency(due[i,],as.Date(format(Sys.time(),tz=s$settings$timezone,format='%Y-%m-%d')))),title=due$title[i],onclick=sprintf("Shiny.setInputValue('calendar_pick','%s',{priority:'event'})",due$id[i]),paste(if(due$hard[i])'Due'else'Aim',due$title[i])))})
  })),tags$p(class='hint','This month overview shows focus blocks for the selected week. Click a date to plan that week. Deadline markers include tasks across the month; other weeks are not fully scheduled here.'))
}
task_urgency <- function(t,today=Sys.Date()) {
  days<-as.numeric(effective_due(t)-as.Date(today))
  ifelse(t$consequence>=5|(t$hard&days<=2),'urgent',ifelse(days<0|(t$hard&days<=7)|t$nudges>=3,'attention','routine'))
}
task_explanation <- function(s,r,p,today,strategy) {
  goal<-s$goals$title[match(r$goal_id,s$goals$id)];hours<-sum(p$plan$hours[p$plan$task_id==r$id]);days<-as.numeric(effective_due(r)-today)
  tags$div(class='explanation',tags$p(paste('Ranked within priority tier',r$tier,'using',strategy,'· score',r$score)),tags$ul(
    tags$li(if(r$blocked)'An unfinished dependency or unresolved block prevents scheduling.'else'No unresolved prerequisite is blocking this task.'),
    tags$li(paste('Deadline:',effective_due(r),if(r$hard)'fixed'else'flexible','·',r$nudges,'nudges · consequence rating',r$consequence,'of 5.')),
    tags$li(paste('Your ratings: personal',r$personal,'· career',r$career,'· social',r$social,'of 5.')),
    if(length(goal)&&!is.na(goal))tags$li(paste('Supports:',goal))else tags$li('No goal is currently linked.'),
    tags$li(paste(round(r$net_hour,2),s$settings$currency,'per hour in estimated marginal net value before tax. Salary is not extra task cash.')),
    tags$li(paste(hours,'of',r$remaining,'remaining hours fit this draft week. A partial fit is not completion.')),
    tags$li(if(r$hard && days<=2)'Delaying this may miss its fixed deadline. The app has no evidence of consequences beyond your entered rating.'else'Any impact of delay depends on your deadline flexibility and consequence rating; it is not independently predicted.')))
}
