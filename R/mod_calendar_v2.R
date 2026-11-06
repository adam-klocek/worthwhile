calendar_entry <- function(s,date,start,end,kind='Unavailable',title='',job_id='',task_id='',replace_id='') {
 date<-as.character(as.Date(date))
 factor<-if(kind=='Task')4 else 60
 if(!valid_date(date)||!is.finite(start)||!is.finite(end)||start<0||end>24||start>=end||abs(start*factor-round(start*factor))>1e-7||abs(end*factor-round(end*factor))>1e-7)stop('Use a same-day interval: task blocks in 15-minute steps, activities in whole minutes.')
 if(kind=='Task') {
  i<-match(task_id,s$tasks$id);if(is.na(i)||s$tasks$done[i])stop('Choose an unfinished task.')
  if(s$tasks$hard[i]&&as.Date(date)>effective_due(s$tasks[i,]))stop('This would move work after a fixed deadline.')
  other<-s$locks[s$locks$id!=replace_id,,drop=FALSE]
  if(sum(other$end[other$task_id==task_id]-other$start[other$task_id==task_id])+end-start>s$tasks$remaining[i]+1e-8)stop('Locked hours exceed the task’s remaining effort. Edit remaining effort or shorten this block.')
  s$locks<-rbind(other,data.frame(id=if(nzchar(replace_id))replace_id else next_id('m',s$locks$id),task_id=task_id,date=date,start=start,end=end))
 }else {
  if(!kind%in%c('Unavailable','Meeting','Available')||!nzchar(trimws(title)))stop('Name the activity and choose its type.')
  s$exceptions<-s$exceptions[s$exceptions$id!=replace_id,,drop=FALSE]
  s$exceptions<-rbind(s$exceptions,data.frame(id=if(nzchar(replace_id))replace_id else next_id('x',s$exceptions$id),date=date,start=start,end=end,kind=kind,title=trimws(title),job_id=if(kind=='Meeting')job_id else ''))
 }
 validate_state(s);s
}
ics_escape <- function(x) {x<-gsub('\\','\\\\',x,fixed=TRUE);x<-gsub('\n','\\n',x,fixed=TRUE);x<-gsub(';','\\;',x,fixed=TRUE);gsub(',','\\,',x,fixed=TRUE)}
ics_fold <- function(line) {
 chars<-strsplit(enc2utf8(line),'',fixed=TRUE)[[1]];out<-character();part<-''
 for(ch in chars){if(nchar(paste0(part,ch),type='bytes')>73){out<-c(out,part);part<-paste0(' ',ch)}else part<-paste0(part,ch)}
 paste(c(out,part),collapse='\r\n')
}
calendar_ics <- function(s,p,from,to,mode='Full schedule',task_ids=s$tasks$id) {
 lines<-c('BEGIN:VCALENDAR','VERSION:2.0','PRODID:-//Worthwhile//Planner 2.0//EN','CALSCALE:GREGORIAN','METHOD:PUBLISH')
 stamp<-format(Sys.time(),tz='UTC',format='%Y%m%dT%H%M%SZ')
 emit<-function(uid,title,date,start=NULL,end=NULL,description='Draft plan from Worthwhile; not a record of work performed.') {
  date<-as.Date(date);event<-c('BEGIN:VEVENT',paste0('UID:',uid,'@worthwhile'),paste0('DTSTAMP:',stamp),paste0('SUMMARY:',ics_escape(title)),paste0('DESCRIPTION:',ics_escape(description)))
  if(is.null(start))event<-c(event,paste0('DTSTART;VALUE=DATE:',format(date,'%Y%m%d')),paste0('DTEND;VALUE=DATE:',format(date+1,'%Y%m%d')))else {
   ts<-function(h){d<-date+floor(h/24);hh<-h%%24;local<-sprintf('%s %02d:%02d:00',d,floor(hh),round(hh%%1*60));format(as.POSIXct(local,tz=s$settings$timezone),tz='UTC',format='%Y%m%dT%H%M%SZ')}
   event<-c(event,paste0('DTSTART:',ts(start)),paste0('DTEND:',ts(end)))
  }
  lines<<-c(lines,event,'END:VEVENT')
 }
 t<-s$tasks[s$tasks$id%in%task_ids&!s$tasks$done,,drop=FALSE];due<-effective_due(t)
 for(i in seq_len(nrow(t)))if(due[i]>=as.Date(from)&&due[i]<=as.Date(to))emit(paste0('deadline-',t$id[i]),paste(if(t$hard[i])'Deadline ·'else'Aim ·',t$title[i]),due[i],description='Task deadline / target date from Worthwhile.')
 if(mode=='Full schedule') {
  z<-p$plan[p$plan$task_id%in%task_ids&p$plan$date>=as.character(from)&p$plan$date<=as.character(to),,drop=FALSE]
  for(i in seq_len(nrow(z)))emit(paste0('plan-',z$task_id[i],'-',z$date[i],'-',round(z$start[i]*60)),z$title[i],z$date[i],z$start[i],z$end[i])
  b<-commitment_events(s,from,to);for(i in seq_len(nrow(b)))emit(b$id[i],b$title[i],b$date[i],b$start[i],b$end[i],paste('Scheduled',b$kind[i],'commitment.'))
 }
 paste(vapply(c(lines,'END:VCALENDAR'),ics_fold,character(1)),collapse='\r\n')
}
calendarV2UI <- function(id) {
 ns<-NS(id)
 tagList(tags$h2('A calendar with breathing room.'),tags$p(class='lead','Click a time to protect it. Move a task to make the week yours.'),
 section('Your calendar',tags$div(class='calendar-toolbar',tags$div(actionButton(ns('previous'),'‹',title='Previous week'),actionButton(ns('today'),'Today'),actionButton(ns('following'),'›',title='Next week')),uiOutput(ns('heading')),radioButtons(ns('view'),NULL,c('Week','Day','Month'),selected='Week',inline=TRUE)),
 conditionalPanel("input.view == 'Day'",ns=ns,selectInput(ns('day'),'Day',NULL)),checkboxInput(ns('all_day'),'Show full 24-hour day',FALSE),
 tags$div(class='card-actions',actionButton(ns('add_activity'),'Add an activity',icon=icon('plus'),class='btn-primary'),selectInput(ns('adjustment'),'Saved one-off activity or manual work block',NULL),actionButton(ns('edit_adjustment'),'Edit selected')),
 tags$p(class='hint','Drag a work block to another time, or click it to edit its date and time with the keyboard. Empty calendar times add an activity. Task blocks use 15-minute steps. Activities accept exact HH:MM times. Appointments protect their surrounding quarter-hour slots. One-off activities always block automatic work; recurring protected time follows Plan preferences.'),
 uiOutput(ns('calendar_legend')),uiOutput(ns('warnings')),uiOutput(ns('week_calendar')),accordion('Daily agenda',uiOutput(ns('agenda')))),
 section('Unplanned effort and deadline pressure',tableOutput(ns('unplanned'))),
 section('Take your plan with you',radioButtons(ns('export_mode'),'What to export',c('Deadlines only','Full schedule'),selected='Full schedule',inline=TRUE),selectizeInput(ns('export_tasks'),'Tasks to include (empty = all tasks)',NULL,multiple=TRUE),downloadButton(ns('ics'),'Download Google Calendar file (.ics)'),downloadButton(ns('csv'),'Download plan CSV'),tags$p(class='hint','Export covers the displayed week. Full schedule includes timed work blocks, commitments and deadlines. In Google Calendar on a computer: Settings → Import & export → Import. Use a separate Worthwhile calendar to manage repeated imports; this is a one-way snapshot, not live synchronization.')))
}
calendarV2Server <- function(id,state,commit,controls,planning)moduleServer(id,function(input,output,session){
 ns<-session$ns;edit<-reactiveVal(NULL)
 observe({s<-state();e<-s$exceptions;l<-s$locks;choices<-c(if(nrow(e))setNames(paste0('x:',e$id),paste(e$date,e$title,fmt_hour(e$start)))else character(),if(nrow(l))setNames(paste0('m:',l$id),paste(l$date,s$tasks$title[match(l$task_id,s$tasks$id)],fmt_hour(l$start)))else character());updateSelectInput(session,'adjustment',choices=c('Choose an entry'='',choices),selected=isolate(input$adjustment));updateSelectizeInput(session,'export_tasks',choices=choices_id(s$tasks),selected=isolate(input$export_tasks))})
 popup<-function(payload=list()) {
  s<-state();kind<-payload$kind %||% 'Unavailable';date<-payload$date %||% as.character(controls$today());a<-num(payload$start %||% 9);b<-num(payload$end %||% (a+1));edit(payload)
  showModal(modalDialog(title=if(kind=='Task')'Place this work block'else'Protect time in your calendar',
   selectInput(ns('entry_kind'),'Activity type',c('Personal activity / unavailable'='Unavailable','Work meeting'='Meeting','Replace this day’s work availability'='Available','Task work block'='Task'),kind),
   conditionalPanel("input.entry_kind != 'Task'",ns=ns,textInput(ns('entry_title'),'Activity name',payload$title %||% '')),conditionalPanel("input.entry_kind == 'Task'",ns=ns,selectInput(ns('entry_task'),'Task',choices_id(active_tasks(s)),payload$task_id %||% '')),conditionalPanel("input.entry_kind == 'Meeting'",ns=ns,selectInput(ns('entry_job'),'Work role (meetings)',choices_id(s$jobs,'name'),payload$job_id %||% '')),dateInput(ns('entry_date'),'Date',date),fluidRow(column(6,textInput(ns('entry_start'),'Starts (HH:MM)',fmt_hour(a))),column(6,textInput(ns('entry_end'),'Ends (HH:MM)',fmt_hour(min(24,b))))),
   tags$p(class='hint','Task blocks are manual decisions and stay fixed until you move or release them. Removing a work block keeps the task in your backlog.'),
   footer=tagList(modalButton('Cancel'),if(nzchar(payload$replace_id %||% ''))actionButton(ns('entry_delete'),'Remove calendar entry',class='btn-danger-soft'),actionButton(ns('entry_save'),'Save to calendar',class='btn-primary'))))
 }
 observeEvent(input$add_activity,popup())
 observeEvent(input$edit_adjustment,{
  key<-input$adjustment %||% '';req(nzchar(key));id<-substring(key,3);s<-state()
  if(startsWith(key,'x:')){r<-s$exceptions[match(id,s$exceptions$id),];popup(as.list(cbind(r,replace_id=id)))}else{r<-s$locks[match(id,s$locks$id),];popup(c(as.list(r),list(kind='Task',replace_id=id)))}
 })
 observeEvent(input$canvas,{
  z<-input$canvas;s<-state()
  if(identical(z$action,'task')) {
   row<-s$locks[s$locks$task_id==z$task_id&s$locks$date==z$original_date&abs(s$locks$start-num(z$original_start))<1e-8,,drop=FALSE]
   z$kind<-'Task';z$replace_id<-if(nrow(row))row$id[1]else''
   if(identical(z$gesture,'drop'))tryCatch({commit(calendar_entry(s,z$date,num(z$start),num(z$end),'Task',task_id=z$task_id,replace_id=z$replace_id))},error=function(e){notify_error(e);session$sendCustomMessage('calendar-refresh',TRUE)})else popup(z)
  }else if(identical(z$action,'activity')){r<-s$exceptions[match(z$id,s$exceptions$id),,drop=FALSE];if(nrow(r)&&!is.na(r$id))popup(c(as.list(r),list(replace_id=r$id)))}else popup(z)
 })
 observeEvent(input$entry_save,{tryCatch({old<-edit();kind<-input$entry_kind;replace<-old$replace_id %||% '';if(nzchar(replace)&&!identical(kind=='Task',identical(old$kind,'Task')))stop('Remove the old entry before changing between a task and an activity.');s<-calendar_entry(state(),input$entry_date,calendar_clock(input$entry_start),calendar_clock(input$entry_end),kind,input$entry_title,input$entry_job %||% '',input$entry_task %||% '',replace);commit(s);removeModal()},error=notify_error)})
 observeEvent(input$entry_delete,{tryCatch({z<-edit();s<-state();if(identical(z$kind,'Task'))s$locks<-s$locks[s$locks$id!=z$replace_id,,drop=FALSE]else s$exceptions<-s$exceptions[s$exceptions$id!=z$replace_id,,drop=FALSE];commit(s);removeModal()},error=notify_error)})
 output$ics<-downloadHandler(filename=function()paste0('worthwhile-',controls$anchor(),'.ics'),content=function(file){ids<-input$export_tasks;if(!length(ids))ids<-state()$tasks$id;writeBin(charToRaw(paste0(calendar_ics(state(),planning(),controls$anchor(),controls$anchor()+6,input$export_mode %||% 'Full schedule',ids),'\r\n')),file)})
})

calendar_clock <- function(x)if(identical(x,'24:00'))24 else if(grepl(':',x,fixed=TRUE))clock_hours(x)else num(x)
