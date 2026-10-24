capacityUI <- function(id) {
  ns<-NS(id)
  tagList(tags$h2('A calendar with breathing room.'),tags$p(class='lead','Available time is only a starting point. Life, meetings and recovery get space first.'),
    section('Your calendar',tags$div(class='calendar-toolbar',tags$div(actionButton(ns('previous'),'‹',title='Previous week'),actionButton(ns('today'),'Today'),actionButton(ns('following'),'›',title='Next week')),uiOutput(ns('heading')),radioButtons(ns('view'),NULL,c('Week','Day','Month'),selected='Week',inline=TRUE)),conditionalPanel("input.view == 'Day'",ns=ns,selectInput(ns('day'),'Day',NULL)),checkboxInput(ns('all_day'),'Show full 24-hour day',FALSE),uiOutput(ns('calendar_legend')),uiOutput(ns('warnings')),uiOutput(ns('week_calendar')),accordion('Simple daily agenda',uiOutput(ns('agenda'))),accordion('Classic calendar plot',plotOutput(ns('calendar'),height='560px')),downloadButton(ns('csv'),'Download plan CSV'),tags$p(class='hint','Quarter-hour slots, recurring local wall-clock times. A draft plan, not a time log. Dates have day-level deadlines. Past time is excluded.')),
    accordion('Fine-tune availability and recurring commitments',fluidRow(column(6,section('When you can work',tableOutput(ns('availability')),selectInput(ns('avail_selected'),'Window to edit / remove',NULL),actionButton(ns('avail_load'),'Load window'),actionButton(ns('avail_new'),'New window'),selectInput(ns('avail_day'),'Weekday',setNames(1:7,c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))),numericInput(ns('avail_start'),'Start hour (e.g. 21.5)',8,min=0,max=23.75,step=.25),numericInput(ns('avail_end'),'End hour',17,min=0,max=24,step=.25),actionButton(ns('avail_save'),'Save window',class='btn-primary'),actionButton(ns('avail_remove'),'Remove window'),tags$p(class='hint','End before start crosses midnight. Overlaps count once. Use multiple windows for split shifts. Sleep stays blocked even during night availability.'))),
    column(6,section('Commitments and meetings',selectInput(ns('block_selected'),'Commitment to edit / remove',NULL),actionButton(ns('block_load'),'Load commitment'),actionButton(ns('block_new'),'New commitment'),textInput(ns('block_title'),'Name'),checkboxGroupInput(ns('block_days'),'Days',setNames(1:7,c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),inline=TRUE),selectInput(ns('block_kind'),'Type',c('Essential','Protected','Meeting')),selectInput(ns('block_job'),'Job (for meetings)',NULL),numericInput(ns('block_start'),'Start hour',12,min=0,max=23.75,step=.25),numericInput(ns('block_end'),'End hour',13,min=0,max=24,step=.25),actionButton(ns('block_save'),'Save commitment',class='btn-primary'),actionButton(ns('block_remove'),'Remove selected commitment'),tags$p(class='hint','Essential and meeting blocks always stay. Protected blocks stay unless you explicitly release them using the planner controls.'))))),
    section('Unplanned effort and deadline pressure',tableOutput(ns('unplanned')))
  )
}
capacityServer <- function(id,state,commit,controls,planning) moduleServer(id,function(input,output,session) {
  edit_avail<-reactiveVal('');edit_block<-reactiveVal('')
  observe({s<-state();updateSelectInput(session,'avail_selected',choices=if(!nrow(s$availability))character()else setNames(s$availability$id,paste(c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')[s$availability$day],fmt_hour(s$availability$start),'–',fmt_hour(s$availability$end))))
    updateSelectInput(session,'block_selected',choices=if(!nrow(s$blocks))character()else setNames(s$blocks$id,paste(c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')[s$blocks$day],s$blocks$title,fmt_hour(s$blocks$start))))
    updateSelectInput(session,'block_job',choices=choices_id(s$jobs,'name'))})
  output$availability<-renderTable({s<-state();data.frame(Day=c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')[s$availability$day],From=fmt_hour(s$availability$start),To=fmt_hour(s$availability$end))},striped=TRUE)
  observeEvent(input$avail_load,{s<-state();r<-s$availability[match(input$avail_selected,s$availability$id),];req(nrow(r)==1);edit_avail(r$id);updateSelectInput(session,'avail_day',selected=r$day);updateNumericInput(session,'avail_start',value=r$start);updateNumericInput(session,'avail_end',value=r$end)})
  observeEvent(input$avail_new,edit_avail(''))
  observeEvent(input$avail_save,{s<-state();aid<-if(nzchar(edit_avail()))edit_avail() else next_id('a',record_ids(s,'availability'));r<-data.frame(id=aid,day=num(input$avail_day),start=input$avail_start,end=input$avail_end,stringsAsFactors=FALSE)
    if(aid%in%s$availability$id)s$availability[match(aid,s$availability$id),]<-r else s$availability<-rbind(s$availability,r)
    commit_safely(commit,s)})
  observeEvent(input$avail_remove,{s<-state();s$availability<-s$availability[s$availability$id!=input$avail_selected,,drop=FALSE];edit_avail('');commit_safely(commit,s)})
  observeEvent(input$block_load,{s<-state();r<-s$blocks[match(input$block_selected,s$blocks$id),];req(nrow(r)==1);edit_block(r$id);updateTextInput(session,'block_title',value=r$title);updateCheckboxGroupInput(session,'block_days',selected=r$day);updateSelectInput(session,'block_kind',selected=r$kind);updateSelectInput(session,'block_job',selected=r$job_id);updateNumericInput(session,'block_start',value=r$start);updateNumericInput(session,'block_end',value=r$end)})
  observeEvent(input$block_new,{edit_block('');updateTextInput(session,'block_title',value='');updateCheckboxGroupInput(session,'block_days',selected=character())})
  observeEvent(input$block_save,{
    if(!nzchar(trimws(input$block_title))||!length(input$block_days)) {showNotification('Name the commitment and choose days.',type='error');return()}
    s<-state();if(nzchar(edit_block()))s$blocks<-s$blocks[s$blocks$id!=edit_block(),,drop=FALSE]
    for(d in input$block_days)s$blocks<-rbind(s$blocks,data.frame(id=next_id('b',record_ids(s,'blocks')),day=num(d),title=trimws(input$block_title),kind=input$block_kind,start=input$block_start,end=input$block_end,job_id=if(input$block_kind=='Meeting')input$block_job else '',stringsAsFactors=FALSE))
    tryCatch({commit(s);edit_block('')},error=notify_error)
  })
  observeEvent(input$block_remove,{s<-state();s$blocks<-s$blocks[s$blocks$id!=input$block_selected,,drop=FALSE];edit_block('');commit_safely(commit,s)})
  output$calendar_legend<-renderUI(tagList(job_legend(state()),tags$p(class='calendar-key','Soft job colours: routine work · Red: critical or fixed deadline within 2 days · Amber: overdue, due soon or repeatedly urged · Blue: meetings · Lilac: personal time')))
  observeEvent(input$previous,controls$set_week(controls$anchor()-7))
  observeEvent(input$following,controls$set_week(controls$anchor()+7))
  observeEvent(input$today,controls$set_week(controls$today()))
  observe({dates<-controls$anchor()+0:6;updateSelectInput(session,'day',choices=setNames(1:7,format(dates,'%a · %d %b')),selected=if(controls$today()%in%dates)as.integer(controls$today()-controls$anchor())+1 else 1)})
  output$heading<-renderUI(tags$div(class='calendar-title',tags$strong(format(controls$anchor()+3,'%B %Y')),tags$small(paste(format(controls$anchor(),'%d %b'),'–',format(controls$anchor()+6,'%d %b')))))
  output$week_calendar<-renderUI({s<-state();p<-planning();if(identical(input$view,'Month'))return(month_calendar(s,p,controls$anchor()));full<-isTRUE(input$all_day)||any(p$plan$start<6|p$plan$end>23);days<-if(identical(input$view,'Day'))as.integer(input$day %||% 1)else 1:7;calendar_html(s,p,controls$anchor(),if(full)0 else 6,if(full)24 else 23,days)})
  output$agenda<-renderUI({p<-planning()$plan;if(!nrow(p))return(tags$p(class='empty-hint','No focus blocks are planned. Add a task or check your availability.'));tagList(lapply(unique(p$date),function(date){z<-p[p$date==date,,drop=FALSE];tags$div(class='agenda-day',tags$strong(format(as.Date(date),'%A · %d %B')),lapply(seq_len(nrow(z)),function(i)tags$div(class='agenda-row',tags$time(paste(fmt_hour(z$start[i]),'–',fmt_hour(z$end[i]))),tags$span(style=paste0('border-color:',job_colour(state(),z$job_id[i])),z$title[i]))))}))})
  output$calendar<-renderPlot({p<-planning();g<-p$grid
    par(mar=c(3,4,3,1),bg=design_tokens$surface,family='sans')
    plot(NA,xlim=c(.5,7.5),ylim=c(24,0),xlab='',ylab='',axes=FALSE,xaxs='i',yaxs='i')
    axis(2,at=seq(0,24,3),labels=sprintf('%02d:00',seq(0,24,3)),las=1,cex.axis=.8)
    axis(3,at=1:7,labels=paste(c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'),format(controls$anchor()+0:6,'%d %b'),sep='\n'),tick=FALSE,cex.axis=.85)
    abline(h=seq(0,24,3),col=design_tokens$line);abline(v=seq(.5,7.5,1),col=design_tokens$line)
    for(i in seq_len(nrow(g))) {
      col<-if(g$blocked[i]) switch(g$kind[i],Meeting=design_tokens$info,Protected=design_tokens$lavender,Essential=design_tokens$line,design_tokens$line) else if(g$reserved[i]) design_tokens$clay else if(g$available[i])design_tokens$background else design_tokens$surface
      rect(g$day[i]-.46,g$start[i],g$day[i]+.46,g$start[i]+.25,col=col,border=NA)
    }
    if(nrow(p$plan))for(i in seq_len(nrow(p$plan))) {r<-p$plan[i,];rect(r$day-.43,r$start,r$day+.43,r$end,col=design_tokens$lavender,border=design_tokens$surface);if(r$hours>=.75)text(r$day,(r$start+r$end)/2,paste(strwrap(r$title,15),collapse='\n'),cex=.6,col=design_tokens$ink)}
  },res=110)
  output$warnings<-renderUI({p<-planning();s<-state();t<-p$unscheduled
    due_soon<-if(nrow(t)) t$hard & effective_due(t)<=controls$anchor()+6 & t$unplanned>0 else logical()
    conflicts<-calendar_conflicts(s,controls$protect())
    total_target<-sum(job_targets(s,s$settings$fulltime));capacity<-sum(p$grid$usable)/4+sum(job_meeting_hours(s))
    tagList(if(length(p$conflicts))tags$div(class='notice warning',tags$strong('Manual work blocks need attention.'),tags$ul(lapply(p$conflicts,tags$li))),if(nrow(conflicts))tags$div(class='notice warning',tags$strong('Commitment conflicts need a decision.'),tags$ul(lapply(seq_len(nrow(conflicts)),function(i)tags$li(paste(conflicts$first[i],'overlaps',conflicts$second[i],'by',conflicts$overlap_h[i],'h'))))),
      if(total_target>capacity)tags$div(class='notice warning',paste0('Contract / personal targets need ',total_target,' h including meetings; this calendar supports about ',round(capacity,1),' h. Renegotiate scope or targets before adding work.')),
      if(any(due_soon))tags$div(class='notice warning',paste(sum(due_soon),'fixed-deadline tasks cannot fit or are already overdue. Review them below.')),
      if(!controls$protect())tags$div(class='notice warning','Protected personal events have been released. Essentials remain blocked.'))
  })
  output$unplanned<-renderTable({t<-planning()$unscheduled;if(!nrow(t))return(NULL);t<-t[t$unplanned>0,,drop=FALSE];data.frame(Task=t$title,Unplanned_h=t$unplanned,Deadline=as.character(effective_due(t)),Fixed=t$hard,Waiting=t$blocked,check.names=FALSE)},striped=TRUE)
  output$csv<-downloadHandler(filename=function()paste0('week-',controls$anchor(),'.csv'),content=function(file)write.csv(planning()$plan,file,row.names=FALSE,fileEncoding='UTF-8'))
})
