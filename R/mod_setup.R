time_choices <- function() setNames(seq(0,23.75,.25),fmt_hour(seq(0,23.75,.25)))
setupUI <- function(id) {
  ns<-NS(id)
  tagList(tags$div(class='setup-shell',
    tags$div(class='setup-intro',tags$span(class='eyebrow','A LITTLE SETUP. A LOT MORE CLARITY.'),tags$h2('Let’s make space for your week.'),tags$p('Five small steps. Start with the essentials — you can fine-tune everything later.'),
      tags$div(class='welcome-options',actionButton(ns('demo'),'Explore the scientist example',icon=icon('flask'),class='btn btn-quiet'),tags$span('or build your own plan below'))),
    uiOutput(ns('steps')),
    tags$div(class='setup-card',tabsetPanel(id=ns('panels'),type='hidden',selected='1',
      tabPanel('Your rhythm',value='1',tags$div(class='step-heading',tags$span(class='step-number','01'),tags$div(tags$h2('When does work fit your life?'),tags$p('Pick a starting rhythm. Your sleep stays protected.'))),
        radioButtons(ns('rhythm'),NULL,c('Daytime'='Daytime','Evenings'='Evenings','Night shifts'='Night shifts','My own rhythm'='Custom'),inline=TRUE),
        checkboxGroupInput(ns('days'),'Days you are available',setNames(1:7,c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),selected=1:5,inline=TRUE),
        fluidRow(column(6,selectInput(ns('start'),'I can start at',time_choices(),9)),column(6,selectInput(ns('end'),'I stop at',c(time_choices(),'24:00'=24),17))),
        tags$div(class='soft-box',tags$div(class='soft-box-title',icon('moon'),'Protect my sleep'),fluidRow(column(6,selectInput(ns('sleep_start'),'Usually asleep by',time_choices(),23)),column(6,selectInput(ns('sleep_end'),'Usually awake at',time_choices(),7))),tags$p(class='hint','For overnight shifts, choose your daytime sleep hours. End before start means the next day.')),
        tags$details(class='advanced',tags$summary('Currency & timezone'),textInput(ns('currency'),'Currency','EUR'),selectInput(ns('timezone'),'Timezone',OlsonNames(),'Europe/Prague'))),
      tabPanel('Your jobs',value='2',tags$div(class='step-heading',tags$span(class='step-number','02'),tags$div(tags$h2('What kinds of work do you do?'),tags$p('Add one job to start. You can add more now or later.'))),
        uiOutput(ns('job_list')),
        tags$div(class='compact-form',textInput(ns('job_name'),'Job name',placeholder='e.g. University, translation, my business'),
          fluidRow(column(6,selectInput(ns('job_pay'),'How are you paid?',c('Fixed salary'='Salary','By the hour'='Hourly','By a page, room or item'='Piece'))),column(6,numericInput(ns('job_hours'),'Expected hours each week',10,min=.25,step=.25))),
          conditionalPanel("input.job_pay == 'Salary'",ns=ns,fluidRow(column(6,numericInput(ns('job_amount'),'Gross salary',0,min=0)),column(6,selectInput(ns('job_period'),'Paid each',c('Month'='Monthly','Week'='Weekly'))))),
          conditionalPanel("input.job_pay != 'Salary'",ns=ns,numericInput(ns('job_rate'),'Rate per hour / item',0,min=0)),
          conditionalPanel("input.job_pay == 'Piece'",ns=ns,textInput(ns('job_unit'),'What is one paid item?','page')),
          checkboxInput(ns('job_self'),'I am self-employed in this job',FALSE),
          tags$details(class='advanced',tags$summary('Choose an icon or add contract details'),selectInput(ns('job_icon'),'Icon',setNames(job_icons,c('University','Research','Books','Cleaning','Development','Briefcase','Creative','Care','Nature','Tools','Music','Global')),'briefcase'),numericInput(ns('job_fte'),'FTE, if your contract specifies one',0,min=0,max=1,step=.05),numericInput(ns('job_expense'),'Business expenses (%)',0,min=0,max=100)),
          actionButton(ns('add_job'),'Add this job',icon=icon('plus'),class='btn-primary'))),
      tabPanel('Real life',value='3',tags$div(class='step-heading',tags$span(class='step-number','03'),tags$div(tags$h2('Keep room for being human.'),tags$p('Use a few sensible defaults, then add your regular meetings or personal time.'))),
        tags$div(class='default-grid',tags$div(class='default-tile',icon('utensils'),checkboxInput(ns('meals'),'Keep three 30-minute meal breaks',TRUE)),tags$div(class='default-tile',icon('person-walking'),checkboxInput(ns('breaks'),'Keep bathroom & movement breaks',TRUE))),
        uiOutput(ns('event_list')),
        tags$details(class='advanced event-details',tags$summary('Add a meeting, hobby or personal commitment'),textInput(ns('event_title'),'What is it?',placeholder='e.g. Lab meeting, climbing, dinner with friends'),selectInput(ns('event_kind'),'Type',c('Job meeting'='Meeting','Personal time I want to keep'='Protected','Essential'='Essential')),conditionalPanel("input.event_kind == 'Meeting'",ns=ns,selectInput(ns('event_job'),'Which job?',NULL)),checkboxGroupInput(ns('event_days'),'Repeat on',setNames(1:7,c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),inline=TRUE),fluidRow(column(6,selectInput(ns('event_start'),'Starts at',time_choices(),9)),column(6,selectInput(ns('event_end'),'Ends at',c(time_choices(),'24:00'=24),10))),actionButton(ns('add_event'),'Add commitment',class='btn-primary')),
        tags$p(class='hint','No regular meetings yet? Continue with the defaults. A 15% buffer and an 8-hour daily focus limit leave room for surprises.')),
      tabPanel('First tasks',value='4',tags$div(class='step-heading',tags$span(class='step-number','04'),tags$div(tags$h2('Start small. What needs doing?'),tags$p('Add just one to three real tasks. You do not need your whole backlog to get started.'))),
        uiOutput(ns('task_list')),textInput(ns('task_title'),'A concrete next action',placeholder='e.g. Draft the paper introduction'),
        fluidRow(column(6,selectInput(ns('task_job'),'For which job?',NULL)),column(6,numericInput(ns('task_hours'),'How many hours will it take?',1,min=.25,step=.25))),
        fluidRow(column(6,dateInput(ns('task_due'),'Aim to finish by',Sys.Date()+7)),column(6,selectInput(ns('task_importance'),'How important is it?',c('Normal','Important','Critical','Personally meaningful')))),
        checkboxInput(ns('task_hard'),'This deadline cannot move',FALSE),
        conditionalPanel("output.task_piece",ns=ns,numericInput(ns('task_units'),'How many paid pages / rooms / items?',1,min=0)),
        actionButton(ns('add_task'),'Add this task',icon=icon('plus'),class='btn-primary'),tags$p(class='hint','Goals, dependencies, career scores and detailed costs can wait. They are available when you need them.')),
      tabPanel('Your plan',value='5',tags$div(class='step-heading',tags$span(class='step-number','05'),tags$div(tags$h2('Here is your starting point.'),tags$p('A realistic first plan — ready to adjust as you learn.'))),uiOutput(ns('review')),
        tags$div(class='soft-box',tags$strong('A simple daily rhythm'),tags$ol(tags$li('Check your next three moves.'),tags$li('Work on one task, then log the time or mark it finished.'),tags$li('Review your week when a deadline or commitment changes.'))),
        tags$div(class='save-reminder',icon('download'),tags$span('Your plan saves automatically on this computer. Keep a downloadable backup too.')))
    ),uiOutput(ns('message')),tags$div(class='wizard-footer',actionButton(ns('back'),'Back',icon=icon('arrow-left'),class='btn-quiet'),tags$span(textOutput(ns('position'),inline=TRUE)),actionButton(ns('next_step'),'Continue',icon=icon('arrow-right'),class='btn-primary'))),
    tags$p(class='setup-footnote','You can revisit this guide any time. Your current plan changes only when you choose “Create my plan”.')))
}
setupServer <- function(id,state,commit,on_finish) moduleServer(id,function(input,output,session) {
  step<-reactiveVal(1L);furthest<-reactiveVal(1L);message<-reactiveVal('')
  draft_jobs<-reactiveVal(blank_state()$jobs);draft_tasks<-reactiveVal(blank_state()$tasks);draft_events<-reactiveVal(blank_state()$blocks)
  titles<-c('Your rhythm','Your jobs','Real life','First tasks','Your plan')
  cfg<-reactive(list(today=as.Date(format(Sys.time(),tz=input$timezone %||% 'Europe/Prague',format='%Y-%m-%d')),currency=input$currency %||% 'EUR',timezone=input$timezone %||% 'Europe/Prague',days=num(input$days),start=num(input$start),end=num(input$end),sleep_start=num(input$sleep_start),sleep_end=num(input$sleep_end),meals=isTRUE(input$meals),breaks=isTRUE(input$breaks)))
  build<-function()setup_state(cfg(),draft_jobs(),draft_tasks(),draft_events())
  output$steps<-renderUI(tags$ol(class='setup-progress',lapply(1:5,function(i)tags$li(class=if(i==step())'current' else if(i<step())'complete' else '',select_event(session$ns,'jump',paste0('s',i),tagList(tags$span(class='step-dot',if(i<step())icon('check')else i),tags$span(titles[i])),'step-link')))))
  output$position<-renderText(paste('Step',step(),'of 5'))
  output$message<-renderUI(if(nzchar(message()))tags$div(class='notice warning',role='alert',message()))
  observe({updateActionButton(session,'next_step',label=if(step()==5)'Create my plan' else 'Continue',icon=icon(if(step()==5)'check' else 'arrow-right'));updateTabsetPanel(session,'panels',selected=as.character(step()))})
  observeEvent(input$rhythm,{
    p<-switch(input$rhythm,Daytime=c(9,17,23,7),Evenings=c(18,22,23,7),'Night shifts'=c(22,6,7,15),NULL)
    if(!is.null(p)){for(i in seq_along(p))updateSelectInput(session,c('start','end','sleep_start','sleep_end')[i],selected=p[i])}
  })
  check_step<-function(n) {
    if(n==1){c<-cfg();if(!length(c$days))stop('Choose at least one workday.');for(pair in list(c(c$start,c$end),c(c$sleep_start,c$sleep_end)))if(length(pair)!=2||any(!is.finite(pair))||pair[1]==pair[2])stop('Choose different start and end times.');if(!c$timezone%in%OlsonNames()||!nzchar(c$currency))stop('Choose a timezone and currency.')}
    if(n==2 && nzchar(trimws(input$job_name %||% '')))stop('Add the job you typed, or clear its name before continuing.')
    if(n==3 && nzchar(trimws(input$event_title %||% '')))stop('Add the commitment you typed, or clear its name before continuing.')
    if(n==2 && !nrow(draft_jobs()))stop('Add at least one job with the “Add this job” button.')
    if(n==4 && nzchar(trimws(input$task_title %||% '')))stop('Add the task you typed, or clear its title to continue without it.')
  }
  go<-function(n){message('');step(n);furthest(max(furthest(),n))}
  observeEvent(input$back,{go(max(1L,step()-1L))})
  observeEvent(input$jump,{n<-num(sub('s','',input$jump));if(is.finite(n)&&n>=1&&n<=furthest())go(as.integer(n))})
  observeEvent(input$next_step,{
    tryCatch({check_step(step());if(step()<5)go(step()+1L)else{commit(build());on_finish()}},error=function(e)message(conditionMessage(e)))
  })
  observeEvent(input$demo,{s<-demo_state(cfg()$today);s$settings$setup_complete<-TRUE;commit(s);on_finish()})
  observeEvent(input$add_job,{
    tryCatch({
      j<-draft_jobs();if(!nzchar(trimws(input$job_name)))stop('Give this job a name first.')
      row<-simple_job(next_id('j',j$id),input$job_name,input$job_pay,input$job_hours,input$job_amount %||% 0,input$job_period %||% 'Monthly',input$job_rate %||% 0,input$job_unit %||% 'unit',isTRUE(input$job_self),input$job_icon %||% 'briefcase',input$job_fte %||% 0,input$job_expense %||% 0)
      test<-blank_state();test$jobs<-rbind(j,row);validate_state(test);draft_jobs(test$jobs);updateTextInput(session,'job_name',value='');message('')
    },error=function(e)message(conditionMessage(e)))
  })
  observe({j<-draft_jobs();updateSelectInput(session,'event_job',choices=choices_id(j,'name'),selected=isolate(input$event_job));updateSelectInput(session,'task_job',choices=choices_id(j,'name'),selected=isolate(input$task_job))})
  output$job_list<-renderUI({j<-draft_jobs();if(!nrow(j))return(tags$p(class='empty-hint','Your jobs will appear here. Start with your main one.'));tagList(lapply(seq_len(nrow(j)),function(i)tags$div(class='draft-row',tags$span(class='draft-icon',icon(j$icon[i])),tags$div(tags$strong(j$name[i]),tags$small(paste(j$pay_type[i],'·',j$target[i],'h / week'))),select_event(session$ns,'remove_job',j$id[i],icon('xmark'),'btn btn-icon'))))})
  observeEvent(input$remove_job,{jid<-input$remove_job;draft_jobs(draft_jobs()[draft_jobs()$id!=jid,,drop=FALSE]);draft_tasks(draft_tasks()[draft_tasks()$job_id!=jid,,drop=FALSE]);draft_events(draft_events()[draft_events()$job_id!=jid,,drop=FALSE])})
  observeEvent(input$add_event,{
    tryCatch({if(!nzchar(trimws(input$event_title))||!length(input$event_days))stop('Name the commitment and choose its days.');ev<-draft_events()
      for(d in input$event_days)ev<-rbind(ev,data.frame(id=next_id('b',ev$id),day=num(d),title=trimws(input$event_title),kind=input$event_kind,start=num(input$event_start),end=num(input$event_end),job_id=if(input$event_kind=='Meeting')input$event_job else '',stringsAsFactors=FALSE))
      test<-blank_state();test$jobs<-draft_jobs();test$blocks<-ev;validate_state(test);draft_events(ev);updateTextInput(session,'event_title',value='');message('')
    },error=function(e)message(conditionMessage(e)))
  })
  output$event_list<-renderUI({e<-draft_events();tagList(lapply(seq_len(nrow(e)),function(i)tags$div(class='draft-row',tags$span(class='draft-icon',icon(if(e$kind[i]=='Meeting')'users' else 'heart')),tags$div(tags$strong(e$title[i]),tags$small(paste(c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')[e$day[i]],fmt_hour(e$start[i]),'–',fmt_hour(e$end[i])))),select_event(session$ns,'remove_event',e$id[i],icon('xmark'),'btn btn-icon'))))})
  observeEvent(input$remove_event,draft_events(draft_events()[draft_events()$id!=input$remove_event,,drop=FALSE]))
  output$task_piece<-reactive({j<-draft_jobs();isTRUE(j$pay_type[match(input$task_job,j$id)]=='Piece')});outputOptions(output,'task_piece',suspendWhenHidden=FALSE)
  observeEvent(input$add_task,{
    tryCatch({if(!nzchar(trimws(input$task_title)))stop('Describe one action to work on.');t<-draft_tasks();j<-draft_jobs();row<-simple_task(next_id('t',t$id),input$task_job,input$task_title,input$task_hours,input$task_due,isTRUE(input$task_hard),input$task_importance,if(isTRUE(j$pay_type[match(input$task_job,j$id)]=='Piece'))input$task_units %||% 0 else 0)
      test<-blank_state();test$jobs<-j;test$tasks<-rbind(t,row);validate_state(test);draft_tasks(test$tasks);updateTextInput(session,'task_title',value='');message('')
    },error=function(e)message(conditionMessage(e)))
  })
  output$task_list<-renderUI({t<-draft_tasks();tagList(lapply(seq_len(nrow(t)),function(i)tags$div(class='draft-row',tags$span(class='draft-icon',icon('check')),tags$div(tags$strong(t$title[i]),tags$small(paste(t$remaining[i],'h ·',t$due[i]))),select_event(session$ns,'remove_task',t$id[i],icon('xmark'),'btn btn-icon'))))})
  observeEvent(input$remove_task,draft_tasks(draft_tasks()[draft_tasks()$id!=input$remove_task,,drop=FALSE]))
  output$review<-renderUI({if(step()!=5)return(NULL)
    tryCatch({s<-build();g<-capacity_grid(s,TRUE,15,8);hours<-sum(g$usable)/4;target<-sum(job_targets(s));meet<-sum(job_meeting_hours(s));conflicts<-calendar_conflicts(s)
      tagList(tags$div(class='review-stats',metric('Focus capacity',paste0(hours,' h'),'After your protected time & buffer'),metric('Work to aim for',paste0(target,' h'),'Across your jobs, including meetings'),metric('A small start',paste(nrow(s$tasks),'tasks'),paste(nrow(s$jobs),'jobs added'))),
        if(target>hours+meet)tags$div(class='notice warning',paste0('Your job targets exceed this week’s capacity by about ',round(target-hours-meet,1),' hours. Start with the most important commitments and review scope.'))else tags$div(class='notice success','Your job targets fit inside this weekly capacity. Your first plan starts with Balanced priorities.'),
        if(nrow(conflicts))tags$div(class='notice warning',paste(nrow(conflicts),'meeting conflicts overlap protected time. Resolve these in Plan → Calendar.')),
        if(!nrow(s$tasks))tags$p(class='hint','No tasks yet — that is fine. Use “Add a task” on your dashboard when you are ready.'))
    },error=function(e)tags$div(class='notice warning',conditionMessage(e)))
  })
  list(step=step,jobs=draft_jobs,tasks=draft_tasks,events=draft_events,build=build)
})
