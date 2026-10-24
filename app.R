# Worthwhile — run from this directory with shiny::runApp('.')
stopifnot(requireNamespace('shiny',quietly=TRUE),requireNamespace('jsonlite',quietly=TRUE))
library(shiny)
for(file in c('design.R','model.R','opportunities.R','planning.R','demo.R','history.R','analytics.R','ui_helpers.R','setup_model.R','mod_setup.R','mod_tasks.R','mod_capacity.R','mod_jobs.R','mod_goals.R','mod_opportunities.R','mod_insights.R','mod_replan.R','mod_decide.R','supabase.R','mod_auth.R','data_ui.R'))source(file.path('R',file),local=TRUE)
planner_ui<-navbarPage(
 title=tags$span(class='brand',tags$span(class='brand-mark','W'),'Worthwhile'),
 id='page',windowTitle='Worthwhile — Make better decisions about what deserves your time.',
 selected='today',collapsible=TRUE,theme=paste0('style.css?v=',worthwhile_version),
 header=tagList(
  tags$head(tags$meta(name='theme-color',content=design_tokens$ink),tags$script(src=paste0('app.js?v=',worthwhile_version))),
  tags$a(class='skip-link',href='#today-content','Skip to main content'),
  tags$div(class='app-tagline','Make better decisions about what deserves your time.'),
  tags$div(class='planner-controls',
   dateInput('week','Planning week',Sys.Date(),width='180px'),tags$div(class='control-spacer'),
   textOutput('current_lens',inline=TRUE),actionButton('undo','Undo',icon=icon('arrow-rotate-left'),class='btn-quiet'),
   tags$details(class='planner-preferences',tags$summary('Plan preferences'),
    numericInput('buffer','Unplanned buffer (%)',15,min=0,max=70,step=5),
    numericInput('cap','Daily focus limit (h)',8,min=.25,max=16,step=.25),
    checkboxInput('protect','Keep protected personal time',TRUE),actionButton('shuffle','Reshuffle random'))),
  uiOutput('account_controls'),uiOutput('sample_notice')),
 tabPanel('Today',value='today',tags$main(id='today-content',tabindex='-1',tasksUI('tasks'))),
 tabPanel('Plan',value='plan',tags$main(id='plan-content',tabindex='-1',
  tags$div(class='experience-heading',tags$p(class='eyebrow','PLAN / A WEEK WITH ROOM TO THINK'),
   tags$h1('Give your commitments enough room.'),tags$p(class='lead','Protect life, place the work, and change the plan when the day changes.')),
  tabsetPanel(id='plan_section',type='pills',
   tabPanel('Calendar',value='calendar',capacityUI('capacity')),
   tabPanel('Replan a day',value='replan',replanUI('replan')),
   tabPanel('Work roles',value='roles',jobsUI('jobs')),
   tabPanel('Goals',value='goals',goalsUI('goals')),
   tabPanel('Guided setup',value='setup',setupUI('guide')),
   tabPanel('Data & preferences',value='data',dataUI())))),
 tabPanel('Decide',value='decide',tags$main(id='decide-content',tabindex='-1',decideUI('decide'))),
 tabPanel('Reflect',value='reflect',tags$main(id='reflect-content',tabindex='-1',insightsUI('insights'))),
 footer=credit_footer()
)
plannerServer <- function(input,output,session,store=NULL,loaded_state=NULL) {
  state_file<-Sys.getenv('WORTHWHILE_STATE_FILE',unset=Sys.getenv('PORTFOLIO_STATE_FILE',unset=file.path('data','worthwhile-state.json')))
  legacy_file<-file.path('data','portfolio-state.json')
  if(!file.exists(state_file)&&nzchar(state_file)&&identical(state_file,file.path('data','worthwhile-state.json'))&&file.exists(legacy_file))state_file<-legacy_file
  cloud<-!is.null(store)
  if(cloud)state_file<-''
  saved_exists<-if(cloud)!is.null(loaded_state)else nzchar(state_file)&&file.exists(state_file)
  load_error<-NULL
  initial<-if(cloud)loaded_state else if(saved_exists)tryCatch(read_backup(state_file),error=function(e){load_error<<-conditionMessage(e);NULL})else NULL
  if(is.null(initial)){today<-as.Date(format(Sys.time(),tz='Europe/Prague',format='%Y-%m-%d'));initial<-if(cloud)blank_state(today)else demo_state(today);if(cloud){initial$settings$example_mode<-FALSE;initial$settings$setup_complete<-FALSE}}
  state<-reactiveVal(initial);history<-list()
  if(!is.null(load_error))showNotification(paste('Saved plan could not be read; the file was left unchanged.',load_error),type='error',duration=NULL)
  commit<-function(s,remember=TRUE) {
    current<-isolate(state())
    # Snapshot the visible planning preferences with ordinary edits. Explicit
    # preferences in an applied scenario/restore take precedence.
    visible<-list(plan_buffer=isolate(input$buffer %||% 15),plan_cap=isolate(input$cap %||% 8),plan_protect=isolate(input$protect %||% TRUE),plan_strategy=isolate(strategy()))
    for(key in names(visible))if(is.null(s$settings[[key]])||identical(s$settings[[key]],current$settings[[key]]))s$settings[[key]]<-visible[[key]]
    validate_state(s)
    s$revision<-current$revision+1L
    if(cloud)store$save(s,expected_revision=current$revision)else save_state(s,state_file,expected_revision=current$revision)
    if(remember)history <<- tail(c(history,list(isolate(state()))),20)
    state(s)
    showNotification(if(cloud)store$label else if(nzchar(state_file))'Saved on this computer.'else'Saved for this session.',duration=2)
    invisible(TRUE)
  }
  strategy<-reactiveVal(initial$settings$plan_strategy %||% 'Balanced');seed<-reactiveVal(1L)
  observeEvent(strategy(),updateSelectInput(session,'decide-strategy',selected=strategy()))
  controls<-list(strategy=strategy,set_strategy=function(x)strategy(x),seed=seed,
    today=reactive(as.Date(format(Sys.time(),tz=state()$settings$timezone,format='%Y-%m-%d'))),
    buffer=reactive(input$buffer %||% 15),cap=reactive(input$cap %||% 8),anchor=reactive(week_start(input$week %||% Sys.Date())),set_week=function(date)updateDateInput(session,'week',value=date),protect=reactive(isTRUE(input$protect)))
  sync_preferences<-function(s) {
    strategy(s$settings$plan_strategy %||% 'Balanced')
    updateSelectInput(session,'decide-strategy',selected=isolate(strategy()))
    updateNumericInput(session,'buffer',value=s$settings$plan_buffer %||% 15)
    updateNumericInput(session,'cap',value=s$settings$plan_cap %||% 8)
    updateCheckboxInput(session,'protect',value=s$settings$plan_protect %||% TRUE)
  }
  session$onFlushed(function()sync_preferences(initial),once=TRUE)
  observeEvent(list(input$buffer,input$cap,input$protect,strategy()),{
    req(is.finite(input$buffer),is.finite(input$cap),!is.null(input$protect))
    s<-isolate(state());before<-s$settings
    s$settings$plan_buffer<-clamp(input$buffer,0,70);s$settings$plan_cap<-clamp(input$cap,.25,16)
    s$settings$plan_protect<-isTRUE(input$protect);s$settings$plan_strategy<-isolate(strategy())
    if(!identical(before,s$settings))commit_safely(function(x)commit(x,remember=FALSE),s)
  },ignoreInit=TRUE)
  planning<-reactive({req(input$week,is.finite(input$buffer),is.finite(input$cap));plan_work(state(),strategy(),controls$anchor(),controls$protect(),clamp(input$buffer,0,70),clamp(input$cap,.25,16),seed(),controls$today())})
  output$sample_notice<-renderUI({if(!isTRUE(state()$settings$setup_complete)&&!isTRUE(state()$settings$example_mode))return(tags$div(class='sample-banner',tags$span('Welcome. Set your working rhythm and add your first work role.'),actionButton('open_setup','Set up my plan',class='btn-primary')));if(!identical(input$page,'setup') && isTRUE(state()$settings$example_mode))tags$div(class='sample-banner',tags$span('Scientist example · The pay and effort estimates are illustrative.'),actionButton('open_setup','Create my own plan',class='btn-quiet'))})
  observeEvent(input$open_setup,{updateNavbarPage(session,'page',selected='plan');updateTabsetPanel(session,'plan_section',selected='setup')})
  output$current_lens<-renderText(paste('Lens:',strategy()))
  observeEvent(input$calendar_date,updateDateInput(session,'week',value=input$calendar_date))
  observeEvent(input$calendar_pick,{updateNavbarPage(session,'page',selected='today');updateSelectInput(session,'tasks-selected',selected=input$calendar_pick);session$sendCustomMessage('open-task-desk',TRUE)})
  observeEvent(input$shuffle,{seed(seed()+1L);strategy('Random')})
  observeEvent(input$undo,{tryCatch({if(!length(history)){showNotification('Nothing to undo.');return()};s<-history[[length(history)]];s$revision<-isolate(state())$revision+1L;if(cloud)store$save(s,expected_revision=isolate(state())$revision)else save_state(s,state_file,expected_revision=isolate(state())$revision);history<<-head(history,-1);state(s);sync_preferences(s);showNotification('Previous state restored.',duration=2)},error=notify_error)})
  guide<-setupServer('guide',state,commit,function(){strategy('Balanced');updateNumericInput(session,'buffer',value=15);updateNumericInput(session,'cap',value=8);updateCheckboxInput(session,'protect',value=TRUE);updateNavbarPage(session,'page',selected='today')})
  task_manager<-tasksServer('tasks',state,commit,controls,planning)
  session$onFlushed(function(){if(saved_exists&&is.null(load_error)){updateNavbarPage(session,'page',selected='today');isolate(task_manager$review())}},once=TRUE)
  capacityServer('capacity',state,commit,controls,planning)
  jobsServer('jobs',state,commit,controls)
  goal_manager<-goalsServer('goals',state,commit)
  open_goal<-function(gid){updateNavbarPage(session,'page',selected='plan');updateTabsetPanel(session,'plan_section',selected='goals');goal_manager$edit(gid)}
  decisions<-decideServer('decide',state,commit,controls,planning,open_goal)
  replanner<-replanServer('replan',state,commit,controls,planning)
  observeEvent(input[['tasks-replan']],{updateNavbarPage(session,'page',selected='plan');updateTabsetPanel(session,'plan_section',selected='replan')})
  insightsServer('insights',state,commit)
  observe({s<-state();updateTextInput(session,'currency',value=s$settings$currency);updateSelectInput(session,'timezone',selected=s$settings$timezone);updateNumericInput(session,'fulltime',value=s$settings$fulltime)})
  observeEvent(input$settings,{s<-state();s$settings$currency<-input$currency;s$settings$timezone<-input$timezone;s$settings$fulltime<-input$fulltime;commit_safely(commit,s)})
  output$backup<-downloadHandler(filename=function()paste0('worthwhile-backup-',controls$today(),'.json'),content=function(file)jsonlite::write_json(state(),file,pretty=TRUE,auto_unbox=TRUE,dataframe='columns'))
  pending_restore<-reactiveVal(NULL)
  observeEvent(input$restore,{req(input$restore$datapath);if(input$restore$size>5e6){showNotification('Backup exceeds 5 MB.',type='error');return()};tryCatch({pending_restore(read_backup(input$restore$datapath));showModal(modalDialog(title='Restore this backup?',tags$p('This replaces the current dataset. A previous-good local save and session Undo are retained.'),footer=tagList(modalButton('Cancel'),actionButton('restore_confirm','Restore backup',class='btn-primary'))))},error=notify_error)})
  observeEvent(input$restore_confirm,{tryCatch({s<-pending_restore();req(s);commit(s);sync_preferences(s);pending_restore(NULL);removeModal();updateNavbarPage(session,'page',selected='today');task_manager$review()},error=notify_error)})
  observeEvent(input$demo,showModal(modalDialog(title='Load the scientist example?',tags$p('This replaces the current plan with illustrative work and pay figures. Your previous save and Undo remain available.'),footer=tagList(modalButton('Cancel'),actionButton('demo_confirm','Load example',class='btn-primary')))))
  observeEvent(input$demo_confirm,{commit_safely(commit,demo_state(controls$today()));removeModal()})
  observeEvent(input$empty,showModal(modalDialog(title='Start a blank plan?',tags$p('This replaces the whole dataset, including report history. For routine removal, use task or work-role deletion instead.'),footer=tagList(modalButton('Cancel'),actionButton('empty_confirm','Start blank plan',class='btn-primary')))))
  observeEvent(input$empty_confirm,{
    s<-demo_state(controls$today());s$settings$example_mode<-FALSE;s$settings$setup_complete<-TRUE;s$jobs<-s$jobs[1,,drop=FALSE];s$jobs$name<-'My work role';s$jobs$notes<-'';s$jobs$fte<-0;s$jobs$target<-20;s$jobs$weekly_income<-0
    for(n in c('tasks','goals','allocations','logs','opportunities'))s[[n]]<-s[[n]][FALSE,,drop=FALSE]
    s$blocks<-s$blocks[s$blocks$kind=='Essential',,drop=FALSE];commit_safely(commit,s);removeModal()
  })

}
app_mode<-worthwhile_mode()
ui<-if(app_mode=='local')planner_ui else bootstrapPage(theme=paste0('style.css?v=',worthwhile_version),tags$head(tags$meta(name='referrer',content='same-origin'),tags$script(src=paste0('auth.js?v=',worthwhile_version))),uiOutput('cloud_page'))
server<-if(app_mode=='local')plannerServer else cloudServer
shinyApp(ui,server)
