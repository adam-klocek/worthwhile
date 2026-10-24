goalsUI <- function(id) {
  ns<-NS(id)
  tagList(tags$h2('Make the longer game visible.'),tags$p(class='lead','A paper, a semester or a career move: connect everyday actions to a clear outcome.'),
    fluidRow(column(7,uiOutput(ns('cards'))),column(5,section('Goal editor',selectInput(ns('selected'),'Existing goal',NULL),actionButton(ns('load'),'Load selected'),actionButton(ns('new'),'New goal'),textInput(ns('title'),'Goal'),selectInput(ns('job'),'Job',NULL),numericInput(ns('target'),'Target units',1,min=.01),numericInput(ns('baseline'),'Units completed before tracking began',0,min=0),textInput(ns('unit'),'Unit','deliverables'),dateInput(ns('start'),'Period starts',Sys.Date()),dateInput(ns('end'),'Period ends',Sys.Date()+365),actionButton(ns('save'),'Save goal',class='btn-primary'),actionButton(ns('delete'),'Delete goal',class='btn-danger-soft'),tags$p(class='hint','Progress = prior baseline + units on linked completed tasks within this period. Reopening a task removes its credit. Create a new period for the next semester or year. Extra work after a contractual target earns no extra salary automatically.')))))
}
goalsServer <- function(id,state,commit) moduleServer(id,function(input,output,session) {
  editing<-reactiveVal('')
  observe({s<-state();updateSelectInput(session,'selected',choices=choices_id(s$goals),selected=if(isolate(editing())%in%s$goals$id)isolate(editing())else isolate(input$selected));updateSelectInput(session,'job',choices=choices_id(s$jobs,'name'))})
  output$cards<-renderUI({s<-state();g<-goal_progress(s);if(!nrow(g))return(tags$div(class='empty-state',icon('compass'),tags$h4('Give the longer game a direction.'),tags$p('Add a goal in the editor, then connect the tasks that move it forward.')))
    tagList(lapply(seq_len(nrow(g)),function(i) {r<-g[i,];section(r$title,tags$p(class='eyebrow',s$jobs$name[match(r$job_id,s$jobs$id)]),tags$strong(paste(r$progress,'/',r$target,r$unit)),progress_bar(r$fraction),tags$p(class='hint',paste(r$start,'to',r$end)),tags$small(paste(sum(s$tasks$goal_id==r$id & !s$tasks$done),'open linked tasks')))}))
  })
  load_goal<-function(gid) {s<-state();r<-s$goals[match(gid,s$goals$id),];req(nrow(r)==1);editing(r$id);updateTextInput(session,'title',value=r$title);updateTextInput(session,'unit',value=r$unit);updateSelectInput(session,'job',selected=r$job_id);updateNumericInput(session,'target',value=r$target);updateNumericInput(session,'baseline',value=r$baseline);updateDateInput(session,'start',value=r$start);updateDateInput(session,'end',value=r$end)}
  observeEvent(input$load,load_goal(input$selected))
  observeEvent(input$new,{editing('');updateTextInput(session,'title',value='');updateNumericInput(session,'baseline',value=0)})
  observeEvent(input$save,{
    if(!nzchar(trimws(input$title))) {showNotification('Name the goal.',type='error');return()}
    s<-state();gid<-if(nzchar(editing()))editing() else next_id('g',record_ids(s,'goals'))
    r<-data.frame(id=gid,job_id=input$job,title=trimws(input$title),target=input$target,baseline=input$baseline,unit=input$unit,start=as.character(input$start),end=as.character(input$end),stringsAsFactors=FALSE)
    if(gid%in%s$goals$id)s$goals[match(gid,s$goals$id),]<-r else s$goals<-rbind(s$goals,r)
    tryCatch({commit(s);editing(gid)},error=notify_error)
  })
  observeEvent(input$delete,{req(input$selected%in%state()$goals$id);session$userData$delete_goal<-input$selected;showModal(modalDialog(title='Delete this goal?',tags$p('Linked tasks remain, with this goal link removed. The goal is kept in removed history and Undo can restore it.'),footer=tagList(modalButton('Cancel'),actionButton(session$ns('delete_confirm'),'Delete goal',class='btn-danger'))))})
  observeEvent(input$delete_confirm,{tryCatch({commit(delete_goal(state(),session$userData$delete_goal));editing('');removeModal()},error=notify_error)})

  list(edit=function(gid){updateSelectInput(session,'selected',selected=gid);load_goal(gid)})

})
