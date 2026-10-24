jobsUI <- function(id) {
  ns<-NS(id)
  tagList(tags$h2('The jobs behind the work.'),tags$p(class='lead','Contracts define obligations. Pay models define marginal earnings. Keep both visible.'),
    fluidRow(column(7,uiOutput(ns('cards')),section('This week: contract effort',tableOutput(ns('balance')),tags$p(class='hint','Actual task logs + recurring meetings are compared with FTE × full-time hours. Future meetings count as committed time. A zero FTE uses your own weekly target.'))),
      column(5,section('Job editor',selectInput(ns('selected'),'Select a job or create one',NULL),actionButton(ns('load'),'Load selected'),actionButton(ns('new'),'Clear for new job'),textInput(ns('name'),'Job name'),selectInput(ns('icon'),'Icon',job_icons),selectInput(ns('engagement'),'Arrangement',c('Employee','Self-employed')),selectInput(ns('pay_type'),'Pay model',c('Salary','Hourly','Piece')),numericInput(ns('fte'),'Contract FTE',0,min=0,max=1,step=.1),numericInput(ns('target'),'Weekly target if FTE = 0 (h)',5,min=0,step=.25),numericInput(ns('salary'),'Fixed weekly gross salary (if salaried)',0,min=0),numericInput(ns('rate'),'Rate per hour / page / room',0,min=0),textInput(ns('unit'),'Paid unit','hour'),numericInput(ns('expense'),'Estimated business expenses (%)',0,min=0,max=100),textAreaInput(ns('areas'),'Contract areas: one Name=FTE per line',rows=3,placeholder='Teaching=0.3\nResearch=0.2'),textAreaInput(ns('notes'),'Notes',rows=3),actionButton(ns('save'),'Save job',class='btn-primary'),actionButton(ns('delete'),'Delete selected job',class='btn-danger-soft'),textOutput(ns('editing_label')),tags$p(class='hint','Gross rates and expense estimates use one currency. Tax, VAT, invoicing and employee benefits are not modelled.')))))
}
jobsServer <- function(id,state,commit,controls) moduleServer(id,function(input,output,session) {
  editing<-reactiveVal('')
  observe({s<-state();updateSelectInput(session,'selected',choices=choices_id(s$jobs,'name'),selected=isolate(input$selected))})
  load_job<-function(jid) {
    s<-state();i<-match(jid,s$jobs$id);req(length(i)==1,!is.na(i));r<-s$jobs[i,];editing(jid)
    for(n in c('name','unit','notes')) updateTextInput(session,n,value=r[[n]])
    for(n in c('icon','engagement','pay_type')) updateSelectInput(session,n,selected=r[[n]])
    for(n in c('fte','target','rate')) updateNumericInput(session,n,value=r[[n]])
    updateNumericInput(session,'salary',value=r$weekly_income);updateNumericInput(session,'expense',value=r$expense_pct)
    a<-s$allocations[s$allocations$job_id==jid,];updateTextAreaInput(session,'areas',value=paste(paste0(a$name,'=',a$fte),collapse='\n'))
  }
  observeEvent(input$load,load_job(input$selected))
  observeEvent(input$selected,{if(length(input$selected)==1&&input$selected%in%state()$jobs$id)load_job(input$selected)})
  observeEvent(input$edit_card,{load_job(input$edit_card);updateSelectInput(session,'selected',selected=input$edit_card)})
  output$editing_label<-renderText(if(nzchar(editing()))'Editing an existing job; Save updates it.' else 'Creating a new job.')
  observeEvent(input$new,{editing('');updateTextInput(session,'name',value='');updateTextAreaInput(session,'areas',value='');updateNumericInput(session,'fte',value=0);updateNumericInput(session,'salary',value=0);updateNumericInput(session,'rate',value=0)})
  output$cards<-renderUI({s<-state();if(!nrow(s$jobs))return(tags$div(class='empty-state',icon('briefcase'),tags$h4('Start with one work role.'),tags$p('Add a name and pay model in the editor. You can refine the details later.')));tagList(lapply(seq_len(nrow(s$jobs)),function(i) {j<-s$jobs[i,];a<-s$allocations[s$allocations$job_id==j$id,]
    section(j$name,tags$div(class='job-icon',icon(j$icon)),tags$p(paste(j$engagement,'·',j$pay_type,'·',j$fte,'FTE')),
      tags$strong(if(j$pay_type=='Salary')paste(j$weekly_income,s$settings$currency,'/ week') else paste(j$rate,s$settings$currency,'/',j$unit)),
      if(nrow(a)) tags$p(paste(paste0(a$name,': ',a$fte,' FTE'),collapse=' · ')),tags$p(class='hint',j$notes),select_event(session$ns,'edit_card',j$id,'Edit job','btn btn-sm btn-quiet'))
  }))})
  output$balance<-renderTable({s<-state();tgt<-job_targets(s,s$settings$fulltime);log_rows<-all_records(s,'logs');l<-log_rows[log_rows$date>=as.character(controls$anchor())&log_rows$date<=as.character(controls$anchor()+6),]
    work<-vapply(s$jobs$id,function(j)sum(l$hours[l$job_id==j]),numeric(1));meet<-job_meeting_hours(s)
    data.frame(Job=s$jobs$name,Target=tgt,Logged=work,Meetings=meet,Gap=round(tgt-work-meet,2),check.names=FALSE)
  },striped=TRUE)
  observeEvent(input$save,{
    tryCatch({
      s<-state();jid<-if(nzchar(editing()))editing() else next_id('j',record_ids(s,'jobs'))
      if(!nzchar(trimws(input$name))) stop('Name the job.')
      j<-data.frame(id=jid,name=trimws(input$name),icon=input$icon,engagement=input$engagement,pay_type=input$pay_type,fte=input$fte,target=input$target,weekly_income=if(input$pay_type=='Salary')input$salary else 0,rate=input$rate,unit=input$unit,expense_pct=input$expense,notes=input$notes,stringsAsFactors=FALSE)
      if(jid%in%s$jobs$id) s$jobs[match(jid,s$jobs$id),]<-j else s$jobs<-rbind(s$jobs,j)
      s$allocations<-s$allocations[s$allocations$job_id!=jid,,drop=FALSE]
      lines<-trimws(strsplit(input$areas,'\n',fixed=TRUE)[[1]]);lines<-lines[nzchar(lines)]
      for(line in lines) {
        parts<-strsplit(line,'=',fixed=TRUE)[[1]]
        if(length(parts)!=2 || !nzchar(trimws(parts[1]))) stop('Use Name=FTE for each contract area.')
        s$allocations<-rbind(s$allocations,data.frame(id=next_id('c',record_ids(s,'allocations')),job_id=jid,name=trimws(parts[1]),fte=num(parts[2]),stringsAsFactors=FALSE))
      }
      commit(s);editing(jid)
    },error=notify_error)
  })
  observeEvent(input$delete,{s<-state();jid<-input$selected;req(jid%in%s$jobs$id);session$userData$delete_job<-jid;showModal(modalDialog(title='Delete this job from the planner?',tags$h4(s$jobs$name[match(jid,s$jobs$id)]),tags$p(paste(sum(s$tasks$job_id==jid),'tasks and this job’s goals and meetings will leave the active plan. Their time records and task estimates remain in report history. Undo restores the job and its records.')),footer=tagList(modalButton('Cancel'),actionButton(session$ns('delete_confirm'),'Delete job & its active work',class='btn-danger'))))})
  observeEvent(input$delete_confirm,{tryCatch({commit(delete_job(state(),session$userData$delete_job));editing('');removeModal()},error=notify_error)})

})
