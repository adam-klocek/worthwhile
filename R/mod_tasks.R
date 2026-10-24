tasksUI <- function(id) {
  ns<-NS(id)
  tagList(tags$div(class='dashboard-heading',tags$div(tags$p(class='eyebrow','TODAY / CHOOSE WITH INTENTION'),tags$h1('What deserves your attention?'),tags$p(class='lead','Choose deliberately. A good day does not have to be a full day.')),tags$div(class='dashboard-actions',actionButton(ns('review'),'Review tasks',class='btn-quiet'),actionButton(ns('new'),'Add a task',icon=icon('plus'),class='btn-primary'))),
    uiOutput(ns('daily_briefing')),uiOutput(ns('metrics')),
    tags$div(class='dashboard-grid',tags$div(class='dashboard-main',section('A considered next step',uiOutput(ns('cards')),checkboxInput(ns('backlog'),'Show the full backlog',FALSE),class='panel recommendation-panel'),section('Continue where you left off',uiOutput(ns('resume')),class='panel resume-panel')),
      tags$aside(class='dashboard-side',section('A realistic day',uiOutput(ns('daily_plan'))),section('Approaching deadlines',uiOutput(ns('deadlines'))),accordion('Change the decision lens',uiOutput(ns('lenses'))))),
    accordion('Task details, manual time logging & completed work',
      tags$div(class='task-desk',fluidRow(column(6,selectInput(ns('selected'),'Choose a task',choices=NULL),actionButton(ns('edit'),'Edit task'),actionButton(ns('delete'),'Delete task',class='btn-danger-soft'),actionButton(ns('nudge'),'Someone urged me again'),actionButton(ns('sessions'),'View work history'),tableOutput(ns('money'))),column(6,numericInput(ns('hours'),'Additional actual hours',1,min=.01,step=.25),numericInput(ns('after'),'Revise remaining effort (optional)',NA,min=.25,step=.25),dateInput(ns('log_date'),'Work date',Sys.Date()),textAreaInput(ns('note'),'Session note (optional)',rows=2),actionButton(ns('log'),'Save work session',class='btn-primary'),tags$p(class='hint','Logging never completes a task. An empty remaining-effort field leaves that estimate unchanged.'),actionButton(ns('reopen'),'Reopen task'),checkboxInput(ns('completed'),'Show completed tasks',FALSE),tableOutput(ns('done')))))))
}
tasksServer <- function(id,state,commit,controls,planning) moduleServer(id,function(input,output,session) {
  ns<-session$ns
  observe({s<-state();sel<-isolate(input$selected);updateSelectInput(session,'selected',choices=choices_id(s$tasks),selected=if(length(sel)==1 && sel%in%s$tasks$id) sel else head(s$tasks$id,1))})
  output$lenses<-renderUI({
    lens<-function(i)actionButton(ns(paste0('lens',i)),tagList(tags$strong(c('Balanced'='A balanced day','Urgency'='Deadlines first','Cash now'='Earn more now')[strategies[i]] %||% strategies[i]),tags$small(switch(strategies[i],Balanced='A little of what matters',Urgency='Protect your commitments','Cash now'='Marginal income per hour',strategies[i]))),class=if(controls$strategy()==strategies[i])'lens-card active' else 'lens-card')
    # All eleven modes stay available without crowding the first choice.
    tagList(tags$div(class='primary-lenses',lapply(c(1,2,4),lens)),accordion(paste('More ways to choose · current:',controls$strategy()),tags$div(class='extra-lenses',lapply(setdiff(seq_along(strategies),c(1,2,4)),function(i)actionButton(ns(paste0('lens',i)),strategies[i],class=if(controls$strategy()==strategies[i])'lens active'else'lens')))))
  })
  output$capacity_visual<-renderUI(capacity_ring(remaining_grid(planning()$grid,controls$anchor(),controls$today(),state()$settings$timezone),planning()$plan))
  output$week_visual<-renderUI(tagList(week_bars(state(),planning(),controls$anchor()),job_legend(state())))
  output$effort_visual<-renderUI(effort_rows(state(),planning(),controls$anchor()))
  output$goal_preview<-renderUI({g<-goal_progress(state());if(!nrow(g))return(tags$p(class='empty-hint','Your first finished task is a good start. Add a longer-term goal in Goals when you are ready.'));g<-head(g,3);tagList(lapply(seq_len(nrow(g)),function(i)tags$div(class='mini-goal',tags$div(tags$strong(g$title[i]),tags$small(paste(g$progress[i],'/',g$target[i],g$unit[i]))),progress_bar(g$fraction[i]))))})
  for(i in seq_along(strategies)) local({ii<-i;observeEvent(input[[paste0('lens',ii)]],{controls$set_strategy(strategies[ii])})})
  output$metrics<-renderUI({s<-state();p<-planning();day<-as.integer(controls$today()-controls$anchor())+1;g<-remaining_grid(p$grid,controls$anchor(),controls$today(),s$settings$timezone);cap<-sum(g$usable[g$day==day])/4;planned<-sum(p$plan$hours[p$plan$date==as.character(controls$today())]);tags$div(class='metric-grid today-metrics',metric('Focus time left today',paste0(cap,' h'),'After life, meetings and breathing room'),metric('Planned today',paste0(planned,' h'),'A plan, not a requirement'),metric('Current lens',controls$strategy(),'Inspectable and adjustable in Decide'))})
  output$daily_briefing<-renderUI({s<-state();p<-planning();day<-as.integer(controls$today()-controls$anchor())+1;today_plan<-p$plan[p$plan$date==as.character(controls$today()),,drop=FALSE];t<-active_tasks(s);due<-t[t$hard & effective_due(t)<=controls$today(),,drop=FALSE];tags$div(class='daily-briefing',tags$p(if(!nrow(today_plan))'There may be no focus blocks left today. Protect the space you need, or adjust availability in Plan.'else paste('Your plan makes room for',length(unique(today_plan$task_id)),'tasks today. Start with one that fits your attention.')),if(nrow(due))tags$p(class='warning-text',paste(nrow(due),'fixed-deadline tasks are due or overdue. Check scope and commitments.')),if(length(p$conflicts))tags$div(class='notice warning',paste(p$conflicts,collapse=' ')),actionButton(ns('replan'),'Replan today',class='btn-quiet'))})
  output$resume<-renderUI({c<-resume_context(state());if(!nrow(c))return(tags$p(class='empty-hint','No work in progress yet. Start a task or record a session; your context will appear here.'));card<-function(i)tags$article(class='resume-card',tags$h4(c$title[i]),tags$p(class='hint',paste(if(nzchar(c$last_date[i]))paste('Last session',c$last_date[i],'·',c$last_hours[i],'h')else'No session logged yet','·',c$total[i],'h total ·',c$remaining[i],'h still estimated')),if(nzchar(c$note[i]))tags$p(class='session-note',c$note[i]),if(nzchar(c$next_action[i]))tags$p(tags$strong('Next: '),c$next_action[i]),tags$div(class='card-actions',select_event(ns,'start',c$id[i],'Resume work','btn btn-sm btn-primary'),select_event(ns,'pause',c$id[i],'Pause','btn btn-sm btn-quiet'),select_event(ns,'log_card',c$id[i],'Log time','btn btn-sm btn-quiet'),select_event(ns,'history_card',c$id[i],'History','btn btn-sm btn-quiet')));tagList(lapply(seq_len(min(3,nrow(c))),card),if(nrow(c)>3)accordion(paste(nrow(c)-3,'more tasks in progress'),lapply(4:nrow(c),card)))})
  output$daily_plan<-renderUI({p<-planning()$plan;p<-p[p$date==as.character(controls$today()),,drop=FALSE];if(!nrow(p))return(tags$p(class='hint','Nothing scheduled for today. A lighter day can be intentional.'));tagList(lapply(seq_len(nrow(p)),function(i)tags$div(class='today-row',tags$time(paste(fmt_hour(p$start[i]),'–',fmt_hour(p$end[i]))),tags$span(p$title[i]))))})
  output$deadlines<-renderUI({t<-active_tasks(state());t<-t[effective_due(t)<=controls$today()+7,,drop=FALSE];if(!nrow(t))return(tags$p(class='hint','No deadlines in the next seven days.'));t<-head(t[order(effective_due(t)),],4);tagList(lapply(seq_len(nrow(t)),function(i)tags$div(class='deadline-row',tags$strong(t$title[i]),tags$small(paste(effective_due(t[i,]),if(t$hard[i])'fixed'else'flexible')))))})
  output$money<-renderTable({s<-state();i<-match(input$selected,s$tasks$id);req(!is.na(i));r<-s$tasks[i,];e<-task_economics(s)[i,];h<-sum(planning()$plan$hours[planning()$plan$task_id==r$id]);fraction<-if(r$remaining>0) min(1,h/r$remaining) else 0
    data.frame(Measure=c('Remaining marginal gross','Remaining marginal net (before tax)','Marginal net / remaining hour','Planned hours this week','Weekly marginal net estimate','Salary reference / contract hour'),Value=round(c(e$gross,e$net,e$net_hour,h,e$net*fraction,e$salary_hour),2),check.names=FALSE)
  },striped=TRUE)
  output$cards<-renderUI({s<-state();t<-rank_tasks(s,controls$strategy(),controls$today(),controls$seed())
    if(!nrow(t)) return(tags$div(class='empty-state',icon('leaf'),tags$h4('A little space to begin.'),tags$p('Add one meaningful task, or keep this time open.')))
    tagList(lapply(seq_len(min(nrow(t),if(isTRUE(input$backlog))100 else 3)),function(i) {r<-t[i,];j<-s$jobs[match(r$job_id,s$jobs$id),]
      due<-as.numeric(effective_due(r)-controls$today());due_text<-if(due<0)paste(abs(due),'days overdue')else if(due==0)'Due today' else if(due==1)'Due tomorrow' else paste('Due in',due,'days')
      tags$article(class=paste('task-card',task_urgency(r,controls$today()),if(r$blocked)'blocked' else ''),style=paste0('--job-colour:',job_colour(s,r$job_id)),
        tags$div(class='task-top',tags$span(class='job-pill',icon(j$icon),j$name),if(r$blocked)tags$span(class='status-label',icon('circle-pause'),'Waiting')else if(r$tier==2)tags$span(class='status-label critical',icon('circle-exclamation'),'Needs attention')),
        select_event(ns,'pick',r$id,r$title,'task-title'),tags$div(class='task-meta',tags$span(task_status(s)[match(r$id,s$tasks$id)]),tags$span(icon('clock'),paste(r$remaining,'h left')),tags$span(icon('calendar'),due_text),if(r$personal>=4)tags$span(icon('heart'),'Personally meaningful'),tags$span(icon('users'),if(nzchar(r$collaborators))r$collaborators else 'Working solo')),
        tags$div(class='task-bottom',tags$span(if(!r$billable&&r$bonus==0)'Preparation · conditional income stays in Future opportunities'else if(j$pay_type=='Salary' && r$net_hour==0)'Part of your salaried role' else paste0(round(r$net_hour,1),' ',s$settings$currency,'/h · extra income estimate')),
        tags$div(class='card-actions',
         select_event(ns,'start',r$id,if(r$started)'Resume work'else'Start work','btn btn-sm btn-primary'),
         select_event(ns,'log_card',r$id,'Log time','btn btn-sm btn-quiet'),
         select_event(ns,'finish',r$id,tagList(icon('check'),'Finish'),'btn btn-sm btn-finish'),
         tags$details(class='task-actions',tags$summary('More'),tags$div(class='task-action-menu',
          select_event(ns,'history_card',r$id,'Work history','btn btn-sm btn-quiet'),
          select_event(ns,'edit_card',r$id,'Edit task','btn btn-sm btn-quiet'))))),
        tags$details(class='why-details',tags$summary('Why this task?'),task_explanation(state(),r,planning(),controls$today(),controls$strategy())))
    }))
  })
  observeEvent(input$pick,{updateSelectInput(session,'selected',selected=input$pick)})
  finishing<-reactiveVal('')
  observeEvent(input$finish,{
    s<-state();i<-match(input$finish,s$tasks$id);req(!is.na(i));r<-s$tasks[i,]
    if(r$done)return()
    if(nzchar(r$blocked_reason)||(nzchar(r$depends)&&!isTRUE(s$tasks$done[match(r$depends,s$tasks$id)]))){showNotification('Review the blocked prerequisite first.',type='warning');return()}
    finishing(r$id);spent<-actual_hours(s,r$id)
    showModal(modalDialog(title='Finish with a useful record',tags$h4(r$title),tags$p(paste('Original estimate:',r$original_estimate,'h · Already logged:',round(spent,2),'h')),
      numericInput(ns('finish_total'),'Total actual hours for the whole task',if(spent>0)spent else NA,min=0,step=.25),
      tags$p(class='hint','Include earlier time logs in this total. Only the difference is added, so hours are not counted twice. Zero is allowed if no work was needed.'),
      footer=tagList(modalButton('Cancel'),actionButton(ns('finish_confirm'),'Save time & finish',class='btn-primary'))))
  })
  observeEvent(input$finish_confirm,{tryCatch({s<-finish_with_actual(state(),finishing(),input$finish_total,controls$today());commit(s);removeModal()},error=notify_error)})
  observeEvent(input$nudge,{s<-state();i<-match(input$selected,s$tasks$id);req(!is.na(i));s$tasks$nudges[i]<-s$tasks$nudges[i]+1;commit_safely(commit,s)})
  observeEvent(input$reopen,{commit_safely(commit,reopen_task(state(),input$selected))})
  observeEvent(input$log,{
    req(input$selected,input$log_date)
    remain<-if(is.null(input$after)||is.na(input$after))NULL else input$after
    tryCatch(commit(log_work(state(),input$selected,input$hours,input$log_date,remain,input$note %||% '')),error=notify_error)
  })
  ask_delete<-function(tid){s<-state();i<-match(tid,s$tasks$id);req(!is.na(i));session$userData$delete_task<-tid;showModal(modalDialog(title='Delete this task from the planner?',tags$p(s$tasks$title[i]),tags$p('Its estimates, completion record and work logs remain in Reflect under deleted history. Dependants will be blocked for review. Undo restores it.'),footer=tagList(modalButton('Cancel'),actionButton(ns('delete_confirm'),'Delete task',class='btn-danger'))))}
  observeEvent(input$delete,ask_delete(input$selected))
  observeEvent(input$delete_confirm,{tryCatch({commit(delete_task(state(),session$userData$delete_task));removeModal()},error=notify_error)})
  output$done<-renderTable({req(input$completed);s<-state();s$tasks[s$tasks$done,c('title','completed','original_estimate','collaborators')]},striped=TRUE)
  show_editor<-function(new=FALSE,task_id=input$selected) {
    s<-state();if(!nrow(s$jobs)){showNotification('Add a job in Jobs before creating a task.',type='warning');return()}
    i<-match(task_id %||% '',s$tasks$id)
    r<-if(new || length(i)!=1 || is.na(i)) s$tasks[FALSE,] else s$tasks[i,]
    v<-function(col,default) if(nrow(r)) r[[col]][1] else default
    showModal(modalDialog(title=if(nrow(r))'Edit your task' else 'One clear next action',size='m',
      tags$div(style='display:none',textInput(ns('f_id'),'ID',v('id',next_id('t',record_ids(s,'tasks'))))),
      textInput(ns('f_title'),'What needs doing?',v('title',''),placeholder='e.g. Revise the methods section'),textInput(ns('f_next'),'Next concrete action (optional)',v('next_action','')),textInput(ns('f_people'),'People working with me (optional)',v('collaborators',''),placeholder='Names separated by commas, or leave empty for no one'),
      form_row(column(7,selectInput(ns('f_job'),'Which job?',choices_id(s$jobs,'name'),v('job_id',s$jobs$id[1]))),column(5,numericInput(ns('f_remaining'),'Hours needed from now',v('remaining',1),min=if(v('done',FALSE))0 else .25,step=.25))),
      dateInput(ns('f_due'),'Aim to finish by',v('due',as.character(controls$today()+7))),checkboxInput(ns('f_hard'),'This deadline cannot move',v('hard',FALSE)),
      conditionalPanel("output.editor_piece",ns=ns,numericInput(ns('f_units'),'Remaining paid pages / rooms / items',v('units',0),min=0)),
      accordion('Optional: importance, goals & finer details',
        form_row(column(6,numericInput(ns('f_consequence'),'Consequences if missed (0–5)',v('consequence',2),min=0,max=5)),column(6,numericInput(ns('f_personal'),'Personal meaning (0–5)',v('personal',2),min=0,max=5))),
        form_row(column(6,numericInput(ns('f_social'),'Social impact (0–5)',v('social',2),min=0,max=5)),column(6,numericInput(ns('f_career'),'Career relevance (0–5)',v('career',2),min=0,max=5))),
        checkboxInput(ns('f_billable'),'This task earns the work role’s hourly / piece rate',v('billable',TRUE)),
        selectInput(ns('f_goal'),'Connect to a goal',choices_id(s$goals,blank=TRUE),v('goal_id','')),numericInput(ns('f_goalunits'),'Goal units when finished',v('goal_units',0),min=0),
        selectInput(ns('f_dep'),'Must wait for another task?',choices_id(s$tasks[s$tasks$id!=v('id',''),],blank=TRUE),v('depends','')),
        form_row(column(6,numericInput(ns('f_total'),'Current overall plan (h); original estimate is preserved',v('total',1),min=.25,step=.25)),column(6,numericInput(ns('f_block'),'Shortest useful focus block (h)',v('min_block',.5),min=.25,step=.25))),
        textInput(ns('f_area'),'Contract area, if any',v('area','')),textInput(ns('f_blocked'),'Reason this task is blocked (clear when resolved)',v('blocked_reason','')),numericInput(ns('f_extension'),'Agreed extension in days (flexible only)',v('extension',0),min=0,step=1),numericInput(ns('f_nudges'),'Times someone has urged me',v('nudges',0),min=0,step=1),numericInput(ns('f_bonus'),'Guaranteed completion bonus',v('bonus',0),min=0),numericInput(ns('f_cost'),'Remaining cash expenses',v('cash_cost',0),min=0),tags$p(class='hint','A 5/5 consequence always takes priority. Connect the final paper milestone to a publication goal to avoid crediting it early.')),
      footer=tagList(modalButton('Cancel'),actionButton(ns('save'),'Save task',class='btn-primary'))))
    session$userData$editing_task <- if(nrow(r)) r$id else ''
  }
  output$editor_piece<-reactive({s<-state();isTRUE(s$jobs$pay_type[match(input$f_job,s$jobs$id)]=='Piece')});outputOptions(output,'editor_piece',suspendWhenHidden=FALSE)
  observeEvent(input$edit,show_editor())
  observeEvent(input$new,show_editor(TRUE))
  observeEvent(input$edit_card,{updateSelectInput(session,'selected',selected=input$edit_card);session$userData$card_edit_id<-input$edit_card;show_editor(FALSE,input$edit_card)})
  observeEvent(input$save,{
    s<-state();old<-session$userData$editing_task;id<-if(nzchar(old))old else trimws(input$f_id)
    if(!nzchar(trimws(input$f_title)) || (!nzchar(old)&&id%in%s$tasks$id)) {showNotification('Provide a title and a unique ID.',type='error');return()}
    r<-data.frame(id=id,job_id=input$f_job,title=trimws(input$f_title),area=input$f_area,total=input$f_total,remaining=input$f_remaining,due=as.character(input$f_due),hard=input$f_hard,extension=input$f_extension,nudges=input$f_nudges,consequence=input$f_consequence,personal=input$f_personal,social=input$f_social,career=input$f_career,min_block=input$f_block,units=input$f_units,bonus=input$f_bonus,cash_cost=input$f_cost,goal_id=input$f_goal,goal_units=input$f_goalunits,depends=input$f_dep,done=FALSE,completed='',original_estimate=input$f_remaining,collaborators=input$f_people %||% '',created=as.character(controls$today()),time_complete=FALSE,blocked_reason=input$f_blocked %||% '',started=FALSE,paused=FALSE,next_action=input$f_next %||% '',billable=if(is.null(input$f_billable)){if(nzchar(old))s$tasks$billable[match(old,s$tasks$id)]else TRUE}else isTRUE(input$f_billable),stringsAsFactors=FALSE)
    if(!nzchar(old)) {r$total<-r$remaining;r$min_block<-min(r$min_block,r$remaining)}
    if(nzchar(old)) {i<-match(old,s$tasks$id);r$done<-s$tasks$done[i];r$completed<-s$tasks$completed[i];r$original_estimate<-s$tasks$original_estimate[i];r$created<-s$tasks$created[i];r$time_complete<-s$tasks$time_complete[i];r$started<-s$tasks$started[i];r$paused<-s$tasks$paused[i];r$total<-max(r$total,r$remaining);s$logs$job_id[s$logs$task_id==id]<-r$job_id;s$tasks[i,]<-r} else s$tasks<-rbind(s$tasks,r)
    tryCatch({commit(s);removeModal();updateSelectInput(session,'selected',selected=id)},error=notify_error)
  })
  review_id<-reactiveVal('')
  show_review<-function(tid=NULL) {
    s<-state();t<-active_tasks(s)
    if(!nrow(t)){showModal(modalDialog(title='Your task review',tags$p('No open tasks. Add a task or review completed work in Reflect.'),easyClose=TRUE));return(invisible(NULL))}
    if(is.null(tid)||!tid%in%t$id)tid<-t$id[1]
    review_id(tid);r<-t[match(tid,t$id),]
    showModal(modalDialog(title='A quick task check-in',size='l',tags$p(class='hint','Update what changed since last time. Save before switching tasks, or use Save & next.'),
      selectInput(ns('review_selected'),'Task to revisit',choices_id(t),tid),tags$div(class='review-baseline',paste('Original estimate:',r$original_estimate,'h · Actual logged:',round(actual_hours(s,tid),2),'h')),
      fluidRow(column(4,numericInput(ns('r_nudges'),'Times urged',r$nudges,min=0,step=1)),column(4,numericInput(ns('r_remaining'),'Hours still needed',r$remaining,min=.25,step=.25)),column(4,numericInput(ns('r_consequence'),'Cost of missing it (0–5)',r$consequence,min=0,max=5))),
      fluidRow(column(6,dateInput(ns('r_due'),'Deadline',r$due)),column(6,numericInput(ns('r_personal'),'Personal importance (0–5)',r$personal,min=0,max=5))),
      checkboxInput(ns('r_hard'),'Deadline cannot move',r$hard),numericInput(ns('r_extension'),'Agreed extension in days (flexible only)',r$extension,min=0,step=1),
      textInput(ns('r_people'),'People working with me (empty means no one)',r$collaborators),
      textInput(ns('r_blocked'),'Blocked reason (clear after resolving)',r$blocked_reason),
      footer=tagList(modalButton('Done reviewing'),actionButton(ns('review_save'),'Save changes',class='btn-quiet'),actionButton(ns('review_next'),'Save & next',class='btn-primary'))))
  }
  observeEvent(input$review,show_review())
  observeEvent(input$review_selected,{if(!identical(input$review_selected,review_id()))show_review(input$review_selected)})
  save_review<-function(next_task=FALSE) {
    tryCatch({s<-state();i<-match(review_id(),s$tasks$id);req(!is.na(i));s$tasks$nudges[i]<-input$r_nudges;s$tasks$remaining[i]<-input$r_remaining;s$tasks$total[i]<-max(s$tasks$total[i],input$r_remaining);s$tasks$consequence[i]<-input$r_consequence;s$tasks$personal[i]<-input$r_personal;s$tasks$due[i]<-as.character(input$r_due);s$tasks$hard[i]<-isTRUE(input$r_hard);s$tasks$extension[i]<-input$r_extension;s$tasks$collaborators[i]<-input$r_people %||% '';s$tasks$blocked_reason[i]<-input$r_blocked %||% '';commit(s)
      if(next_task){ids<-active_tasks(s)$id;idx<-match(review_id(),ids);show_review(ids[if(idx<length(ids))idx+1 else 1])}
    },error=notify_error)
  }
  observeEvent(input$review_save,save_review())
  observeEvent(input$review_next,save_review(TRUE))
  work_id<-reactiveVal('')
  show_log<-function(tid){s<-state();i<-match(tid,s$tasks$id);req(!is.na(i));work_id(tid);r<-s$tasks[i,];showModal(modalDialog(title='Record a work session',tags$h4(r$title),tags$p(class='hint',paste('Already logged:',round(actual_hours(s,tid),2),'h · Remaining estimate:',r$remaining,'h')),
    dateInput(ns('q_date'),'Work date',controls$today()),numericInput(ns('q_hours'),'Additional hours worked',NA,min=.01,step=.25),textAreaInput(ns('q_note'),'What did you do? (optional)',rows=3),numericInput(ns('q_remaining'),'New remaining estimate (optional)',NA,min=.25,step=.25),textInput(ns('q_next'),'Where should you pick up next?',r$next_action),tags$p(class='hint','The remaining estimate stays unchanged unless you revise it. Start/Pause are focus bookmarks, not a running timer.'),footer=tagList(modalButton('Cancel'),actionButton(ns('q_save'),'Save session',class='btn-primary'))))}
  observeEvent(input$log_card,show_log(input$log_card))
  observeEvent(input$start,{tryCatch({commit(start_focus(state(),input$start));show_log(input$start)},error=notify_error)})
  observeEvent(input$pause,{commit_safely(commit,pause_focus(state(),input$pause))})
  observeEvent(input$q_save,{tryCatch({remain<-if(is.null(input$q_remaining)||is.na(input$q_remaining))NULL else input$q_remaining;s<-log_work(state(),work_id(),input$q_hours,input$q_date,remain,input$q_note %||% '');s$tasks$next_action[match(work_id(),s$tasks$id)]<-input$q_next %||% '';commit(s);removeModal()},error=notify_error)})
  session_task<-reactiveVal('')
  show_sessions<-function(tid){s<-state();session_task(tid);l<-all_records(s,'logs');l<-l[l$task_id==tid,,drop=FALSE];showModal(modalDialog(title='Work-session history',size='l',tableOutput(ns('session_table')),selectInput(ns('session_id'),'Choose an entry to correct',if(nrow(l))setNames(l$id,paste(l$date,l$hours,'h',l$note))else character()),numericInput(ns('s_hours'),'Actual hours',NA,min=.01,step=.25),dateInput(ns('s_date'),'Work date',controls$today()),textAreaInput(ns('s_note'),'Session note',rows=2),tags$p(class='hint','Changing a session changes actual totals, never the remaining-work estimate.'),footer=tagList(modalButton('Close'),actionButton(ns('s_delete'),'Delete selected entry',class='btn-danger-soft'),actionButton(ns('s_save'),'Save correction',class='btn-primary'))))}
  output$session_table<-renderTable({l<-all_records(state(),'logs');l[l$task_id==session_task(),c('date','hours','note','created_at','updated_at'),drop=FALSE]},digits=2,striped=TRUE)
  observeEvent(input$sessions,show_sessions(input$selected))
  observeEvent(input$history_card,show_sessions(input$history_card))
  observeEvent(input$session_id,{l<-all_records(state(),'logs');i<-match(input$session_id,l$id);req(!is.na(i));updateNumericInput(session,'s_hours',value=l$hours[i]);updateDateInput(session,'s_date',value=l$date[i]);updateTextAreaInput(session,'s_note',value=l$note[i])})
  observeEvent(input$s_save,{tryCatch({commit(edit_work_session(state(),input$session_id,input$s_hours,input$s_date,input$s_note %||% ''));showNotification('Session corrected; remaining effort was preserved.')},error=notify_error)})
  observeEvent(input$s_delete,{req(input$session_id);session$userData$session_delete<-input$session_id;showModal(modalDialog(title='Delete this incorrect session?',tags$p('Actual totals will be recalculated. The task and remaining estimate stay intact. Undo can recover the entry.'),footer=tagList(modalButton('Cancel'),actionButton(ns('s_delete_confirm'),'Delete session',class='btn-danger'))))})
  observeEvent(input$s_delete_confirm,{s<-state();id<-session$userData$session_delete;if(id%in%s$logs$id)s$logs<-s$logs[s$logs$id!=id,,drop=FALSE]else s$archive$logs<-s$archive$logs[s$archive$logs$id!=id,,drop=FALSE];tryCatch({commit(s);show_sessions(session_task())},error=notify_error)})
  list(review=show_review,review_id=review_id,log=show_log,history=show_sessions)

})
