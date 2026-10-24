Sys.setenv(WORTHWHILE_MODE='local')
# Worthwhile regression suite. Run from the application directory.
Sys.setenv(WORTHWHILE_STATE_FILE='',PORTFOLIO_STATE_FILE='')
library(shiny);source('app.R')
passed<-0L
ok<-function(x,label){stopifnot(isTRUE(x));passed<<-passed+1L;cat('PASS:',label,'\n')}
fails<-function(x)inherits(tryCatch({force(x);NULL},error=identity),'error')
s<-demo_state(as.Date('2026-11-02'));s$tasks<-simple_task('t100','j1','Write the introduction',8,as.Date('2026-11-13'),FALSE)
original<-s
s<-log_work(s,'t100',1.5,'2026-11-02',note='Outline ready')
s<-log_work(s,'t100',2,'2026-11-04',note='Draft paragraphs')
s<-log_work(s,'t100',.75,'2026-11-06',note='References checked')
ok(nrow(s$logs)==3&&actual_hours(s,'t100')==4.25&&!s$tasks$done,'three distinct sessions total 4.25 h without completion')
ok(s$tasks$remaining==8&&s$tasks$original_estimate==8,'logging does not infer remaining effort')
ok(length(unique(s$logs$id))==3&&all(nzchar(s$logs$created_at))&&all(nzchar(s$logs$updated_at)),'stable session IDs and timestamps')
s<-log_work(s,'t100',.5,'2026-11-06',remaining=5,note='One more session')
ok(nrow(s$logs)==4&&actual_hours(s,'t100')==4.75&&s$tasks$remaining==5,'multiple sessions on one date and explicit independent remaining estimate')
s<-edit_work_session(s,s$logs$id[1],1,'2026-11-02','Corrected outline')
ok(actual_hours(s,'t100')==4.25&&s$tasks$remaining==5&&s$logs$note[1]=='Corrected outline','session correction recalculates total only')
s$logs<-s$logs[-4,,drop=FALSE]
ok(actual_hours(s,'t100')==3.75&&s$tasks$remaining==5,'session deletion preserves remaining work')
ctx<-resume_context(s)
ok(ctx$last_date=='2026-11-06'&&ctx$last_hours==.75&&ctx$total==3.75&&ctx$note=='References checked','resume context contains latest session and cumulative actuals')
s<-start_focus(s,'t100');s<-pause_focus(s,'t100')
ok(s$tasks$started&&s$tasks$paused&&task_status(s)=='In progress','focus bookmarks retain in-progress status')
s<-finish_with_actual(s,'t100',5,'2026-11-07')
ok(actual_hours(s,'t100')==5&&nrow(s$logs)==4&&s$tasks$done&&s$tasks$remaining==0,'finish records only unlogged difference')
s<-finish_with_actual(s,'t100',5,'2026-11-07')
ok(actual_hours(s,'t100')==5&&nrow(s$logs)==4&&task_report(s)$Verified,'repeat finish is idempotent and retained in reports')
z<-original;z<-log_work(z,'t100',12,'2026-11-02');ok(!z$tasks$done&&z$tasks$remaining==8&&actual_hours(z,'t100')==12,'actual effort may exceed estimate without completion')
ok(fails(log_work(z,'t100',-1))&&fails(log_work(z,'t100',Inf))&&fails(log_work(z,'t100',NaN))&&fails(edit_work_session(z,z$logs$id[1],0,'2026-11-02')),'negative, nonfinite and zero session durations rejected')
z$logs<-z$logs[FALSE,,drop=FALSE];z<-log_work(z,'t100',1,'2026-11-03');ok(z$logs$id=='l2','deleted session identifiers are not reused')
# Both old schemas are constructed without schema-3 metadata, including archived history.
for(version in c(1L,2L)) {
 old<-demo_state(as.Date('2026-11-02'));old<-log_work(old,'t1',1,'2026-11-02');old<-delete_task(old,'t1')
 for(place in c('active','archive'))for(nm in c('tasks','logs')) {
   d<-if(place=='active')old[[nm]]else old$archive[[nm]]
   drop<-if(nm=='tasks')c('started','paused','next_action')else c('note','created_at','updated_at')
   d<-d[,!names(d)%in%drop,drop=FALSE]
   if(place=='active')old[[nm]]<-d else old$archive[[nm]]<-d
 }
 old$schema<-version;old$exceptions<-NULL;old$locks<-NULL;old$revision<-NULL;old$log_sequence<-NULL
 if(version==1L){old$archive<-NULL;old$tasks<-old$tasks[,!names(old$tasks)%in%c('original_estimate','collaborators','created','time_complete','blocked_reason'),drop=FALSE]}
 f<-tempfile(fileext='.json');jsonlite::write_json(old,f,auto_unbox=TRUE,dataframe='columns')
 up<-read_backup(f);ok(validate_state(up)&&up$schema==4L&&nrow(up$tasks)==16,paste('schema',version,'migrates without losing active tasks'))
 up$revision<-1L;save_state(up,f,expected_revision=0L)
 ok(file.exists(paste0(f,'.schema',version,'.backup.json'))&&file.exists(paste0(f,'.previous.json')),paste('schema',version,'original snapshot and previous-good backup preserved'))
 ok(fails(save_state(up,f,expected_revision=0L)),'stale-session save rejected')
 unlink(c(f,paste0(f,'.schema',version,'.backup.json'),paste0(f,'.previous.json')))
}
# Dated capacity, manual work locks and financial overlays use the original engine.
s<-demo_state(as.Date('2026-11-02'));anchor<-as.Date('2026-11-02');today<-anchor
p<-plan_work(s,'Balanced',anchor,TRUE,0,12,1,today)
z<-add_exception(s,anchor,10,11,'Meeting','Unexpected meeting','j1')
pz<-plan_work(z,'Balanced',anchor,TRUE,0,12,1,today)
ix<-interval_indices(1,10,11)
ok(all(pz$grid$blocked[ix])&&!any(vapply(seq_len(nrow(pz$plan)),function(i){r<-pz$plan[i,];r$day==1&&max(r$start,10)<min(r$end,11)},logical(1))),'dated meeting blocks real scheduling capacity')
z<-add_exception(s,anchor,9,10,'Available','Shorter Monday')
g<-capacity_grid(z,TRUE,0,12,anchor)
ok(!any(g$available[g$day==1 & (g$start<9|g$start>=10)]),'dated work window replaces that day recurring availability')
# Choose an existing valid scheduled interval for a manual lock.
r<-p$plan[1,];s$locks<-data.frame(id='m1',task_id=r$task_id,date=r$date,start=r$start,end=r$end)
p2<-plan_work(s,'Personal meaning',anchor,TRUE,0,12,1,today)
ok(!length(p2$conflicts)&&any(p2$plan$task_id==r$task_id&p2$plan$start==r$start&p2$plan$end==r$end),'manual work decision survives a priority change')
z<-add_exception(s,r$date,r$start,r$end,'Unavailable','Interruption');pz<-plan_work(z,'Balanced',anchor,TRUE,0,12,1,today)
ok(length(pz$conflicts)>0,'fixed interruption conflicting with a lock is surfaced')
base<-demo_state(anchor);after<-remove_focus_hours(base,5,anchor,TRUE,15,8,today)
beforeg<-remaining_grid(capacity_grid(base,TRUE,15,8,anchor),anchor,today,base$settings$timezone)
afterg<-remaining_grid(capacity_grid(after,TRUE,15,8,anchor),anchor,today,base$settings$timezone)
ok((sum(beforeg$usable)-sum(afterg$usable))/4==5&&nrow(base$exceptions)==0,'scenario removes five net focus hours without mutating source')
# Real Shiny interactions: logging, corrections/deletion, stale previews, explicit apply.
shiny::testServer(server,{
 session$setInputs(week='2026-11-02',buffer=0,cap=12,protect=TRUE)
 session$setInputs(`tasks-log_card`='t11')
 session$setInputs(`tasks-q_date`='2026-11-02',`tasks-q_hours`=1.5,`tasks-q_note`='Opening pages',`tasks-q_remaining`=NA,`tasks-q_next`='Continue page six',`tasks-q_save`=1)
 stopifnot(actual_hours(state(),'t11')==1.5,state()$tasks$remaining[11]==6,state()$tasks$next_action[11]=='Continue page six')
 session$setInputs(`tasks-q_hours`=2,`tasks-q_date`='2026-11-04',`tasks-q_save`=2)
 stopifnot(actual_hours(state(),'t11')==3.5,nrow(state()$logs)==2)
 lid<-state()$logs$id[1]
 session$setInputs(`tasks-history_card`='t11',`tasks-session_id`=lid)
 session$setInputs(`tasks-s_hours`=1,`tasks-s_date`='2026-11-02',`tasks-s_note`='Correction',`tasks-s_save`=1)
 stopifnot(actual_hours(state(),'t11')==3,state()$tasks$remaining[11]==6)
 session$setInputs(`tasks-s_delete`=1)
 stopifnot(nrow(state()$logs)==2) # confirmation is required
 session$setInputs(`tasks-s_delete_confirm`=1)
 stopifnot(actual_hours(state(),'t11')==2,nrow(state()$logs)==1)
 session$setInputs(`replan-date`='2026-11-02',`replan-kind`='Meeting',`replan-start`='10',`replan-end`='11',`replan-title`='Unexpected review',`replan-job`='j1',`replan-task`='',`replan-lock`=FALSE,`replan-preview`=1)
 stopifnot(!is.null(replanner$preview()),nrow(state()$exceptions)==0)
 session$setInputs(`tasks-selected`='t11',`tasks-nudge`=1)
 session$setInputs(`replan-apply`=1)
 stopifnot(nrow(state()$exceptions)==0,grepl('changed',replanner$error()))
 session$setInputs(`replan-preview`=2,`replan-apply`=2)
 stopifnot(nrow(state()$exceptions)==1,is.null(replanner$preview()))
 session$setInputs(`decide-sim_strategy`='Cash now',`decide-fewer`=5,`decide-project`=TRUE,`decide-project_job`='j5',`decide-project_name`='Possible build',`decide-project_hours`=2,`decide-project_bonus`=100,`decide-simulate`=1)
 stopifnot(!is.null(decisions$scenario()),nrow(state()$tasks)==17)
 session$setInputs(`decide-apply`=1)
 stopifnot(nrow(state()$tasks)==18,strategy()=='Cash now',is.null(decisions$scenario()))
 session$setInputs(undo=1)
 stopifnot(nrow(state()$tasks)==17,strategy()=='Balanced')
 for(page in c('today','plan','decide','reflect'))session$setInputs(page=page)
 stopifnot(nzchar(output[['tasks-resume']]$html),nzchar(output[['insights-metrics']]$html),nzchar(output[['capacity-week_calendar']]$html))
})
ok(TRUE,'Shiny session logging, correction, confirmation, stale replan, sandbox and explicit scenario apply')
# Restart from a real file preserves every field, settings, focus context and review.
f<-tempfile(fileext='.json');s<-demo_state();s<-log_work(s,'t11',2,'2026-11-02',note='Restart context');s<-pause_focus(s,'t11');s$tasks$next_action[11]<-'Next page';s$settings$plan_strategy<-'Personal meaning';s$settings$plan_buffer<-20;s$settings$plan_cap<-6;s$settings$plan_protect<-TRUE;save_state(s,f)
Sys.setenv(WORTHWHILE_STATE_FILE=f)
shiny::testServer(server,{
 session$setInputs(week='2026-11-02',buffer=20,cap=6,protect=TRUE)
 stopifnot(saved_exists,strategy()=='Personal meaning',state()$tasks$paused[11],resume_context(state())$note=='Restart context',nzchar(task_manager$review_id()))
 session$setInputs(`tasks-selected`='t11',`tasks-nudge`=1)
 stopifnot(read_backup(f)$settings$plan_cap==6,read_backup(f)$tasks$next_action[11]=='Next page',actual_hours(read_backup(f),'t11')==2)
})
unlink(c(f,paste0(f,'.previous.json')));Sys.setenv(WORTHWHILE_STATE_FILE='',PORTFOLIO_STATE_FILE='')
ok(TRUE,'real-file restart preserves notes, bookmarks, remaining effort, lens and task review')
# Inspect the actual navigation structure, with exactly four primary experiences.
html<-as.character(ui)
ok(all(vapply(c('today','plan','decide','reflect'),function(x)grepl(paste0('data-value="',x,'"'),html,fixed=TRUE),logical(1)))&&!grepl('Portfolio Planner',html,fixed=TRUE),'four primary experience tabs render with Worthwhile branding')
cat(passed,'Worthwhile feature checks passed, including two Shiny interaction scenarios.\n')
