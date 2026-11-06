# Version 2 migration, accounting, calendar, routines and actual Shiny flows.
Sys.setenv(WORTHWHILE_MODE='local',WORTHWHILE_STATE_FILE='',PORTFOLIO_STATE_FILE='')
library(shiny);source('app.R')
checks<-0L
ok<-function(x,msg){stopifnot(isTRUE(x));checks<<-checks+1L;cat('PASS v2:',msg,'\n')}
throws<-function(expr)inherits(tryCatch({force(expr);NULL},error=identity),'error')
s<-demo_state(as.Date('2026-11-02'));s<-log_work(s,'t11',2,'2026-11-02',note='Original text');s<-delete_task(s,'t9');s$settings$plan_strategy<-'Social impact';old<-s;old$schema<-4L;for(n in v2_tables())old[[n]]<-NULL
old$settings<-old$settings[!names(old$settings)%in%c('reserve_pct','timesheet_start','timesheet_end','timesheet_lunch_start','timesheet_lunch_end')]
f<-tempfile();jsonlite::write_json(old,f,auto_unbox=TRUE,dataframe='columns');up<-read_backup(f);unlink(f)
for(n in record_tables){ok(isTRUE(all.equal(up[[n]],old[[n]],check.attributes=FALSE)),paste('schema 4 preserves',n));ok(isTRUE(all.equal(up$archive[[n]],old$archive[[n]],check.attributes=FALSE)),paste('schema 4 preserves archived',n))}
ok(up$schema==5L&&up$settings$plan_strategy=='Category:social'&&up$settings$reserve_pct==20,'migration adds defaults and preserves the previous decision lens')
ok(nrow(up$session_details)==0&&actual_hours(up,'t11')==2,'old actual durations retain unknown clock times')
f<-tempfile();save_state(up,f);twice<-read_backup(f);unlink(f);ok(identical(twice$session_details,up$session_details)&&nrow(twice$categories)==2,'schema 5 round-trip and idempotent migration')
# Category preferences and ratings affect preference order, never critical tiers.
s<-demo_state(as.Date('2026-11-02'));s$categories<-rbind(s$categories,data.frame(id='health',label='Health',enabled=TRUE,weight=.2));s$ratings<-data.frame(task_id=c('t11','t12'),category_id='health',value=c(5,1));r<-rank_tasks(s,'Category:health',as.Date('2026-11-02'))
ok(r$score[r$id=='t11']==1&&r$score[r$id=='t12']==.2&&r$tier[r$id=='t13']==2,'custom lens preserves safety tiers and uses entered ratings')
s$categories$enabled[s$categories$id=='health']<-FALSE;ok(!'Category:health'%in%decision_lenses(s)&&nrow(s$ratings)==2,'disabled category keeps existing ratings')
# Chores reset by date/week, not by destructive resets.
s$chores<-data.frame(id=c('ch1','ch2'),title=c('Dishes','Bins'),period=c('Daily','Weekly'),minutes=c(10,15),start_date='2026-11-02',active=TRUE)
s<-toggle_chore(s,'ch1','2026-11-02');s<-toggle_chore(s,'ch2','2026-11-02')
ok(chore_done(s,'ch1','2026-11-02')&&!chore_done(s,'ch1','2026-11-03')&&chore_done(s,'ch2','2026-11-08')&&!chore_done(s,'ch2','2026-11-09'),'daily and Monday-based weekly chore completion')
s<-toggle_chore(s,'ch1','2026-11-02');ok(!chore_done(s,'ch1','2026-11-02'),'chore completion can be undone independently')
# Calendar activities block the existing scheduler; task movements become manual locks.
s<-demo_state(as.Date('2026-11-02'));s<-calendar_entry(s,'2026-11-04',9,11,'Unavailable','Dentist');g<-capacity_grid(s,TRUE,0,16,as.Date('2026-11-02'));ok(all(g$blocked[interval_indices(3,9,11)]),'one-off activity blocks automatic work')
s<-calendar_entry(s,'2026-11-04',13,14,'Task',task_id='t1');ok(nrow(s$locks)==1,'calendar placement persists a manual work block')
s<-calendar_entry(s,'2026-11-05',14,15,'Task',task_id='t1',replace_id=s$locks$id[1]);ok(nrow(s$locks)==1&&s$locks$date[1]=='2026-11-05','moving an existing lock replaces it without duplication')
ok(throws(calendar_entry(s,'2026-11-05',14,14.123,'Unavailable','Bad slot')),'invalid calendar intervals are rejected')
ok(throws(calendar_entry(s,'2027-01-01',9,10,'Task',task_id='t13')),'fixed deadline cannot be moved past its date')
p<-plan_work(s,'Balanced',as.Date('2026-11-02'),TRUE,0,16,1,as.Date('2026-11-02'));cal<-calendar_ics(s,p,'2026-11-02','2026-11-08','Full schedule');deadlines<-calendar_ics(s,p,'2026-11-02','2026-11-08','Deadlines only','t13')
ok(grepl('BEGIN:VCALENDAR',cal,fixed=TRUE)&&grepl('Dentist',cal,fixed=TRUE)&&grepl('DTSTART:',cal,fixed=TRUE),'full ICS contains timed work and dated commitments')
ok(!grepl('Dentist',deadlines,fixed=TRUE)&&grepl('DTSTART;VALUE=DATE:',deadlines,fixed=TRUE),'filtered deadline export is all-day and excludes work blocks')
ok(all(nchar(strsplit(ics_fold(paste(rep('Ž',100),collapse='')),'\r\n',fixed=TRUE)[[1]],type='bytes')<=75),'ICS line folding respects UTF-8 byte limits')
# Salary savings need explicit attestation; rate/target snapshots survive contract edits.
s<-demo_state(as.Date('2026-11-02'));s$jobs$rate[1]<-25;s<-log_work(s,'t1',12,'2026-11-02');r<-role_week_report(s,'2026-11-02');ok(r$Saved_h[1]==0&&r$Still_needed_h[1]==8,'under target is not automatically counted as saved time')
s<-save_role_review(s,'j1','2026-11-02',TRUE,'Scope finished');r<-role_week_report(s,'2026-11-02');ok(r$Saved_h[1]==8&&r$Time_value[1]==200&&r$Still_needed_h[1]==0,'completed necessary work releases eight hours valued at the reference rate')
s$jobs$rate[1]<-999;s$jobs$fte[1]<-.8;r<-role_week_report(s,'2026-11-02');ok(r$Target_h[1]==20&&r$Time_value[1]==200,'reviewed week retains original target and rate')
s<-log_work(s,'t1',10,'2026-11-03');r<-role_week_report(s,'2026-11-02');ok(r$Over_h[1]==2&&r$Time_value[1]== -50,'fixed salary overwork is a negative time value')
# Paid units and rates are recorded per session, independent of task completion.
s<-demo_state(as.Date('2026-11-02'));s<-log_work(s,'t11',2,'2026-11-03');s<-set_session_details(s,tail(s$logs$id,1),9,6);s$jobs$rate[3]<-100;r<-role_week_report(s,'2026-11-02');ok(r$Units[3]==6&&r$Gross_extra[3]==72,'piecework uses actual item counts and the recorded rate')
s<-log_work(s,'t13',1.5,'2026-11-04');s<-set_session_details(s,tail(s$logs$id,1),8,0);s$jobs$rate[5]<-99;r<-role_week_report(s,'2026-11-02');ok(r$Gross_extra[5]==67.5,'hourly estimate preserves the session rate')
# Timesheet raw data is preserved; separate draft flags/reallocates and exposes overflow.
s<-log_work(s,'t1',2,'2026-11-07');s<-set_session_details(s,tail(s$logs$id,1),21,0);ledger<-actual_timesheet(s,'2026-11-01','2026-11-30');flags<-timesheet_flags(ledger)
ok(any(ledger$Date=='2026-11-07'&ledger$Start=='21:00')&&any(grepl('Weekend',flags$Review)),'recorded timesheet preserves weekend/evening truth and flags it')
draft<-working_window_draft(s,'2026-11-01','2026-11-30');allocated<-draft[draft$Source_entry!=''&draft$Planned_date!='',];ok(all(as.integer(format(as.Date(allocated$Planned_date),'%u'))<=5)&&all(allocated$Start>='07:00'&allocated$End<='17:00'),'planning draft uses weekdays and selected working window')
ok(all(!(allocated$Start<'13:00'&allocated$End>'12:00'))&&any(grepl('Hypothetical',draft$Record_type)),'draft keeps lunch free and labels hypothetical allocations')
actual<-ledger[ledger$Status!='Scheduled; attendance unconfirmed',];ok(abs(sum(draft$Hours[draft$Source_entry!=''])-sum(actual$Hours))<1e-8,'draft accounts for every recorded duration, including overflow')
small<-working_window_draft(s,'2026-11-07','2026-11-08');ok(any(grepl('insufficient',small$Record_type)),'insufficient capacity is listed rather than silently losing hours')
# Deletion does not complete a task and retains metadata until explicit purge.
s<-delete_task(s,'t11');ok(!s$archive$tasks$done[s$archive$tasks$id=='t11']&&actual_hours(s,'t11')==2&&validate_state(s),'delete unfinished task preserves actual logs and report history')
s<-purge_history(s,'tasks','t11');ok(!'t11'%in%record_ids(s,'tasks')&&validate_state(s),'permanent purge also clears orphan session metadata')
# Real app state: start/log, delete, calendar activity, chores, categories and weekly review.
shiny::testServer(server,{
 session$setInputs(week='2026-11-02',buffer=0,cap=12,protect=TRUE)
 session$setInputs(`tasks-start`='t11')
 session$setInputs(`tasks-q_date`='2026-11-03',`tasks-q_start`='09:00',`tasks-q_hours`=2,`tasks-q_units`=4,`tasks-q_remaining`=4,`tasks-q_note`='Translated',`tasks-q_next`='Next pages',`tasks-q_save`=1)
 stopifnot(actual_hours(state(),'t11')==2,state()$session_details$units==4,state()$tasks$units[11]==16)
 session$setInputs(`tasks-delete_card`='t11',`tasks-delete_confirm`=1);stopifnot(!'t11'%in%state()$tasks$id,'t11'%in%state()$archive$tasks$id)
 session$setInputs(`capacity-add_activity`=1)
 session$setInputs(`capacity-entry_kind`='Unavailable',`capacity-entry_title`='Dentist',`capacity-entry_date`='2026-11-04',`capacity-entry_start`='13',`capacity-entry_end`='14',`capacity-entry_save`=1);stopifnot(nrow(state()$exceptions)==1)
 session$setInputs(`routines-hobby_title`='Climbing',`routines-hobby_days`=c('2','4'),`routines-hobby_start`='20:00',`routines-hobby_end`='21:30',`routines-hobby_save`=1);stopifnot(sum(state()$blocks$title=='Climbing')==2)
 session$setInputs(`routines-title`='Bins',`routines-period`='Weekly',`routines-minutes`=10,`routines-start`='2026-11-01',`routines-active`=TRUE,`routines-save`=1);stopifnot(nrow(state()$chores)==1)
 session$setInputs(`chores-toggle`='ch1');stopifnot(chore_done(state(),'ch1',controls$today()))
 session$setInputs(`preferences-label`='Health',`preferences-weight`=.2,`preferences-add`=1);stopifnot('Health'%in%state()$categories$label)
 session$setInputs(`reflection-week`='2026-11-02',`reflection-role`='j1',`reflection-section`='Weekly');session$setInputs(`reflection-complete`=TRUE,`reflection-role_note`='Finished scope',`reflection-review_save`=1);stopifnot(state()$role_reviews$complete[1])
 stopifnot(nzchar(output[['reflection-role_cards']]$html))
 session$setInputs(`reflection-year`=2026,`reflection-month`='11',`reflection-section`='Monthly');stopifnot(nzchar(output[['reflection-week_matrix']]$html),nzchar(output[['reflection-year_table']]))
 d<-state();d$jobs$name[2]<-d$jobs$name[1];state(d);session$flushReact();stopifnot(nzchar(output[['reflection-week_matrix']]$html),nzchar(output[['reflection-month_roles']]))
 stopifnot(validate_state(state()))
})
ok(TRUE,'Shiny start/log, visible delete, calendar, chores, categories and weekly/monthly reflections')
cat(checks,'version 2 checks passed.\n')

# A fresh cloud account has no meetings or roles; it must remain connected and open setup.
fresh_server<-plannerServer;formals(fresh_server)$store<-quote(list(save=function(s,expected_revision)TRUE,label='Test account'))
shiny::testServer(fresh_server,{
 session$setInputs(week=as.character(Sys.Date()),buffer=15,cap=8,protect=TRUE)
 stopifnot(nrow(state()$jobs)==0,needs_guided_setup(state()),identical(task_manager$review_id(),''),!session$isClosed())
 session$setInputs(`capacity-add_activity`=1)
 session$setInputs(`capacity-entry_kind`='Unavailable',`capacity-entry_title`='Dentist at 09:40',`capacity-entry_date`=as.character(Sys.Date()+7),`capacity-entry_start`='09:40',`capacity-entry_end`='10:20',`capacity-entry_save`=1)
 stopifnot(nrow(state()$exceptions)==1,abs(state()$exceptions$start[1]-(9+40/60))<1e-8,!session$isClosed())
 session$setInputs(`routines-hobby_title`='New account hobby',`routines-hobby_days`=c('2','4'),`routines-hobby_start`='20:00',`routines-hobby_end`='21:30',`routines-hobby_save`=1)
 stopifnot(sum(state()$blocks$title=='New account hobby')==2,!session$isClosed())
})
cat('PASS: new account with no roles or meetings stays connected and enters guided setup.\n')
