# Run from project root: Rscript tests/test-model.R
source('R/design.R');source('R/model.R');source('R/opportunities.R');source('R/planning.R');source('R/demo.R');source('R/history.R')
checks<-0L
check<-function(ok,label){stopifnot(isTRUE(ok));checks<<-checks+1L;cat('PASS:',label,'\n')}
expect_error<-function(expr) inherits(tryCatch({force(expr);NULL},error=identity),'error')
s<-demo_state(as.Date('2026-10-22'));check(validate_state(s),'demo schema and references')
# Overlap union, overnight wrap and protected-event handling.
z<-s;z$availability<-data.frame(id=c('a1','a2'),day=c(1,1),start=c(8,9),end=c(12,11));z$blocks<-z$blocks[FALSE,]
check(sum(capacity_grid(z,buffer=0,daily_cap=24)$usable)/4==4,'overlapping availability counts once')
check(identical(interval_indices(7,23,1),c(669:672,1:4)),'Sunday overnight wraps to Monday')
z$blocks<-data.frame(id=c('b1','b2'),day=c(1,1),title=c('Sleep','Hobby'),kind=c('Essential','Protected'),start=c(8,9),end=c(10,11),job_id=c('',''))
check(sum(capacity_grid(z,TRUE,0,24)$usable)/4==1,'overlapping essential and protected blocks count once')
check(sum(capacity_grid(z,FALSE,0,24)$usable)/4==2,'releasing protected time keeps essentials')
check(sum(capacity_grid(z,FALSE,25,24)$usable)/4==1.5,'buffer removes usable slots once')
e<-task_economics(s)
check(e$gross[e$id=='t5']==0,'extra supervision has zero marginal salary')
check(e$gross[e$id=='t11']==240,'translation uses pages times rate')
check(e$gross[e$id=='t12']==100,'cleaning uses rooms times rate')
check(e$gross[e$id=='t13']==180,'hourly development uses hours times rate')
check(abs(e$net[e$id=='t11']-216)<1e-9,'piecework expenses apply')
check(e$salary_hour[e$id=='t1']==22.5,'salary reference uses FTE contract hours')
a<-complete_task(s,'t4',as.Date('2026-10-22'));p<-goal_progress(a)
check(p$progress[p$id=='g3']==5,'finishing credits goal units')
check(goal_progress(complete_task(a,'t4',as.Date('2026-10-22')))$progress[p$id=='g3']==5,'completion is idempotent')
check(goal_progress(reopen_task(a,'t4'))$progress[p$id=='g3']==0,'reopening removes goal credit')
check(goal_progress(complete_task(s,'t4',as.Date('2027-01-01')))$progress[p$id=='g3']==0,'goal periods exclude later completions')
check(rank_tasks(s,'Cash now',as.Date('2026-10-22'))$id[1]=='t13','critical deadline overrides cash preference')
check(rank_tasks(s,'Random',as.Date('2026-10-22'),42)$id[1]=='t13','random keeps critical guardrail')
check(identical(rank_tasks(s,'Random',as.Date('2026-10-22'),42)$id,rank_tasks(s,'Random',as.Date('2026-10-22'),42)$id),'random strategy is reproducible')
check(rank_tasks(s)$blocked[match('t17',rank_tasks(s)$id)],'dependent task stays blocked')
for(mode in c('Balanced','Urgency','Consequences','Cash now','Contract balance','Personal meaning','Social impact','Career','Random','Quick wins','One big + small')) {
  out<-schedule_week(s,mode,as.Date('2026-10-19'),today=as.Date('2026-10-19'));p<-out$plan
  check(sum(p$hours)<=sum(out$grid$usable)/4,paste(mode,'respects total capacity'))
  occupied<-integer()
  for(i in seq_len(nrow(p))) {
    ix<-interval_indices(p$day[i],p$start[i],p$end[i]);check(all(out$grid$usable[ix]),'scheduled time is usable');check(!any(ix%in%occupied),'no schedule overlap');occupied<-c(occupied,ix)
    r<-s$tasks[match(p$task_id[i],s$tasks$id),]
    check(!r$hard || as.Date(p$date[i])<=effective_due(r),'fixed deadline respected')
  }
  check(!'t17'%in%p$task_id,'uncompleted prerequisites never scheduled')
  hours<-tapply(p$hours,p$task_id,sum);check(all(hours<=s$tasks$remaining[match(names(hours),s$tasks$id)]+1e-9),'task effort never overscheduled')
}
z<-s;z$tasks$depends[1]<-'t17';z$tasks$depends[17]<-'t1';check(expect_error(validate_state(z)),'dependency cycles rejected')
z<-s;z$tasks$remaining[1]<-NaN;check(expect_error(validate_state(z)),'nonfinite imports rejected')
z<-s;z$allocations$fte[1]<-.9;check(expect_error(validate_state(z)),'contract area FTE cannot exceed job FTE')
z<-s;z$blocks$start[1]<-1.1;check(expect_error(validate_state(z)),'off-grid commitments rejected')
o<-opportunity_scenario(.2,1,10000,2,.05,0,0,0);check(abs(o$net)<1e-9,'no probability uplift yields no benefit')
o<-opportunity_scenario(.2,2,10000,0,0,0,0,0);check(abs(o$p_after-1/3)<1e-9,'odds conversion correct')
o<-opportunity_scenario(.2,2,10000,0,0,0,40,25);check(abs(o$net-(10000*(1/3-.2)-1000))<1e-9,'opportunity cost deducted')
if(requireNamespace('jsonlite',quietly=TRUE)) {
  source('R/ui_helpers.R');f<-tempfile(fileext='.json');jsonlite::write_json(s,f,auto_unbox=TRUE,dataframe='columns');a<-read_backup(f);check(validate_state(a),'JSON roundtrip with empty log table');unlink(f)
  z<-s;for(n in c('tasks','goals','allocations','logs','opportunities'))z[[n]]<-z[[n]][FALSE,];f<-tempfile();jsonlite::write_json(z,f,auto_unbox=TRUE,dataframe='columns');check(validate_state(read_backup(f)),'empty workspace JSON roundtrip');unlink(f)
}
# Contract balance must spread planned effort, even before actual logs exist.
z<-s;z$jobs<-s$jobs[1:2,];z$allocations<-z$allocations[FALSE,];z$blocks<-z$blocks[FALSE,];z$tasks<-s$tasks[1:2,];z$tasks$job_id<-c('j1','j2');z$tasks$total<-40;z$tasks$remaining<-40;z$tasks$hard<-FALSE;z$tasks$goal_id<-'';z$tasks$due<-'2026-11-30';z$availability<-data.frame(id='a1',day=1,start=8,end=16)
p<-schedule_week(z,'Contract balance',as.Date('2026-10-19'),buffer=0,today=as.Date('2026-10-19'))$plan
check(all(tapply(p$hours,p$job_id,sum)==4),'contract balance updates with planned work')
z$blocks<-data.frame(id=c('b1','b2'),day=c(1,1),title=c('Meeting A','Meeting B'),kind=c('Meeting','Meeting'),start=c(9,9.5),end=c(10,10.5),job_id=c('j1','j2'))
check(calendar_conflicts(z)$overlap_h==.5,'overlapping mandatory meetings are flagged')
set.seed(123);rng_before<-.Random.seed;invisible(rank_tasks(s,'Random',as.Date('2026-10-22'),5));check(identical(.Random.seed,rng_before),'random ranking preserves session RNG state')
cat('\n',checks,'checks passed.\n')
