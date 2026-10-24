# Small, predictable defaults for the guided setup. No UI dependencies.
blank_state <- function(today=Sys.Date()) {
  s<-demo_state(today)
  for(n in c('jobs','allocations','tasks','goals','availability','blocks','logs','opportunities')) s[[n]]<-s[[n]][FALSE,,drop=FALSE]
  s
}
simple_job <- function(id,name,pay_type='Salary',hours=10,amount=0,period='Weekly',rate=0,unit='unit',self_employed=FALSE,icon='briefcase',fte=0,expenses=0) {
  data.frame(id=id,name=trimws(name),icon=icon,engagement=if(self_employed)'Self-employed' else 'Employee',pay_type=pay_type,fte=fte,target=hours,weekly_income=if(pay_type=='Salary') amount*if(period=='Monthly')12/52 else 1 else 0,rate=rate,unit=unit,expense_pct=expenses,notes='',stringsAsFactors=FALSE)
}
simple_task <- function(id,job,title,hours=1,due=Sys.Date()+7,hard=FALSE,importance='Normal',units=0) {
  data.frame(id=id,job_id=job,title=trimws(title),area='',total=hours,remaining=hours,due=as.character(due),hard=hard,extension=0,nudges=0,consequence=if(importance=='Critical')5 else if(importance=='Important')4 else 2,personal=if(importance=='Personally meaningful')5 else 2,social=2,career=2,min_block=min(.5,hours),units=units,bonus=0,cash_cost=0,goal_id='',goal_units=0,depends='',done=FALSE,completed='',original_estimate=hours,collaborators='',created=as.character(Sys.Date()),time_complete=FALSE,blocked_reason='',started=FALSE,paused=FALSE,next_action='',billable=TRUE,stringsAsFactors=FALSE)
}
setup_state <- function(config,jobs,tasks,events) {
  s<-blank_state(config$today %||% Sys.Date());s$jobs<-jobs;s$tasks<-tasks;s$tasks$created<-rep(as.character(config$today %||% Sys.Date()),nrow(tasks));s$blocks<-events
  if(!length(config$days)) stop('Choose at least one day you can work.',call.=FALSE)
  s$settings<-list(currency=config$currency,timezone=config$timezone,fulltime=40,setup_complete=TRUE,example_mode=FALSE)
  add<-function(day,title,kind,start,end) {
    s$blocks<<-rbind(s$blocks,data.frame(id=next_id('b',s$blocks$id),day=day,title=title,kind=kind,start=start,end=end,job_id='',stringsAsFactors=FALSE))
  }
  for(d in config$days) s$availability<-rbind(s$availability,data.frame(id=next_id('a',s$availability$id),day=as.numeric(d),start=config$start,end=config$end,stringsAsFactors=FALSE))
  for(d in 1:7) {
    add(d,'Sleep','Essential',config$sleep_start,config$sleep_end)
    # Meal breaks follow the wake period, including night-shift schedules.
    wake<-config$sleep_end%%24
    if(isTRUE(config$meals))for(offset in c(.5,5,10)) {
      a<-(wake+offset)%%24;add(d,'Food & a pause','Essential',a,(a+.5)%%24)
    }
    if(isTRUE(config$breaks))for(offset in c(3,8)) {
      a<-(wake+offset)%%24;add(d,'Bathroom & movement','Essential',a,(a+.25)%%24)
    }
  }
  validate_state(s);s
}
