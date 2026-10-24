# Conditional two-outcome forecasts. These are assumptions, not fitted probabilities.
empty_opportunities <- function()data.frame(id=character(),job_id=character(),name=character(),kind=character(),goal_id=character(),status=character(),deadline=character(),hours=numeric(),p_low=numeric(),p_base=numeric(),p_high=numeric(),income_success=numeric(),income_failure=numeric(),cost_upfront=numeric(),cost_success=numeric(),cost_failure=numeric(),award_total=numeric(),years=numeric(),discount=numeric(),alt_hour=numeric(),exclusive_group=character(),basis=character(),notes=character(),stringsAsFactors=FALSE)
new_opportunity <- function(id,job_id,name,deadline=Sys.Date()+30,kind='Grant') {
 data.frame(id=id,job_id=job_id,name=name,kind=kind,goal_id='',status='Considering',deadline=as.character(deadline),hours=20,p_low=.1,p_base=.2,p_high=.3,income_success=5000,income_failure=0,cost_upfront=0,cost_success=0,cost_failure=0,award_total=0,years=1,discount=.05,alt_hour=25,exclusive_group='',basis='',notes='',stringsAsFactors=FALSE)
}
example_opportunities <- function(s,today) {
 a<-new_opportunity('o1','j2','Research grant · focused call',today+21);a$hours<-14;a$goal_id<-'g7';a$basis<-'Illustrative assumptions; replace with call-specific evidence.';a$award_total<-100000
 b<-new_opportunity('o2','j2','Research grant · ambitious call',today+45);b$hours<-40;b$p_low<-.03;b$p_base<-.08;b$p_high<-.15;b$income_success<-25000;b$award_total<-400000;b$exclusive_group<-'Same research project';a$exclusive_group<-b$exclusive_group
 c<-new_opportunity('o3','j1','Write my own book',today+180,'Book');c$hours<-160;c$p_low<-.05;c$p_base<-.2;c$p_high<-.4;c$income_success<-20000;c$cost_upfront<-1500;c$cost_failure<-500;c$years<-2
 rbind(a,b,c)
}
opportunity_effort <- function(s,o) {
 t<-all_records(s,'tasks');ids<-if(nzchar(o$goal_id))t$id[t$goal_id==o$goal_id]else character()
 active<-s$tasks[s$tasks$id%in%ids,,drop=FALSE]
 # Once a goal has task breakdowns, those tasks are the remaining-work forecast.
 remaining<-if(length(ids))sum(active$remaining[!active$done])else o$hours
 if(length(ids)&&remaining==0){g<-goal_progress(s);i<-match(o$goal_id,g$id);if(!is.na(i)&&g$fraction[i]<1)remaining<-o$hours}
 l<-all_records(s,'logs')
 list(remaining=remaining,actual=sum(l$hours[l$task_id%in%ids]),tasks=length(ids))
}
opportunity_values <- function(o,hours=o$hours) {
 present<-(1+o$discount)^o$years
 win<-(o$income_success-o$cost_success)/present-o$cost_upfront
 lose<-(o$income_failure-o$cost_failure)/present-o$cost_upfront
 p<-c(o$p_low,o$p_base,o$p_high)
 cash<-p*win+(1-p)*lose
 economic<-cash-hours*o$alt_hour
 delta<-win-lose
 break_even<-if(delta<=1e-12)NA_real_ else (hours*o$alt_hour-lose)/delta
 list(cash=cash,economic=economic,per_hour=if(hours>0)economic[2]/hours else NA_real_,win=win,lose=lose,
   loss_probability=o$p_base*as.numeric(o$income_success-o$cost_success-o$cost_upfront<0)+(1-o$p_base)*as.numeric(o$income_failure-o$cost_failure-o$cost_upfront<0),max_cash_loss=max(0,o$cost_upfront+o$cost_success-o$income_success,o$cost_upfront+o$cost_failure-o$income_failure),break_even=break_even)
}
opportunity_report <- function(s,ids=s$opportunities$id) {
 o<-s$opportunities[s$opportunities$id%in%ids,,drop=FALSE]
 if(!nrow(o))return(data.frame())
 do.call(rbind,lapply(seq_len(nrow(o)),function(i){r<-o[i,];eff<-opportunity_effort(s,r);v<-opportunity_values(r,eff$remaining);g<-goal_progress(s);gi<-match(r$goal_id,g$id)
 data.frame(id=r$id,Opportunity=r$name,Role=s$jobs$name[match(r$job_id,s$jobs$id)],Status=r$status,Personal_income_if_successful=r$income_success,Chance_pct=100*r$p_base,Remaining_h=eff$remaining,Logged_h=eff$actual,Expected_cash=v$cash[2],Economic_value=v$economic[2],Economic_value_per_h=v$per_hour,Conservative_value=v$economic[1],Optimistic_value=v$economic[3],Cash_loss_chance_pct=100*v$loss_probability,Maximum_cash_loss=v$max_cash_loss,Break_even_chance_pct=100*v$break_even,Deadline=r$deadline,Goal=if(is.na(gi))'Not linked'else paste(g$title[gi],paste0(round(100*g$fraction[gi]),'%')),Exclusive_group=r$exclusive_group,stringsAsFactors=FALSE)}))
}
validate_opportunities <- function(s) {
 ref<-empty_opportunities()
 for(place in c('current','archive')) {
  d<-if(place=='current')s$opportunities else s$archive$opportunities
  if(!is.data.frame(d)||!identical(names(d),names(ref))||nrow(d)>1000||anyNA(d))stop('Invalid opportunities table.')
  for(k in names(ref))if((is.character(ref[[k]])&&!is.character(d[[k]]))||(is.numeric(ref[[k]])&&(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))))stop('Invalid opportunity values.')
  if(anyDuplicated(d$id)||any(!grepl('^[A-Za-z][A-Za-z0-9_]*$',d$id))||any(!nzchar(trimws(d$name))))stop('Name each opportunity and use unique IDs.')
  if(any(!d$kind%in%c('Grant','Book','Other'))||any(!d$status%in%c('Considering','Pursuing','Submitted','Won','Lost','Withdrawn')))stop('Invalid opportunity type or status.')
  if(any(d$p_low<0|d$p_high>1|d$p_low>d$p_base|d$p_base>d$p_high))stop('Use ordered low ≤ central ≤ high success chances between 0 and 100%.')
  if(any(d$hours<.25|d$hours*4!=round(d$hours*4)))stop('Use at least 0.25 forecast hours, in quarter-hour steps.')
  for(k in c('income_success','income_failure','cost_upfront','cost_success','cost_failure','award_total','years','discount','alt_hour'))if(any(d[[k]]<0))stop('Income, costs, timing and time value must be nonnegative.')
  if(any(d$years>100|d$discount>1))stop('Use a horizon up to 100 years and a discount rate up to 100%.')
  dt<-suppressWarnings(as.Date(d$deadline,format='%Y-%m-%d'));if(anyNA(dt)||any(format(dt,'%Y-%m-%d')!=d$deadline))stop('Invalid opportunity deadline.')
  if(place=='current') {
   if(any(!d$job_id%in%s$jobs$id)||any(nzchar(d$goal_id)&!d$goal_id%in%s$goals$id))stop('An opportunity references a missing work role or goal.')
   linked<-d[nzchar(d$goal_id),,drop=FALSE]
   if(anyDuplicated(linked$goal_id))stop('Use a separate preparation goal for each opportunity to avoid counting the same effort twice.')
   if(nrow(linked)&&any(s$goals$job_id[match(linked$goal_id,s$goals$id)]!=linked$job_id))stop('The opportunity and its goal must belong to the same work role.')
  }
 }
 if(anyDuplicated(c(s$opportunities$id,s$archive$opportunities$id)))stop('Opportunity IDs are shared with removed history.')
 invisible(TRUE)
}
create_opportunity_goal <- function(s,id,today=Sys.Date()) {
 i<-match(id,s$opportunities$id);if(is.na(i))stop('Choose an opportunity first.')
 o<-s$opportunities[i,]
 if(nzchar(o$goal_id))return(list(state=s,goal_id=o$goal_id))
 gid<-next_id('g',record_ids(s,'goals'));tid<-next_id('t',record_ids(s,'tasks'))
 title<-paste(if(o$kind=='Grant')'Submit proposal:'else if(o$kind=='Book')'Finish manuscript:'else'Prepare:',o$name)
 if(as.Date(o$deadline)<as.Date(today))stop('Review the past preparation deadline before creating a new goal.')
 due<-as.Date(o$deadline)
 g<-data.frame(id=gid,job_id=o$job_id,title=title,target=1,baseline=0,unit=if(o$kind=='Grant')'submission'else'deliverable',start=as.character(today),end=as.character(due),stringsAsFactors=FALSE)
 task<-simple_task(tid,o$job_id,title,o$hours,due,hard=o$kind=='Grant');task$goal_id<-gid;task$goal_units<-1;task$billable<-FALSE;task$next_action<-'Break this preparation into concrete milestones if useful.'
 s$goals<-rbind(s$goals,g);s$tasks<-rbind(s$tasks,task);s$opportunities$goal_id[i]<-gid;s$opportunities$status[i]<-'Pursuing'
 # Conditional proceeds are never copied to guaranteed task bonuses.
 validate_state(s);list(state=s,goal_id=gid)
}
