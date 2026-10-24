# Descriptive, evidence-linked feedback; unfinished or unverified work is not a duration outcome.
task_report <- function(s,include_deleted=TRUE) {
  t<-if(include_deleted)all_records(s,'tasks')else s$tasks
  j<-all_records(s,'jobs');logs<-all_records(s,'logs')
  actual<-vapply(t$id,function(id)sum(logs$hours[logs$task_id==id]),numeric(1))
  deleted<-!t$id%in%s$tasks$id;verified<-t$done&t$time_complete
  data.frame(id=t$id,job_id=t$job_id,Task=t$title,Job=j$name[match(t$job_id,j$id)],Status=ifelse(deleted,ifelse(t$done,'Deleted · complete','Deleted · unfinished'),task_status(s)[match(t$id,s$tasks$id)]),Deleted=deleted,Complete=t$done,Verified=verified,Original_h=t$original_estimate,Actual_h=actual,Remaining_h=t$remaining,Error_h=ifelse(verified,actual-t$original_estimate,NA_real_),Ratio=ifelse(verified,actual/t$original_estimate,NA_real_),People=ifelse(nzchar(t$collaborators),t$collaborators,'No one'),Completed=t$completed,Nudges=t$nudges,stringsAsFactors=FALSE)
}
analytics_summary <- function(r) {
  v<-r[r$Verified,,drop=FALSE]
  list(total=nrow(r),complete=sum(r$Complete),open=sum(!r$Complete & !r$Deleted),deleted=sum(r$Deleted),verified=nrow(v),hours=sum(r$Actual_h),median=if(nrow(v))median(v$Actual_h)else NA_real_,ratio=if(nrow(v))median(v$Ratio)else NA_real_,overrun=if(nrow(v))mean(v$Actual_h>v$Original_h+1e-8)else NA_real_)
}
behaviour_recommendations <- function(r) {
  out<-character();v<-r[r$Verified,,drop=FALSE];n<-nrow(v)
  if(n<3)out<-c(out,paste0('Only ',n,' completed tasks have confirmed actual totals. Finish and time at least three before treating estimation patterns as useful guidance.'))
  if(n>=3) {
    ratio<-median(v$Ratio)
    if(ratio>1.15)out<-c(out,paste0('Across ',n,' verified completions, the median actual/estimate ratio is ',round(ratio,2),'×. Try multiplying estimates for similar work by about ',round(ratio,1),' and compare the next few outcomes.'))
    else if(ratio<.85)out<-c(out,paste0('Across ',n,' verified completions, median duration is ',round(100*ratio),'% of the original estimate. For comparable work, try a smaller estimate while keeping your buffer.'))
    else out<-c(out,paste0('Across ',n,' verified completions, the median actual/estimate ratio is ',round(ratio,2),'×. Your overall estimates are fairly close; check individual job patterns before changing them.'))
    if(median(v$Actual_h)>=4)out<-c(out,'Your median completed task takes at least four hours. Try separating comparable work into concrete milestones that fit a focus block; retain the parent goal for the larger outcome.')
  }
  open<-r[!r$Complete & !r$Deleted,,drop=FALSE]
  if(nrow(open)>=10)out<-c(out,paste0('There are ',nrow(open),' open tasks. Choose a small work-in-progress limit for the week and explicitly defer the rest.'))
  overspent<-open$Actual_h>=open$Original_h & open$Actual_h>0
  if(any(overspent))out<-c(out,paste(sum(overspent),'unfinished tasks have already consumed their original estimate. Revisit remaining work or renegotiate scope before adding more.'))
  if(any(open$Nudges>=3))out<-c(out,paste(sum(open$Nudges>=3),'open tasks have at least three nudges. Agree on the next deliverable or an honest revised date with the people involved.'))
  for(j in unique(v$job_id)) {
    z<-v[v$job_id==j,,drop=FALSE]
    if(nrow(z)>=3 && median(z$Ratio)>1.25)out<-c(out,paste0(z$Job[1],': ',nrow(z),' verified tasks have a median ',round(median(z$Ratio),2),'× overrun. Use a job-specific allowance for similar work.'))
  }
  if(!length(out))out<-'Keep logging actual effort and reviewing estimates. Similar tasks and job-specific patterns are more useful than comparing unrelated work.'
  unique(out)
}
job_report <- function(r) {
  ids<-unique(r$job_id)
  if(!length(ids))return(data.frame(Job=character(),Tasks=integer(),Open=integer(),Complete=integer(),Deleted=integer(),Actual_h=numeric(),Verified=integer(),Median_actual_h=numeric(),Median_ratio=numeric()))
  do.call(rbind,lapply(ids,function(id){z<-r[r$job_id==id,,drop=FALSE];v<-z[z$Verified,,drop=FALSE];data.frame(Job=z$Job[1],Tasks=nrow(z),Open=sum(!z$Complete&!z$Deleted),Complete=sum(z$Complete),Deleted=sum(z$Deleted),Actual_h=sum(z$Actual_h),Verified=nrow(v),Median_actual_h=if(nrow(v))median(v$Actual_h)else NA_real_,Median_ratio=if(nrow(v))median(v$Ratio)else NA_real_)}))
}
