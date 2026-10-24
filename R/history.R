# Schema migration, durable single-user storage, reversible removal, and actual-time accounting.
record_tables <- c('jobs','allocations','tasks','goals','availability','blocks','logs','opportunities')
migrate_state <- function(s) {
  version<-as.integer(s$schema)
  if(length(version)!=1 || !version%in%c(1L,2L,3L,4L))stop('Unsupported backup version.',call.=FALSE)
  if(!is.data.frame(s$tasks))stop('Invalid tasks table.',call.=FALSE)
  n<-nrow(s$tasks)
  defaults<-list(original_estimate=s$tasks$total,collaborators=rep('',n),created=rep('',n),time_complete=rep(FALSE,n),blocked_reason=rep('',n),started=rep(FALSE,n),paused=rep(FALSE,n),next_action=rep('',n),billable=rep(TRUE,n))
  for(col in names(defaults))if(!col%in%names(s$tasks))s$tasks[[col]]<-defaults[[col]]
  if(is.null(s$opportunities))s$opportunities<-empty_opportunities()
  if(is.null(s$archive))s$archive<-list()
  for(nm in record_tables) {
    if(is.null(s$archive[[nm]]))s$archive[[nm]]<-s[[nm]][FALSE,,drop=FALSE]
  }
  for(place in c('current','archive')) {
    t<-if(place=='current')s$tasks else s$archive$tasks;n<-nrow(t)
    for(col in c('started','paused'))if(!col%in%names(t))t[[col]]<-rep(FALSE,n)
    if(!'next_action'%in%names(t))t$next_action<-rep('',n)
    if(!'billable'%in%names(t))t$billable<-rep(TRUE,n)
    l<-if(place=='current')s$logs else s$archive$logs
    for(col in c('note','created_at','updated_at'))if(!col%in%names(l))l[[col]]<-rep('',nrow(l))
    if(place=='current'){s$tasks<-t;s$logs<-l}else{s$archive$tasks<-t;s$archive$logs<-l}
  }
  if(is.null(s$exceptions))s$exceptions<-empty_exceptions()
  if(is.null(s$locks))s$locks<-empty_locks()
  if(is.null(s$revision))s$revision<-0L
  if(is.null(s$log_sequence))s$log_sequence<-max(c(0,suppressWarnings(as.numeric(sub('^l','',c(s$logs$id,s$archive$logs$id))))),na.rm=TRUE)
  # Legacy columns were appended during earlier migrations. Canonical ordering
  # keeps active and archived records compatible without dropping any fields.
  template<-demo_state(as.Date('2026-01-05'))
  for(nm in record_tables)for(place in c('current','archive')) {
    d<-if(place=='current')s[[nm]] else s$archive[[nm]]
    cols<-names(template[[nm]])
    if(setequal(names(d),cols))d<-d[,cols,drop=FALSE]
    if(place=='current')s[[nm]]<-d else s$archive[[nm]]<-d
  }
  s$schema<-4L;s
}
all_records <- function(s,table) {
  archived<-s$archive[[table]]
  if(is.null(archived)||!nrow(archived))return(s[[table]])
  rbind(s[[table]],archived)
}
record_ids <- function(s,table)all_records(s,table)$id
append_archive <- function(s,table,ids) {
  rows<-s[[table]][s[[table]]$id%in%ids,,drop=FALSE]
  if(nrow(rows))s$archive[[table]]<-rbind(s$archive[[table]],rows)
  s[[table]]<-s[[table]][!s[[table]]$id%in%ids,,drop=FALSE];s
}
delete_task <- function(s,id) {
  if(!id%in%s$tasks$id)return(s)
  affected<-s$tasks$depends==id
  s$tasks$depends[affected]<-'';s$tasks$blocked_reason[affected]<-'Prerequisite deleted. Review this task before scheduling it.'
  s<-append_archive(s,'logs',s$logs$id[s$logs$task_id==id])
  s$locks<-s$locks[s$locks$task_id!=id,,drop=FALSE]
  append_archive(s,'tasks',id)
}
delete_job <- function(s,id) {
  if(!id%in%s$jobs$id)return(s)
  s$exceptions<-s$exceptions[s$exceptions$job_id!=id,,drop=FALSE]
  for(tid in s$tasks$id[s$tasks$job_id==id])s<-delete_task(s,tid)
  for(nm in c('opportunities','goals','blocks','allocations'))s<-append_archive(s,nm,s[[nm]]$id[s[[nm]]$job_id==id])
  s<-append_archive(s,'logs',s$logs$id[s$logs$job_id==id]);append_archive(s,'jobs',id)
}
delete_goal <- function(s,id) {
  s$opportunities$goal_id[s$opportunities$goal_id==id]<-''
  hit<-s$tasks$goal_id==id;s$tasks$goal_id[hit]<-'';s$tasks$goal_units[hit]<-0
  append_archive(s,'goals',id)
}
actual_hours <- function(s,id) {
  logs<-all_records(s,'logs');sum(logs$hours[logs$task_id==id])
}
log_work <- function(s,id,hours,date=Sys.Date(),remaining=NULL,note='') {
  i<-match(id,s$tasks$id)
  if(is.na(i)||s$tasks$done[i])stop('Choose an unfinished task.',call.=FALSE)
  if(length(hours)!=1||!is.finite(hours)||hours<=0)stop('Enter positive actual hours.',call.=FALSE)
  if(is.null(remaining))remaining<-s$tasks$remaining[i]
  if(length(remaining)!=1||!is.finite(remaining)||remaining<.25||abs(remaining*4-round(remaining*4))>1e-8)stop('Set at least 0.25 h remaining, in quarter-hour steps. Use Finish when it is done.',call.=FALSE)
  s$log_sequence<-max(c(s$log_sequence %||% 0,suppressWarnings(as.numeric(sub('^l','',record_ids(s,'logs'))))),na.rm=TRUE)+1
  s$logs<-rbind(s$logs,data.frame(id=paste0('l',format(s$log_sequence,scientific=FALSE,trim=TRUE)),task_id=id,job_id=s$tasks$job_id[i],date=as.character(date),hours=hours,note=as.character(note),created_at=utc_stamp(),updated_at=utc_stamp(),stringsAsFactors=FALSE))
  s$tasks$started[i]<-TRUE
  s$tasks$remaining[i]<-remaining;s$tasks$total[i]<-max(s$tasks$total[i],remaining)
  s
}
finish_with_actual <- function(s,id,total_actual,date=Sys.Date()) {
  i<-match(id,s$tasks$id)
  if(is.na(i))stop('Task no longer exists.',call.=FALSE)
  if(s$tasks$done[i])return(s)
  spent<-actual_hours(s,id)
  if(length(total_actual)!=1||!is.finite(total_actual)||total_actual<0)stop('Enter total actual hours, including any work already logged.',call.=FALSE)
  if(total_actual+1e-8<spent)stop('Total is below your existing time log. Correct the log in Reflect first.',call.=FALSE)
  delta<-total_actual-spent
  if(delta>1e-8)s<-log_work(s,id,delta,date,remaining=s$tasks$remaining[i])
  s<-complete_task(s,id,date);s$tasks$time_complete[i]<-TRUE;s
}
utc_stamp <- function()format(Sys.time(),tz='UTC',format='%Y-%m-%dT%H:%M:%SZ')
edit_work_session <- function(s,id,hours,date,note='') {
  if(length(hours)!=1||!is.finite(hours)||hours<=0)stop('Actual hours must be finite and positive.')
  archived<-!id%in%s$logs$id;l<-if(archived)s$archive$logs else s$logs;i<-match(id,l$id)
  if(is.na(i))stop('Session no longer exists.')
  l$hours[i]<-hours;l$date[i]<-as.character(date);l$note[i]<-as.character(note);l$updated_at[i]<-utc_stamp()
  if(archived)s$archive$logs<-l else s$logs<-l
  validate_state(s);s
}
start_focus <- function(s,id) {
  i<-match(id,s$tasks$id);if(is.na(i)||s$tasks$done[i])stop('Choose an unfinished task.')
  r<-rank_tasks(s);if(isTRUE(r$blocked[match(id,r$id)]))stop('Resolve the task dependency or blocked reason first.')
  s$tasks$paused[s$tasks$started & !s$tasks$done]<-TRUE
  s$tasks$started[i]<-TRUE;s$tasks$paused[i]<-FALSE;s
}
pause_focus <- function(s,id) {i<-match(id,s$tasks$id);if(!is.na(i))s$tasks$paused[i]<-TRUE;s}
task_status <- function(s) {
  t<-s$tasks;l<-all_records(s,'logs');logged<-vapply(t$id,function(id)sum(l$hours[l$task_id==id]),numeric(1))
  blocked<-nzchar(t$blocked_reason)|vapply(t$depends,function(id)nzchar(id)&&!isTRUE(t$done[match(id,t$id)]),logical(1))
  ifelse(t$done,'Completed',ifelse(blocked,'Blocked',ifelse(t$started|logged>0,'In progress','Not started')))
}
resume_context <- function(s) {
  t<-s$tasks;status<-task_status(s);ids<-which(status=='In progress');l<-all_records(s,'logs')
  if(!length(ids))return(data.frame(id=character(),title=character(),last_date=character(),last_hours=numeric(),total=numeric(),remaining=numeric(),note=character(),next_action=character(),paused=logical()))
  do.call(rbind,lapply(ids,function(i){z<-l[l$task_id==t$id[i],,drop=FALSE];if(nrow(z))z<-z[order(z$date,z$updated_at,seq_len(nrow(z)),decreasing=TRUE),,drop=FALSE]
    data.frame(id=t$id[i],title=t$title[i],last_date=if(nrow(z))z$date[1]else'',last_hours=if(nrow(z))z$hours[1]else 0,total=sum(z$hours),remaining=t$remaining[i],note=if(nrow(z))z$note[1]else'',next_action=t$next_action[i],paused=t$paused[i],stringsAsFactors=FALSE)}))
}
save_state <- function(s,path,expected_revision=NULL) {
  if(!nzchar(path))return(invisible(FALSE))
  validate_state(s);dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  if(file.exists(path)) {
    old<-read_backup(path)
    if(!is.null(expected_revision)&&old$revision!=expected_revision)stop('This plan changed in another session. Reload the saved file before saving; your current session has been kept.')
    raw<-jsonlite::fromJSON(path,simplifyVector=FALSE)
    if(as.integer(raw$schema)<4){migration<-paste0(path,'.schema',raw$schema,'.backup.json');if(!file.exists(migration)&&!file.copy(path,migration))stop('Could not preserve the pre-migration backup.')}
    if(!file.copy(path,paste0(path,'.previous.json'),overwrite=TRUE))stop('Could not preserve the previous good save.')
  } else if(!is.null(expected_revision)&&expected_revision!=0L)stop('The saved file is missing; refusing to overwrite a stale session.')
  tmp<-tempfile('worthwhile-',tmpdir=dirname(path),fileext='.json');on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  jsonlite::write_json(s,tmp,pretty=TRUE,auto_unbox=TRUE,dataframe='columns')
  if(!file.rename(tmp,path))stop('Could not replace the save. Previous data and backup were retained.')
  invisible(TRUE)
}
purge_history <- function(s,kind,id) {
  if(!kind%in%c('tasks','jobs','goals','opportunities')||!id%in%s$archive[[kind]]$id)stop('Choose a deleted record to remove from history.',call.=FALSE)
  if(kind=='jobs') {
    tids<-s$archive$tasks$id[s$archive$tasks$job_id==id]
    for(tid in tids)s<-purge_history(s,'tasks',tid)
    for(nm in c('opportunities','goals','blocks','allocations','logs'))s$archive[[nm]]<-s$archive[[nm]][s$archive[[nm]]$job_id!=id,,drop=FALSE]
  }
  if(kind=='tasks')s$archive$logs<-s$archive$logs[s$archive$logs$task_id!=id,,drop=FALSE]
  s$archive[[kind]]<-s$archive[[kind]][s$archive[[kind]]$id!=id,,drop=FALSE]
  s
}
