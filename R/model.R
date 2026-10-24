# Pure functions: no Shiny dependencies. Time is a recurring local wall-clock week.
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
strategies <- c('Balanced','Urgency','Consequences','Cash now','Contract balance','Personal meaning','Social impact','Career','Random','Quick wins','One big + small')
week_start <- function(x = Sys.Date()) as.Date(x) - (as.integer(format(as.Date(x), '%u')) - 1L)
num <- function(x) suppressWarnings(as.numeric(x))
clamp <- function(x, lo = 0, hi = 1) pmin(hi, pmax(lo, x))
next_id <- function(prefix, ids) {
  i <- 1L
  while (paste0(prefix, i) %in% ids) i <- i + 1L
  paste0(prefix, i)
}
empty_plan <- function() data.frame(task_id=character(), job_id=character(), title=character(), date=character(), day=integer(), start=numeric(), end=numeric(), hours=numeric(), stringsAsFactors=FALSE)
active_tasks <- function(s) s$tasks[!s$tasks$done & s$tasks$remaining > 0, , drop=FALSE]
effective_due <- function(t) as.Date(t$due) + ifelse(t$hard, 0, t$extension)
# Intervals are half open; crossing midnight wraps into the next weekday.
interval_indices <- function(day, start, end) {
  a <- (as.integer(day)-1L)*96L + round(start*4)
  b <- (as.integer(day)-1L)*96L + round(end*4)
  if (end <= start) b <- b+96L
  if (b <= a) return(integer())
  ((seq.int(a,b-1L)) %% 672L)+1L
}
capacity_grid <- function(s, protect=TRUE, buffer=15, daily_cap=10,anchor=NULL) {
  g <- data.frame(day=rep(1:7,each=96), start=rep(seq(0,23.75,.25),7), available=FALSE, blocked=FALSE, reserved=FALSE, label='', kind='', job_id='', stringsAsFactors=FALSE)
  for(i in seq_len(nrow(s$availability))) {
    r <- s$availability[i,]; g$available[interval_indices(r$day,r$start,r$end)] <- TRUE
  }
  for(i in seq_len(nrow(s$blocks))) {
    r <- s$blocks[i,]; ix <- interval_indices(r$day,r$start,r$end)
    g$label[ix] <- ifelse(nzchar(g$label[ix]), paste(g$label[ix],r$title,sep=' / '),r$title)
    g$kind[ix] <- r$kind; g$job_id[ix] <- r$job_id
    if(r$kind != 'Protected' || protect) g$blocked[ix] <- TRUE
  }
  g<-apply_dated_grid(g,s,anchor)
  g$free <- g$available & !g$blocked
  for(d in 1:7) {
    ix <- which(g$day==d & g$free)
    usable <- min(floor(length(ix)*(1-buffer/100)), floor(daily_cap*4))
    if(length(ix)>usable) g$reserved[tail(ix,length(ix)-usable)] <- TRUE
  }
  g$usable <- g$free & !g$reserved
  g
}
job_targets <- function(s, fulltime=40) ifelse(s$jobs$fte>0,s$jobs$fte*fulltime,s$jobs$target)
job_meeting_hours <- function(s, protect=TRUE) {
  vapply(s$jobs$id, function(j) {
    z <- s$blocks[s$blocks$kind=='Meeting' & s$blocks$job_id==j,,drop=FALSE]
    ix <- integer()
    for(i in seq_len(nrow(z))) ix <- union(ix,interval_indices(z$day[i],z$start[i],z$end[i]))
    length(ix)/4
  }, numeric(1))
}
calendar_conflicts <- function(s,protect=TRUE) {
  b <- s$blocks[s$blocks$kind!='Protected' | protect,,drop=FALSE]
  out <- data.frame(first=character(),second=character(),overlap_h=numeric(),stringsAsFactors=FALSE)
  if(nrow(b)<2) return(out)
  for(i in seq_len(nrow(b)-1L)) for(k in seq.int(i+1L,nrow(b))) {
    if(!'Meeting'%in%c(b$kind[i],b$kind[k])) next
    h <- length(intersect(interval_indices(b$day[i],b$start[i],b$end[i]),interval_indices(b$day[k],b$start[k],b$end[k])))/4
    if(h>0) out<-rbind(out,data.frame(first=b$title[i],second=b$title[k],overlap_h=h,stringsAsFactors=FALSE))
  }
  out
}
task_economics <- function(s) {
  t <- s$tasks; j <- s$jobs[match(t$job_id,s$jobs$id),,drop=FALSE]
  gross <- ifelse(t$billable,ifelse(j$pay_type=='Hourly',t$remaining*j$rate,
                  ifelse(j$pay_type=='Piece',t$units*j$rate,0)),0) + t$bonus
  gross[t$done]<-0
  net <- gross*(1-j$expense_pct/100)-ifelse(t$done,0,t$cash_cost)
  salary_rate <- ifelse(j$pay_type=='Salary',j$weekly_income/pmax(.25,ifelse(j$fte>0,j$fte*s$settings$fulltime,j$target)),0)
  data.frame(id=t$id, gross=gross, net=net, net_hour=net/pmax(.25,t$remaining), salary_hour=salary_rate, stringsAsFactors=FALSE)
}
rank_tasks <- function(s, strategy='Balanced', today=Sys.Date(), seed=1, balance_week=week_start(today), planned=empty_plan()) {
  t <- active_tasks(s)
  if(!nrow(t)) return(t)
  e <- task_economics(s); e <- e[match(t$id,e$id),]
  due <- effective_due(t); days <- as.numeric(due-as.Date(today))
  blocked <- vapply(t$depends, function(dep) nzchar(dep) && !isTRUE(s$tasks$done[match(dep,s$tasks$id)]),logical(1))
  blocked <- blocked | nzchar(t$blocked_reason)
  t$blocked <- blocked
  # Critical obligations always precede preference scores, but remain visible when blocked.
  t$tier <- ifelse(t$consequence==5 | (t$hard & days<=2),2,ifelse(t$hard & days<=7,1,0))
  urgency <- clamp((8-days)/8) + clamp(t$nudges/5)
  risk <- t$consequence/5
  cash <- clamp(pmax(0,e$net_hour)/max(1,pmax(0,e$net_hour)))
  targets <- job_targets(s,s$settings$fulltime)
  log_rows<-all_records(s,'logs')
  hist <- log_rows[log_rows$date>=as.character(balance_week) & log_rows$date<=as.character(balance_week+6),,drop=FALSE]
  worked <- vapply(s$jobs$id,function(j) sum(hist$hours[hist$job_id==j])+sum(planned$hours[planned$job_id==j]),numeric(1)) + job_meeting_hours(s)
  gap <- clamp((targets-worked)/pmax(.25,targets))
  fair <- gap[match(t$job_id,s$jobs$id)]
  for(i in seq_len(nrow(t))) {
    a <- s$allocations[s$allocations$job_id==t$job_id[i] & s$allocations$name==t$area[i],,drop=FALSE]
    if(nrow(a)==1 && a$fte>0) {
      ids <- s$tasks$id[s$tasks$job_id==t$job_id[i] & s$tasks$area==t$area[i]]
      area_hours <- sum(hist$hours[hist$task_id%in%ids])+sum(planned$hours[planned$task_id%in%ids])
      fair[i] <- .5*fair[i]+.5*clamp(1-area_hours/(a$fte*s$settings$fulltime))
    }
  }
  small <- 1/pmax(.25,t$remaining)
  score <- switch(strategy,
    'Urgency'=urgency,
    'Consequences'=risk,
    'Cash now'=cash,
    'Contract balance'=fair,
    'Personal meaning'=t$personal/5,
    'Social impact'=t$social/5,
    'Career'=t$career/5,
    'Quick wins'=small,
    'Random'={
      had_seed <- exists('.Random.seed',.GlobalEnv,inherits=FALSE)
      if(had_seed) old_seed <- get('.Random.seed',.GlobalEnv)
      on.exit(if(had_seed) assign('.Random.seed',old_seed,.GlobalEnv) else if(exists('.Random.seed',.GlobalEnv,inherits=FALSE)) rm('.Random.seed',envir=.GlobalEnv),add=TRUE)
      set.seed(as.integer(seed));runif(nrow(t))
    },
    .25*urgency+.2*risk+.15*cash+.1*fair+.15*t$personal/5+.1*t$career/5+.05*t$social/5)
  t$urgency_factor<-urgency;t$risk_factor<-risk;t$cash_factor<-cash;t$balance_factor<-fair
  t$score <- round(score,4); t$net_hour <- e$net_hour; t$salary_hour <- e$salary_hour
  t$why <- paste0('Due ',due,ifelse(t$hard,' (fixed)',' (flexible)'),'; nudges ',t$nudges,'; consequence ',t$consequence,'/5; cash ',round(e$net_hour,1),'/h',ifelse(blocked,'; waiting on dependency',''))
  t[order(t$blocked,-t$tier,-t$score,days,t$id),,drop=FALSE]
}
# Transparent greedy heuristic. It never claims a globally optimal income schedule.
schedule_week <- function(s,strategy='Balanced',anchor=week_start(),protect=TRUE,buffer=15,daily_cap=10,seed=1,today=Sys.Date(),locked=empty_plan()) {
  g <- capacity_grid(s,protect,buffer,daily_cap,anchor)
  t <- rank_tasks(s,strategy,today,seed,balance_week=as.Date(anchor)); plan <- locked
  if(!nrow(t)) return(list(plan=plan,grid=g,unscheduled=t))
  left <- setNames(t$remaining,t$id)
  occupied<-integer()
  for(i in seq_len(nrow(locked))) {left[[locked$task_id[i]]]<-max(0,left[[locked$task_id[i]]]-locked$hours[i]);occupied<-union(occupied,interval_indices(locked$day[i],locked$start[i],locked$end[i]))}
  for(d in 1:7) {
    date <- as.Date(anchor)+d-1L
    if(date<as.Date(today)) next
    ix <- setdiff(which(g$day==d & g$usable),occupied)
    # Never schedule elapsed quarter hours on the current date.
    if(date==as.Date(today) && as.Date(today)==as.Date(format(Sys.time(),tz=s$settings$timezone,format='%Y-%m-%d'))) {
      now <- as.POSIXlt(Sys.time(),tz=s$settings$timezone)
      ix <- ix[g$start[ix]>=ceiling((now$hour+now$min/60)*4)/4]
    }
    big_used <- FALSE
    while(length(ix)) {
      if(strategy=='Contract balance') t <- rank_tasks(s,strategy,today,seed,balance_week=as.Date(anchor),planned=plan)
      start_ix <- ix[1]; contiguous <- start_ix
      while((tail(contiguous,1)+1L) %in% ix) contiguous <- c(contiguous,tail(contiguous,1)+1L)
      selected <- NA_integer_; nslots <- 0L
      for(k in seq_len(nrow(t))) {
        r <- t[k,]; remain <- left[[r$id]]
        if(r$blocked || remain<=0 || (r$hard && effective_due(r)<date)) next
        if(strategy=='One big + small' && big_used && remain>1) next
        if(strategy=='One big + small' && !big_used && any(left[t$id]>=2 & !t$blocked & (!t$hard | effective_due(t)>=date) & t$min_block<=length(contiguous)/4) && remain<2) next
        need <- min(r$min_block,remain)
        if(length(contiguous)/4+1e-9<need) next
        selected <- k
        nslots <- min(length(contiguous),ceiling(remain*4),if(strategy=='One big + small' && remain>=2) 12L else 8L)
        nslots <- max(nslots,ceiling(need*4))
        nslots <- min(nslots,length(contiguous))
        break
      }
      if(is.na(selected)) {ix <- setdiff(ix,contiguous);next}
      r <- t[selected,]; used <- contiguous[seq_len(nslots)]; h <- nslots/4
      plan <- rbind(plan,data.frame(task_id=r$id,job_id=r$job_id,title=r$title,date=as.character(date),day=d,start=g$start[used[1]],end=g$start[tail(used,1)]+.25,hours=h,stringsAsFactors=FALSE))
      left[[r$id]] <- max(0,left[[r$id]]-h)
      if(h>=2) big_used <- TRUE
      ix <- setdiff(ix,used)
    }
  }
  t$unplanned <- unname(left[t$id]); list(plan=plan,grid=g,unscheduled=t)
}
complete_task <- function(s,id,date=Sys.Date()) {
  i <- match(id,s$tasks$id)
  if(is.na(i) || s$tasks$done[i]) return(s)
  s$tasks$done[i] <- TRUE; s$tasks$remaining[i] <- 0; s$tasks$completed[i] <- as.character(date)
  s
}
reopen_task <- function(s,id) {
  i <- match(id,s$tasks$id)
  if(!is.na(i)) {s$tasks$done[i]<-FALSE;s$tasks$remaining[i]<-s$tasks$total[i];s$tasks$completed[i]<-'';s$tasks$time_complete[i]<-FALSE}
  s
}
goal_progress <- function(s) {
  g <- s$goals
  historical_tasks<-all_records(s,'tasks')
  g$progress <- vapply(seq_len(nrow(g)),function(i) {
    hit <- historical_tasks$done & historical_tasks$goal_id==g$id[i] & historical_tasks$completed>=g$start[i] & historical_tasks$completed<=g$end[i]
    g$baseline[i]+sum(historical_tasks$goal_units[hit])
  },numeric(1))
  g$fraction <- clamp(g$progress/pmax(.001,g$target));g
}
opportunity_scenario <- function(base,odds_multiplier,benefit,years,discount,cost,hours,opportunity_hour) {
  # Hypothetical odds multiplier, not a causal estimate from publication counts.
  p <- clamp(base,.000001,.999999)
  p_after <- (p*odds_multiplier)/(1-p+p*odds_multiplier)
  pv <- benefit/(1+discount)^years
  uplift <- (p_after-p)*pv
  data.frame(base=base,p_after=p_after,uplift=uplift,net=uplift-cost-hours*opportunity_hour)
}
validate_state <- function(s) {
  fail <- function(msg) stop(msg,call.=FALSE)
  if(!is.list(s) || !identical(as.integer(s$schema),4L)) fail('Unsupported backup version.')
  template <- demo_state(as.Date('2026-01-05'))
  for(nm in c('jobs','allocations','tasks','goals','availability','blocks','logs')) {
    d <- s[[nm]]; ref <- template[[nm]]
    if(!is.data.frame(d) || !all(names(ref)%in%names(d)) || nrow(d)>10000) fail(paste('Invalid table:',nm))
    if(anyNA(d)) fail(paste('Missing values in',nm))
    for(col in names(ref)) {
      if(is.numeric(ref[[col]]) && (!is.numeric(d[[col]]) || any(!is.finite(d[[col]])))) fail(paste('Invalid numeric column:',nm,col))
      if(is.logical(ref[[col]]) && !is.logical(d[[col]])) fail(paste('Invalid logical column:',nm,col))
      if(is.character(ref[[col]]) && !is.character(d[[col]])) fail(paste('Invalid text column:',nm,col))
    }
    if('id'%in%names(d) && (anyDuplicated(d$id) || any(!grepl('^[A-Za-z][A-Za-z0-9_]*$',d$id)))) fail(paste('Invalid IDs:',nm))
  }
  j <- s$jobs;t<-s$tasks;g<-s$goals
  a <- s$allocations
  if(any(!a$job_id%in%j$id) || any(a$fte<0|a$fte>1) || anyDuplicated(paste(a$job_id,a$name))) fail('Invalid contract area.')
  for(jid in j$id) if(sum(a$fte[a$job_id==jid])>j$fte[match(jid,j$id)]+1e-8) fail('Area FTE exceeds job FTE.')
  if(any(!t$job_id%in%j$id) || any(!g$job_id%in%j$id) || any(nzchar(t$goal_id)&!t$goal_id%in%g$id) || any(nzchar(t$depends)&!t$depends%in%t$id)) fail('Broken job, goal or dependency reference.')
  if(any(!j$pay_type%in%c('Salary','Hourly','Piece')) || any(!j$engagement%in%c('Employee','Self-employed'))) fail('Unknown job type.')
  if(any(j$fte<0|j$fte>1|j$target<0|j$weekly_income<0|j$rate<0|j$expense_pct<0|j$expense_pct>100)) fail('Invalid job values.')
  if(any(t$original_estimate<=0|t$total<=0|t$remaining<0|t$remaining>t$total|t$units<0|t$bonus<0|t$cash_cost<0|t$extension<0|t$extension!=round(t$extension)|t$nudges<0|t$nudges!=round(t$nudges)|t$min_block<.25|t$min_block>t$total|t$goal_units<0)) fail('Invalid task amounts.')
  for(col in c('personal','social','career','consequence')) if(any(t[[col]]<0|t[[col]]>5)) fail('Ratings must be 0 to 5.')
  if(any(!t$done & t$remaining==0)) fail('Finish a task when remaining time reaches zero.')
  if(any(t$total*4!=round(t$total*4)|t$remaining*4!=round(t$remaining*4)|t$min_block*4!=round(t$min_block*4))) fail('Use quarter-hour task estimates.')
  if(any(t$done & t$remaining!=0)) fail('Completed tasks must have zero remaining hours.')
  for(i in seq_len(nrow(t))) {
    seen <- t$id[i]; dep <- t$depends[i]
    while(nzchar(dep)) {if(dep%in%seen) fail('Dependency cycle.');seen<-c(seen,dep);dep<-t$depends[match(dep,t$id)]}
    if(nzchar(t$goal_id[i]) && g$job_id[match(t$goal_id[i],g$id)]!=t$job_id[i]) fail('Goals must belong to the task job.')
  }
  good_date <- function(x,blank=FALSE) {
    z <- suppressWarnings(as.Date(x,format='%Y-%m-%d'))
    (!is.na(z) & format(z,'%Y-%m-%d')==x) | (blank & x=='')
  }
  if(any(!good_date(t$due)) || any(!good_date(t$completed,TRUE)) || any(t$done & !nzchar(t$completed))) fail('Invalid task dates.')
  if(any(!good_date(g$start)|!good_date(g$end)|g$end<g$start|g$target<=0|g$baseline<0)) fail('Invalid goal period or target.')
  for(nm in c('availability','blocks')) {
    d <- s[[nm]]
    if(any(d$day<1|d$day>7|d$day!=round(d$day)|d$start<0|d$start>=24|d$end<0|d$end>24|d$start==d$end|d$start*4!=round(d$start*4)|d$end*4!=round(d$end*4))) fail('Use distinct times on quarter-hour boundaries, with days 1 to 7.')
  }
  if(any(!s$blocks$kind%in%c('Essential','Protected','Meeting')) || any(s$blocks$kind=='Meeting' & !s$blocks$job_id%in%j$id)) fail('Invalid calendar block.')
  if(any(!s$logs$task_id%in%t$id)) fail('Work log references a missing task.')
  if(any(!s$logs$job_id%in%j$id) || any(!good_date(s$logs$date)) || any(s$logs$hours<=0)) fail('Invalid work log.')
  if(!is.list(s$settings) || !s$settings$timezone%in%OlsonNames() || !is.numeric(s$settings$fulltime) || length(s$settings$fulltime)!=1 || !is.finite(s$settings$fulltime) || s$settings$fulltime<=0 || !is.character(s$settings$currency) || length(s$settings$currency)!=1) fail('Invalid settings.')
  if(!is.list(s$archive)||!all(c('jobs','allocations','tasks','goals','availability','blocks','logs')%in%names(s$archive)))fail('Invalid report archive.')
  for(nm in names(template$archive)) {
    d<-s$archive[[nm]];ref<-template[[nm]]
    if(!is.data.frame(d)||!identical(names(d),names(ref))||nrow(d)>10000||anyNA(d))fail(paste('Invalid archive table',nm))
    for(col in names(ref))if((is.numeric(ref[[col]])&&(!is.numeric(d[[col]])||any(!is.finite(d[[col]]))))||(is.character(ref[[col]])&&!is.character(d[[col]]))||(is.logical(ref[[col]])&&!is.logical(d[[col]])))fail(paste('Invalid archive column',nm,col))
    ids<-c(s[[nm]]$id,d$id);if(anyDuplicated(ids))fail(paste('Current and removed records share an ID:',nm))
  }
  # Validate archived references together; removed records are never silently re-used.
  at<-rbind(t,s$archive$tasks);aj<-rbind(j,s$archive$jobs);al<-rbind(s$logs,s$archive$logs)
  if(any(!at$job_id%in%aj$id)||any(!al$task_id%in%at$id)||any(!al$job_id%in%aj$id)||any(al$hours<=0)||any(!good_date(al$date)))fail('Broken historical task or log reference.')
  if(any(at$original_estimate<=0)||any(at$done & at$remaining!=0))fail('Invalid historical task estimate.')
  validate_opportunities(s)
  validate_overlays(s)
  TRUE
}
