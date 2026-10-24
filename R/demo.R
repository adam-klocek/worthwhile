demo_state <- function(today=Sys.Date()) {
  monday <- week_start(today)
  year <- format(today,'%Y'); sem_start <- as.Date(paste0(year,if(as.integer(format(today,'%m'))<=6) '-01-01' else '-07-01'))
  sem_end <- as.Date(if(as.integer(format(today,'%m'))<=6) paste0(year,'-06-30') else paste0(year,'-12-31'))
  jobs <- data.frame(id=paste0('j',1:5),name=c('University · teaching & research','University · research institute','Book translation','Room cleaning','Development studio'),icon=c('graduation-cap','flask','book','broom','laptop-code'),engagement=c('Employee','Employee','Self-employed','Self-employed','Self-employed'),pay_type=c('Salary','Salary','Piece','Piece','Hourly'),fte=c(.5,.5,0,0,0),target=c(20,20,5,3,8),weekly_income=c(450,550,0,0,0),rate=c(0,0,12,25,45),unit=c('','','page','room','hour'),expense_pct=c(0,0,10,15,10),notes=c('Teaching 0.3 FTE (12 h/week); research 0.2 FTE (8 h/week). Editable sample pay.','One first-author and two other studies/year. Outreach bonus; grant applications expected.','English → German. All rates are fictional.','Per room. Include travel and supplies in estimates.','Hours are billable only when actually worked. Deadlines still apply.'),stringsAsFactors=FALSE)
  goals <- data.frame(id=paste0('g',1:9),job_id=c('j1','j1','j1','j1','j2','j2','j2','j3','j5'),title=c('First-author paper · university A','Guarantee two courses','Supervise five theses','Extra supervision toward promotion','First-author study · university B','Two coauthored studies','Submit a grant application','Translate the book','Ship the long-term project'),target=c(1,2,5,5,1,2,1,200,1),baseline=c(0,0,0,0,0,0,0,100,0),unit=c('paper','courses','theses','theses','paper','papers','application','pages','release'),start=c(paste0(year,'-01-01'),as.character(sem_start),as.character(sem_start),as.character(sem_start),rep(paste0(year,'-01-01'),5)),end=c(paste0(year,'-12-31'),as.character(sem_end),as.character(sem_end),as.character(sem_end),rep(paste0(year,'-12-31'),5)),stringsAsFactors=FALSE)
  task <- function(id,job,title,h,days,hard=TRUE,goal='',units=0,quantity=0,bonus=0,nudges=0,consequence=2,personal=2,social=2,career=2,area='',min_block=.5,depends='',cash_cost=0) data.frame(id=id,job_id=job,title=title,area=area,total=h,remaining=h,due=as.character(today+days),hard=hard,extension=0,nudges=nudges,consequence=consequence,personal=personal,social=social,career=career,min_block=min_block,units=quantity,bonus=bonus,cash_cost=cash_cost,goal_id=goal,goal_units=units,depends=depends,done=FALSE,completed='',stringsAsFactors=FALSE)
  tasks <- do.call(rbind,list(
    task('t1','j1','Finish and submit first-author paper',24,28,goal='g1',units=1,career=5,area='Research',min_block=1),
    task('t2','j1','Guarantee course: research methods',12,12,goal='g2',units=1,consequence=4,area='Teaching'),
    task('t3','j1','Guarantee course: data analysis',10,19,goal='g2',units=1,consequence=4,area='Teaching'),
    task('t4','j1','Supervise five required theses: remaining work',20,35,goal='g3',units=5,area='Teaching',consequence=4),
    task('t5','j1','Supervise five additional interesting theses',20,60,FALSE,goal='g4',units=5,personal=4,career=5,area='Teaching'),
    task('t6','j2','Submit first-author study',18,21,goal='g5',units=1,career=5,area='Research',min_block=1),
    task('t7','j2','Finish coauthored study A',8,14,goal='g6',units=1,career=4),
    task('t8','j2','Finish coauthored study B',10,42,FALSE,goal='g6',units=1,career=4),
    task('t9','j2','Write and submit grant application',14,10,goal='g7',units=1,career=5,consequence=3,min_block=1),
    task('t10','j2','Deliver science outreach talk',2,6,bonus=180,social=5,personal=4),
    task('t11','j3','Translate next 20 pages',6,5,goal='g8',units=20,quantity=20,personal=3),
    task('t12','j4','Clean four rooms including travel',3,2,quantity=4,consequence=4),
    task('t13','j5','Deadline sprint: repair checkout',4,1,nudges=3,consequence=5,career=3),
    task('t14','j5','Build long-term project milestone',16,30,goal='g9',units=1,career=4,min_block=1),
    task('t15','j5','Send the progress update',.25,1,nudges=2,consequence=2,min_block=.25),
    task('t16','j1','Sketch my own book proposal',3,90,FALSE,personal=5,career=3,min_block=1),
    task('t17','j2','Revise grant budget after application draft',1,11,depends='t9',min_block=.5)
  ))
  tasks$original_estimate<-tasks$total;tasks$collaborators<-'';tasks$created<-as.character(today);tasks$time_complete<-FALSE;tasks$blocked_reason<-'';tasks$started<-FALSE;tasks$paused<-FALSE;tasks$next_action<-'';tasks$billable<-TRUE
  availability <- data.frame(id=paste0('a',1:7),day=1:7,start=c(rep(7,5),9,9),end=c(rep(23,5),18,18),stringsAsFactors=FALSE)
  blocks <- data.frame(id=character(),day=integer(),title=character(),kind=character(),start=numeric(),end=numeric(),job_id=character(),stringsAsFactors=FALSE)
  add <- function(day,title,kind,start,end,job='') {blocks <<- rbind(blocks,data.frame(id=paste0('b',nrow(blocks)+1),day=day,title=title,kind=kind,start=start,end=end,job_id=job,stringsAsFactors=FALSE))}
  for(d in 1:7) {
    add(d,'Sleep','Essential',23,7);add(d,'Breakfast & hygiene','Essential',7,8)
    add(d,'Lunch','Essential',12,13);add(d,'Dinner','Essential',18,19)
    add(d,'Bathroom & movement break','Essential',10.5,10.75);add(d,'Bathroom & movement break','Essential',15.5,15.75)
  }
  add(2,'University A meeting','Meeting',9,10,'j1');add(4,'University B lab meeting','Meeting',9,11,'j2')
  add(3,'Hobby evening','Protected',19,23);add(5,'Friends & family','Protected',19,23)
  add(6,'Social time','Protected',9,18);add(7,'Rest & hobbies','Protected',9,18)
  s<-list(schema=4L,settings=list(currency='EUR',timezone='Europe/Prague',fulltime=40,setup_complete=FALSE,example_mode=TRUE),jobs=jobs,allocations=data.frame(id=c('c1','c2'),job_id=c('j1','j1'),name=c('Teaching','Research'),fte=c(.3,.2),stringsAsFactors=FALSE),goals=goals,tasks=tasks,availability=availability,blocks=blocks,logs=data.frame(id=character(),task_id=character(),job_id=character(),date=character(),hours=numeric(),note=character(),created_at=character(),updated_at=character(),stringsAsFactors=FALSE))
  s$exceptions<-empty_exceptions();s$locks<-empty_locks();s$revision<-0L;s$log_sequence<-0
  s$opportunities<-example_opportunities(s,today)
  s$archive<-lapply(s[c('jobs','allocations','tasks','goals','availability','blocks','logs','opportunities')],function(d)d[FALSE,,drop=FALSE]);s
}
