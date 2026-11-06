Sys.setenv(WORTHWHILE_MODE='local',WORTHWHILE_STATE_FILE='',PORTFOLIO_STATE_FILE='')
library(shiny);source('app.R')
# Exact binary outcomes, costs in failure, time and zero-probability cases.
s<-demo_state();o<-new_opportunity('o100','j1','A book',Sys.Date()+100,'Book');o$hours<-10;o$p_low<-.1;o$p_base<-.25;o$p_high<-.5;o$income_success<-1000;o$cost_upfront<-100;o$cost_failure<-20;o$years<-0;o$alt_hour<-10
v<-opportunity_values(o);stopifnot(v$cash[2]==135,v$economic[2]==35,v$per_hour==3.5,v$loss_probability==.75,v$max_cash_loss==120,abs(v$break_even-220/1020)<1e-12)
x<-o;x$income_success<-100;x$cost_upfront<-90;x$cost_failure<-0;x$years<-20;x$discount<-.1;stopifnot(opportunity_values(x)$loss_probability==.75,opportunity_values(x)$max_cash_loss==90)
o$p_low<-0;o$p_base<-0;o$p_high<-0;stopifnot(opportunity_values(o)$economic[2]==-220)
s$opportunities<-rbind(s$opportunities,o);x<-create_opportunity_goal(s,'o100');stopifnot(nrow(x$state$goals)==nrow(s$goals)+1,nrow(x$state$tasks)==nrow(s$tasks)+1,x$state$opportunities$goal_id[4]==x$goal_id,tail(x$state$tasks$bonus,1)==0)
y<-create_opportunity_goal(x$state,'o100');stopifnot(identical(x,y))
tid<-tail(x$state$tasks$id,1);s<-log_work(x$state,tid,2,remaining=5);r<-opportunity_report(s,'o100');stopifnot(r$Remaining_h==5,r$Logged_h==2)
s<-finish_with_actual(s,tid,6);stopifnot(opportunity_report(s,'o100')$Remaining_h==0,goal_progress(s)$progress[goal_progress(s)$id==x$goal_id]==1,s$opportunities$status[4]=='Pursuing')
s<-delete_goal(s,x$goal_id);stopifnot(s$opportunities$goal_id[4]=='',validate_state(s));s<-delete_job(s,'j1');stopifnot(!'o100'%in%s$opportunities$id,'o100'%in%s$archive$opportunities$id,validate_state(s))
# Non-billable preparation in an hourly role never becomes guaranteed task pay.
a<-new_opportunity('o101','j5','Unpaid proposal',Sys.Date()+10);s<-demo_state();s$opportunities<-rbind(s$opportunities,a);z<-create_opportunity_goal(s,'o101');stopifnot(tail(task_economics(z$state)$gross,1)==0,!tail(z$state$tasks$billable,1))
# A genuine schema-3 plan lacked this table in both current and archive.
s<-demo_state();s$schema<-3L;s$opportunities<-NULL;s$archive$opportunities<-NULL;f<-tempfile();jsonlite::write_json(s,f,auto_unbox=TRUE,dataframe='columns');z<-read_backup(f);unlink(f);stopifnot(z$schema==5L,nrow(z$opportunities)==0,validate_state(z))
# Mock transport exercises per-user JWTs, verified identity, refresh rotation,
# persistence, conflict handling and token removal. It does not contact Supabase.
uid1<-'11111111-1111-4111-8111-111111111111';uid2<-'22222222-2222-4222-8222-222222222222'
remote<-new.env();remote$rows<-list();remote$calls<-list();now<-0
transport<-function(method,url,key,token,body){
 remote$calls[[length(remote$calls)+1]]<-list(url=url,key=key,token=token)
 if(grepl('grant_type=password',url,fixed=TRUE)){who<-if(body$email=='a@example.com')'a'else'b';return(list(status=200,data=list(access_token=paste0('token-',who),refresh_token=paste0('refresh-',who),expires_in=100)))}
 if(grepl('grant_type=refresh_token',url,fixed=TRUE)){who<-substring(body$refresh_token,nchar(body$refresh_token));return(list(status=200,data=list(access_token=paste0('token-',who),refresh_token=paste0('rotated-',who),expires_in=100)))}
 who<-if(identical(token,'token-a'))uid1 else if(identical(token,'token-b'))uid2 else NULL
 if(grepl('/auth/v1/user',url,fixed=TRUE))return(list(status=if(is.null(who))401 else 200,data=list(id=who,email=if(identical(who,uid1))'a@example.com'else'b@example.com')))
 if(grepl('/auth/v1/logout',url,fixed=TRUE))return(list(status=204,data=NULL))
 if(is.null(who))return(list(status=401,data=NULL))
 if(grepl('/rest/v1/worthwhile_plans?',url,fixed=TRUE)){stopifnot(endsWith(url,who));row<-remote$rows[[who]];return(list(status=200,data=if(is.null(row))list()else list(row)))}
 if(grepl('/rpc/save_worthwhile_plan',url,fixed=TRUE)){row<-remote$rows[[who]];rev<-if(is.null(row))0 else row$revision;if(body$p_expected_revision!=rev)return(list(status=409,data=list(code='40001')));remote$rows[[who]]<-list(revision=rev+1,payload=body$p_payload);return(list(status=200,data=rev+1))}
 stop('Unexpected mock request')
}
config<-list(url='https://example.supabase.co',key='sb_publishable_test',signup=TRUE)
a<-new_supabase_client(config,transport,clock=function()now);b<-new_supabase_client(config,transport,clock=function()now)
stopifnot(a$sign_in('a@example.com','password')$id==uid1,b$sign_in('b@example.com','password')$id==uid2,is.null(a$load()),is.null(b$load()))
s<-demo_state();s$revision<-1;a$save(s,0);stopifnot(a$load()$revision==1,is.null(b$load()))
s$tasks$title[1]<-'B private';b$save(s,0);stopifnot(a$load()$tasks$title[1]!='B private',b$load()$tasks$title[1]=='B private')
stopifnot(inherits(tryCatch({a$save(s,0);NULL},error=identity),'error'))
now<-60;a$refresh();stopifnot(any(vapply(remote$calls,function(x)grepl('refresh_token',x$url,fixed=TRUE),logical(1))))
a$logout();stopifnot(is.null(a$user()),inherits(tryCatch({a$load();NULL},error=identity),'error'),b$load()$tasks$title[1]=='B private')
# Disabled modules retain data and cannot mutate through their old controls.
shiny::testServer(server,{
 session$setInputs(week=as.character(Sys.Date()),buffer=15,cap=8,protect=TRUE)
 before<-state()$opportunities
 session$setInputs(`decide-future-selected`='o2',`decide-future-goal`=1,`decide-apply`=1)
 stopifnot(identical(before,state()$opportunities),nrow(state()$tasks)==17)
})
# Cloud entry gate does not reveal the planner before credentials/configuration.
Sys.setenv(WORTHWHILE_MODE='supabase',SUPABASE_URL='',SUPABASE_PUBLISHABLE_KEY='');source('app.R')
shiny::testServer(server,{stopifnot(!authenticated(),is.null(config),grepl('Server setup required',output$cloud_page$html,fixed=TRUE),!grepl('task-desk',output$cloud_page$html,fixed=TRUE))})
# A verified sign-in mounts the real planner with cloud storage and saves only A.
Sys.setenv(SUPABASE_URL='https://example.supabase.co',SUPABASE_PUBLISHABLE_KEY='sb_publishable_test')
source('app.R');real_factory<-new_supabase_client
new_supabase_client<-function(config)real_factory(config,transport,clock=function()now)
shiny::testServer(server,{
 session$setInputs(auth_email='a@example.com',auth_password='password',auth_signin=1)
 stopifnot(authenticated(),grepl('today-content',output$cloud_page$html,fixed=TRUE))
 session$setInputs(week=as.character(Sys.Date()),buffer=15,cap=8,protect=TRUE)
 session$setInputs(`tasks-selected`='t1',`tasks-nudge`=1)
 stopifnot(as.numeric(unlist(remote$rows[[uid1]]$payload$tasks$nudges))[1]==1,b$load()$tasks$title[1]=='B private')
 session$setInputs(auth_logout=1);stopifnot(!authenticated(),grepl('/auth/v1/logout',tail(remote$calls,1)[[1]]$url,fixed=TRUE))
})
new_supabase_client<-real_factory
Sys.setenv(WORTHWHILE_MODE='local');Sys.unsetenv(c('SUPABASE_URL','SUPABASE_PUBLISHABLE_KEY'));source('app.R')
stopifnot(grepl(author_name,as.character(ui),fixed=TRUE),grepl(funding_sentence,as.character(ui),fixed=TRUE))
cat('PASS: opportunity risk/cost arithmetic, loss cases, role/goal/task integration, independent actuals, idempotent goal creation, schema-3 migration, user-scoped cloud clients, token rotation, conflicts, logout, module actions, cloud entry gate, author and funding credits.\n')
