# One authenticated client per Shiny session; no service-role key and no local fallback.
worthwhile_mode <- function() {
 mode<-Sys.getenv('WORTHWHILE_MODE',unset='supabase')
 if(!mode%in%c('local','supabase'))stop('WORTHWHILE_MODE must be local or supabase.')
 mode
}
supabase_config <- function() {
 url<-sub('/+$','',Sys.getenv('SUPABASE_URL'));key<-Sys.getenv('SUPABASE_PUBLISHABLE_KEY')
 if(!grepl('^https://[A-Za-z0-9.-]+$',url)||!startsWith(key,'sb_publishable_'))stop('Server setup required: configure SUPABASE_URL (HTTPS project URL) and SUPABASE_PUBLISHABLE_KEY. Use a publishable key, never a secret or service-role key.')
 list(url=url,key=key,signup=identical(tolower(Sys.getenv('WORTHWHILE_ALLOW_SIGNUP','true')),'true'))
}
supabase_http <- function(method,url,key,token=NULL,body=NULL) {
 if(!requireNamespace('httr2',quietly=TRUE))stop('Install the httr2 package to enable cloud access.')
 req<-httr2::request(url);req<-httr2::req_method(req,method);req<-httr2::req_headers(req,apikey=key)
 if(!is.null(token))req<-httr2::req_headers(req,Authorization=paste('Bearer',token))
 if(!is.null(body))req<-httr2::req_body_json(req,body,auto_unbox=TRUE,digits=NA)
 req<-httr2::req_timeout(req,20);req<-httr2::req_error(req,is_error=function(resp)FALSE)
 resp<-tryCatch(httr2::req_perform(req),error=function(e)stop('Could not reach Supabase. Your current plan was kept; no local fallback save was made.',call.=FALSE))
 value<-tryCatch(httr2::resp_body_json(resp,simplifyVector=FALSE),error=function(e)NULL)
 list(status=httr2::resp_status(resp),data=value)
}
new_supabase_client <- function(config,transport=supabase_http,clock=function()as.numeric(Sys.time())) {
 secret<-new.env(parent=emptyenv());secret$access<-NULL;secret$refresh<-NULL;secret$expires<-0;secret$user<-NULL
 call<-function(method,path,body=NULL,authorized=FALSE) {
  token<-if(authorized)secret$access else NULL
  r<-transport(method,paste0(config$url,path),config$key,token,body)
  if(!is.list(r)||is.null(r$status))stop('Invalid cloud response.')
  r
 }
 accept<-function(data) {
  if(!is.list(data)||!is.character(data$access_token)||length(data$access_token)!=1||!nzchar(data$access_token)||!is.character(data$refresh_token)||length(data$refresh_token)!=1)stop('Sign-in did not return a valid session.')
  secret$access<-data$access_token;secret$refresh<-data$refresh_token
  ttl<-as.numeric(data$expires_in %||% 3600);if(length(ttl)!=1||!is.finite(ttl)||ttl<=0)stop('Invalid session lifetime.')
  secret$expires<-clock()+ttl
 }
 clear<-function(){secret$access<-NULL;secret$refresh<-NULL;secret$expires<-0;secret$user<-NULL;invisible(TRUE)}
 refresh<-function(force=FALSE) {
  if(is.null(secret$access))stop('Please sign in before accessing a plan.')
  if(force||clock()>secret$expires-60){r<-call('POST','/auth/v1/token?grant_type=refresh_token',list(refresh_token=secret$refresh));if(r$status!=200){clear();stop('Your session expired. Reload the page and sign in again.')};accept(r$data)}
  invisible(TRUE)
 }
 verified_user<-function(){r<-call('GET','/auth/v1/user',authorized=TRUE);u<-r$data;if(r$status!=200||!is.list(u)||!is.character(u$id)||length(u$id)!=1||!grepl('^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$',u$id)){clear();stop('Could not verify your Supabase identity.')};secret$user<-list(id=u$id,email=u$email %||% 'Signed-in user');secret$user}
 sign_in<-function(email,password){clear();r<-call('POST','/auth/v1/token?grant_type=password',list(email=trimws(email),password=password));if(r$status!=200)stop(if(r$status==429)'Too many attempts. Please wait before trying again.'else'Sign-in failed. Check your email, password and email confirmation.');accept(r$data);verified_user()}
 sign_up<-function(email,password){if(!isTRUE(config$signup))stop('Registration is disabled. Ask the app owner for an account.');r<-call('POST','/auth/v1/signup',list(email=trimws(email),password=password));if(!r$status%in%c(200L,201L))stop('Could not create an account. Check the account requirements or try again later.');if(is.list(r$data)&&!is.null(r$data$access_token)){accept(r$data);verified_user()}else NULL}
 read_plan<-function(){refresh();if(is.null(secret$user))stop('No verified user.');r<-call('GET',paste0('/rest/v1/worthwhile_plans?select=revision,payload&user_id=eq.',secret$user$id),authorized=TRUE);if(r$status==401){refresh(TRUE);r<-call('GET',paste0('/rest/v1/worthwhile_plans?select=revision,payload&user_id=eq.',secret$user$id),authorized=TRUE)};if(r$status!=200)stop('Could not load your private plan. Check the database setup or reconnect.');rows<-r$data;if(!is.list(rows)||length(rows)>1)stop('Invalid private-plan response.');if(!length(rows))return(NULL);row<-rows[[1]];rev<-as.numeric(row$revision);if(length(rev)!=1||!is.finite(rev)||rev<1||rev!=round(rev))stop('Invalid cloud revision.');payload<-jsonlite::fromJSON(jsonlite::toJSON(row$payload,auto_unbox=TRUE,digits=NA,null='null'),simplifyVector=TRUE);s<-decode_state(payload);s$revision<-rev;validate_state(s);s}
 save_plan<-function(s,expected_revision) {
  refresh();if(is.null(secret$user))stop('No verified user.');validate_state(s)
  if(s$revision!=expected_revision+1)stop('Invalid save revision.')
  # Serialize data frames as columns, exactly like downloadable backups.
  payload<-jsonlite::fromJSON(jsonlite::toJSON(s,auto_unbox=TRUE,digits=NA,dataframe='columns',null='null'),simplifyVector=FALSE)
  r<-call('POST','/rest/v1/rpc/save_worthwhile_plan',list(p_payload=payload,p_expected_revision=expected_revision),authorized=TRUE)
  if(r$status==409||r$status==400&&identical(r$data$code,'40001'))stop('Your plan changed in another tab or device. This change was not saved. Copy any edited form values before reloading the saved plan.')
  if(r$status==401)stop('Your session expired. Download a backup if needed, then reload and sign in again.')
  if(!r$status%in%c(200L,201L))stop('Cloud save failed. Your current plan was kept. Check connectivity and database configuration; no local fallback was used.')
  rev<-suppressWarnings(as.numeric(unlist(r$data)))
  if(length(rev)!=1||!is.finite(rev)||rev!=s$revision)stop('Cloud save was not confirmed. Reload before making another save.')
  invisible(TRUE)
 }
 logout<-function(){token<-secret$access;clear();if(!is.null(token))try(transport('POST',paste0(config$url,'/auth/v1/logout?scope=local'),config$key,token,NULL),silent=TRUE);invisible(TRUE)}
 list(sign_in=sign_in,sign_up=sign_up,load=read_plan,save=save_plan,refresh=refresh,logout=logout,clear=clear,user=function()secret$user,label='Saved to your private account.')
}
