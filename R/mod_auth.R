author_name <- 'Mgr. Adam Klocek, Ph.D.'
funding_sentence <- 'Funded by the project Mediated Society (MEDIS:ON), registration number: CZ.02.01.01/00/23_025/0008713, supported by the European Union and Czech Republic.'
credit_footer <- function()tags$footer(class='app-footer',tags$div(tags$span(class='footer-brand','Worthwhile'),tags$p(paste('Author:',author_name))),tags$p(class='funding-credit',funding_sentence))
authUI <- function(signup=TRUE,message=NULL)tagList(
 tags$div(class='auth-shell',tags$div(class='auth-brand',tags$span(class='brand',tags$span(class='brand-mark','W'),'Worthwhile'),tags$p(class='lead','Make better decisions about what deserves your time.')),
 tags$section(class='setup-card auth-card',tags$p(class='eyebrow','YOUR PRIVATE WORKSPACE'),tags$h1('A little clarity starts here.'),if(!is.null(message))tags$div(class='notice warning',message),
 textInput('auth_email','Email',placeholder='you@example.com'),passwordInput('auth_password','Password'),tags$div(class='card-actions',actionButton('auth_signin','Sign in',class='btn-primary'),if(signup)actionButton('auth_signup','Create account',class='btn-quiet')),tags$p(class='hint','Your tasks and work history are saved to your own account. Passwords are sent over HTTPS to Supabase Auth; they are not stored in your plan.'),tags$p(class='hint','If registration requires email confirmation, confirm your email and return here to sign in. Signing out or refreshing ends this app connection; sign in again to resume.'))),credit_footer())
cloudServer <- function(input,output,session) {
 authenticated<-reactiveVal(FALSE);config_error<-NULL
 config<-tryCatch(supabase_config(),error=function(e){config_error<<-conditionMessage(e);NULL})
 client<-NULL;started<-FALSE;user<-NULL
 output$cloud_page<-renderUI({if(authenticated())planner_ui else if(is.null(config))authUI(FALSE,config_error)else authUI(config$signup)})
 output$account_controls<-renderUI({if(authenticated())tags$div(class='account-controls',tags$span(user$email),actionButton('auth_logout','Sign out',class='btn-quiet'))})
 login<-function(register=FALSE){
  if(is.null(config)||started)return()
  req(nzchar(trimws(input$auth_email)),nzchar(input$auth_password))
  if(register&&nchar(input$auth_password)<8){showNotification('Use a password of at least eight characters.',type='warning');return()}
  # A fresh client prevents a failed login from retaining an earlier identity.
  candidate<-new_supabase_client(config)
  tryCatch({u<-if(register)candidate$sign_up(input$auth_email,input$auth_password)else candidate$sign_in(input$auth_email,input$auth_password)
   updateTextInput(session,'auth_password',value='');session$sendCustomMessage('worthwhile-clear-password',TRUE)
   if(is.null(u)){candidate$clear();showNotification('Check your email to confirm registration, then sign in.',duration=NULL);return()}
   loaded<-candidate$load();client<<-candidate;user<<-u;started<<-TRUE
   plannerServer(input,output,session,store=client,loaded_state=loaded)
   authenticated(TRUE)
  },error=function(e){candidate$clear();updateTextInput(session,'auth_password',value='');showNotification(conditionMessage(e),type='error',duration=10)})
 }
 observeEvent(input$auth_signin,login(FALSE));observeEvent(input$auth_signup,login(TRUE))
 observeEvent(input$auth_logout,{if(!is.null(client))client$logout();authenticated(FALSE);session$sendCustomMessage('worthwhile-signed-out',TRUE);session$onFlushed(function()session$close(),once=TRUE)})
 observe({invalidateLater(60000,session);if(authenticated()&&!is.null(client))tryCatch(client$refresh(),error=function(e)showNotification(conditionMessage(e),type='error',duration=NULL))})
 session$onSessionEnded(function(){if(!is.null(client))client$clear()})
}
