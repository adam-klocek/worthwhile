Sys.setenv(WORTHWHILE_MODE='local')
# Theme wiring and rendered UI regressions; run from application directory.
Sys.setenv(WORTHWHILE_STATE_FILE='',PORTFOLIO_STATE_FILE='')
library(shiny);source('app.R')
stopifnot(identical(paste(readLines('www/tokens.css',warn=FALSE),collapse='\n'),theme_tokens_css()))
html<-shiny:::renderPage(ui)
bootstrap_pos<-regexpr('bootstrap-[^" ]+/css/bootstrap.min.css',html)[1]
theme_pos<-regexpr(paste0('style.css?v=',worthwhile_version),html,fixed=TRUE)[1]
stopifnot(bootstrap_pos>0,theme_pos>bootstrap_pos,grepl('class="skip-link"',html,fixed=TRUE))
for(id in c('today','plan','decide','reflect'))stopifnot(grepl(paste0('<main id="',id,'-content" tabindex="-1">'),html,fixed=TRUE))
shiny::testServer(server,{
 session$setInputs(week=as.character(Sys.Date()),buffer=15,cap=8,protect=TRUE)
 cards<-output[['tasks-cards']]$html
 stopifnot(grepl('task-actions',cards,fixed=TRUE),grepl('task-action-menu',cards,fixed=TRUE),!grepl('>Next up<',cards,fixed=TRUE))
 stopifnot(grepl('recommendation-panel',as.character(ui),fixed=TRUE))
})
cat('PASS: generated theme tokens, post-Bootstrap stylesheet order, versioned assets, skip link, four main landmarks and quieter task actions.\n')
