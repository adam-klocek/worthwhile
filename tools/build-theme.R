# Run from the application directory; no packages needed.
source('R/design.R')
writeLines(theme_tokens_css(),'www/tokens.css',useBytes=TRUE)
cat('Generated www/tokens.css from R/design.R\n')
