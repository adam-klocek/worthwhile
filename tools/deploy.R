# Optional console publishing for Posit Connect Cloud after setting account access.
# GitHub publishing can use the included renv.lock without this script.
if(!requireNamespace('rsconnect',quietly=TRUE))install.packages('rsconnect',repos='https://cloud.r-project.org')
if(!file.exists('app.R'))stop('Run from the Worthwhile repository root.')
if(!identical(Sys.getenv('WORTHWHILE_MODE'),'supabase'))stop('Set WORTHWHILE_MODE=supabase before deployment.')
source('R/model.R');source('R/supabase.R');invisible(supabase_config())
rsconnect::connectCloudUser()
rsconnect::deployApp(appDir='.',appPrimaryDoc='app.R',appName='worthwhile')

cat('Set the Supabase variables in your Connect Cloud content settings, then restart the app.\n')
