# Run from the extracted application directory with Rscript tests/run-all.R.
Sys.setenv(WORTHWHILE_MODE='local',WORTHWHILE_STATE_FILE='',PORTFOLIO_STATE_FILE='')
options(expressions=max(5000,getOption('expressions')))
for(test in c('test-model.R','test-server.R','test-setup.R','test-history.R','test-worthwhile.R','test-design.R','test-cloud-opportunities.R')) {
 cat('\nRUN:',test,'\n')
 source(file.path('tests',test))
}
cat('\nAll Worthwhile regression suites passed.\n')
