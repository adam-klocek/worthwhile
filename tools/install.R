# Run from the repository root in R/RStudio/Posit Cloud.
install.packages('renv',repos='https://cloud.r-project.org')
renv::restore(prompt=FALSE)
cat('Dependencies restored. Use renv::load() in this R session before launching.\n')
