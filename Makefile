all: README.md

README.md: README.Rmd
	R -e "devtools::build_readme()"

deploy:
	@R -e "try(rsconnect::terminateApp(appName = 'pop_projection', account = 'unpop'), silent = TRUE); \
	unlink(renv::paths[['cache']](), recursive = TRUE); \
  setwd('/home/jorge/repositories/demographydash/'); \
	rsconnect::forgetDeployment('/home/jorge/repositories/demographydash/'); \
	rsconnect::deployApp(appName = 'pop_projection', account = 'unpop', forceUpdate = TRUE)"
