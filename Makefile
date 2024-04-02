all: README.md

README.md: README.Rmd
	R -e "devtools::build_readme()"

deploy:
	@R -e "try(rsconnect::terminateApp(appName = 'pop_projection', account = 'unpop'), silent = TRUE); \
	try(rsconnect::purgeApp(appName = 'pop_projection', account = 'unpop'), silent = TRUE); \
	unlink('/home/jorge/.cache/R/', recursive = TRUE); \
	renv::clean(); \
  setwd('/home/jorge/repositories/demographydash/'); \
	rsconnect::forgetDeployment('/home/jorge/repositories/demographydash/', force = TRUE); \
	rsconnect::deployApp(appName = 'pop_projection', account = 'unpop', forceUpdate = TRUE)"
