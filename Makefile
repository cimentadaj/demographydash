all: README.md

README.md: README.Rmd
	R -e "devtools::build_readme()"

clean:
	rm README.md
