README.md: README.Rmd
	Rscript -e 'library(knitr); print(dir()); knit("README.Rmd")'