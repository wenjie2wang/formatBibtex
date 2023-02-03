objects := $(wildcard R/*.R) $(wildcard src/*.[hc]pp) DESCRIPTION
version := $(shell grep "Version" DESCRIPTION | sed "s/Version: //")
pkg := $(shell grep "Package" DESCRIPTION | sed "s/Package: //")
tar := $(pkg)_$(version).tar.gz
checkLog := $(pkg).Rcheck/00check.log
# tests := $(wildcard tests/testthat/*.R)
# rmd := $(wildcard vignettes/*.Rmd)
# vignettes := $(patsubst %.Rmd,%.html,$(rmd))

.PHONY: check
check: $(checkLog)

.PHONY: build
build: $(tar)

.PHONY: install
install: $(tar)
	R CMD INSTALL $(tar)

# .PHONY: preview
# preview: $(vignettes)

.PHONY: pkgdown
pkgdown:
	@Rscript -e "library(methods); \
	options(pkgdown.internet = FALSE); \
	pkgdown::build_site();"

$(tar): $(objects)
	@Rscript -e "library(methods);" \
	-e "devtools::document();";
	@$(MAKE) updateTimestamp
	R CMD build .

$(checkLog): $(tar)
	R CMD check --as-cran $(tar)

# vignettes/%.html: vignettes/%.Rmd
#	Rscript -e "library(methods); rmarkdown::render('$?')"

## update copyright year in HEADER, R script and date in DESCRIPTION
.PHONY: updateTimestamp
updateTimestamp:
	@bash misc/update_timestamp.sh

## make tags
.PHONY: TAGS
TAGS:
	Rscript -e "utils::rtags(path = 'R', ofile = 'TAGS')"

.PHONY: clean
clean:
	@rm -rf *~ */*~ *.Rhistroy *.tar.gz src/{*.so,*.o} *.Rcheck/ .\#*
