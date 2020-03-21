filename := scrape
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

main:
	Rscript -e 'rmarkdown::render("${ROOT_DIR}/${filename}.Rmd")'

clean:
	rm -f ${filename}.html
	rm -f ${filename}.md
