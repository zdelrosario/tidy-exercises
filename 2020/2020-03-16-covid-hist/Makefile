filename=scrape

main:
	Rscript -e 'rmarkdown::render("${filename}.Rmd")'

clean:
	rm -f ${filename}.html
	rm -f ${filename}.md
