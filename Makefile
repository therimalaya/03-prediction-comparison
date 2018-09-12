DIR="docs"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

all:
	make gitbook
	make pdf

all: pdf gitbook
.PHONY: all

coverletter:
	Rscript --quiet -e "rmarkdown::render('CoverLetter.Rmd', output_format = rmarkdown::pdf_document(template = 'templates/CoverLetter.tex'))"

tufte:
	Rscript --quiet _render.R "bookdown::tufte_html_book"

htmlbook:
	Rscript --quiet _render.R "bookdown::html_book"

html:
	Rscript --quiet _render.R "bookdown::html_document2" && mv main.html public/index.html

serve:
	browser-sync start --server $(DIR) --files $(DIR) --no-open --no-ui

clean:
	rm -rf $(DIR)

app:
	Rscript -e "shiny::runApp('shiny/app.R', port = 4321)"

opdf:
	open docs/main.pdf

