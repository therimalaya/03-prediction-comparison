DIR="docs"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

all: pdf gitbook epub

serve:
	Rscript --quiet -e "servr::httd('docs', port = 5555, host = '0.0.0.0')"

clean:
	rm -rf $(DIR) && rm main*.* && rm -rf _bookdown_files

app:
	Rscript -e "shiny::runApp('.', port = 4321, host = '0.0.0.0')"

epub:
	Rscript --quiet _render.R "bookdown::epub_book"

.PHONY: all app

# all:
#		make gitbook
#		make pdf

# coverletter:
#		Rscript --quiet -e "rmarkdown::render('CoverLetter.Rmd', output_format = rmarkdown::pdf_document(template = 'templates/CoverLetter.tex'))"
#
# tufte:
#		Rscript --quiet _render.R "bookdown::tufte_html_book"
#
# htmlbook:
#		Rscript --quiet _render.R "bookdown::html_book"
#
# html:
#		Rscript --quiet _render.R "bookdown::html_document2" && mv main.html public/index.html
