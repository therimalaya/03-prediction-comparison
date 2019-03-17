DIR="docs"

all: pdf gitbook epub

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

serve:
	Rscript --quiet -e "servr::httw(dir = 'docs', watch = 'docs', port = 5555, host = '0.0.0.0')"

clean:
	rm -rf $(DIR) && rm main*.* && rm -rf _bookdown_files

app:
	Rscript -e "shiny::runApp('.', port = 4321, host = '0.0.0.0')"

epub:
	Rscript --quiet _render.R "bookdown::epub_book"

word:
	Rscript --quiet _render.R "bookdown::word_document2"

coverletter:
	Rscript --quiet -e "rmarkdown::render('letters/First-Submit.Rmd', output_format = 'linl::linl', output_dir = 'letters')"

.PHONY: all app

# all:
#		make gitbook
#		make pdf
#
# tufte:
#		Rscript --quiet _render.R "bookdown::tufte_html_book"
#
# htmlbook:
#		Rscript --quiet _render.R "bookdown::html_book"
#
# html:
#		Rscript --quiet _render.R "bookdown::html_document2" && mv main.html public/index.html
