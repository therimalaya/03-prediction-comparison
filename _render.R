quiet = "--quiet" %in% commandArgs(TRUE)
formats = commandArgs(TRUE)

if (length(formats) == 0) formats = c('bookdown::pdf_book', 'bookdown::gitbook')
for (fmt in formats) {
  cmd = sprintf("bookdown::render_book('index.Rmd', '%s', quiet = %s)", fmt, quiet)
  res = bookdown:::Rscript(c('-e', shQuote(cmd)))
  if (res != 0) stop('Failed to compile the book to ', fmt)
}

# if (length(formats) > 1 && Sys.getenv('USER') == 'therimalaya') {
#   bookdown::publish_book(account = 'yihui', server = 'bookdown.org')
# }
# 
# 
# # Get arguments from command line
# quiet <- "--quiet" %in% commandArgs(FALSE)
# formats <- commandArgs(TRUE)
# 
# # Identify Source File
# src <- (function() {
#   attr(body(sys.function()), 'srcfile')
# })()$filename
# if (is.null(src) || src == '') src = '.'
# 
# # Set working directory to source directory
# owd <- setwd(dirname(src))
# 
# # provide default formats if necessary
# if (length(formats) == 0) {
#   formats <- c(
#     'bookdown::pdf_book',
#     'bookdown::gitbook'
#     # 'bookdown::epub_book',
#     # 'bookdown::html_document2'
#     # 'bookdown::tufte_html_book'
#   )
# }
# 
# # render the book to all formats unless they are specified via command-line args
# for (fmt in formats) {
#   cmd <- sprintf("bookdown::render_book('index.Rmd', '%s', quiet = %s)",
#                  fmt, quiet)
#   
#   res <- bookdown:::Rscript(c('-e', shQuote(cmd)))
#   if (res != 0) stop('Failed to compile the book to ', fmt)
# }
# 
# # Set previous working directory
# setwd(owd)
