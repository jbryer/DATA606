library(knitr)
library(markdown)

renderRmd <- function(path, input, envir=new.env(), quiet=TRUE) {
	return(renderText( {
		contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
		assign('input', input, envir=envir)
		html <- knitr::knit2html(text = contents, fragment.only = TRUE, 
								 envir=envir, quiet=quiet)
		Encoding(html) <- 'UTF-8'
		
		# Remove <!–html_preserve–> and <!–/html_preserve–> from output
		# This are put there with actionLink from Shiny
		html <- sub('&lt;!&ndash;html_preserve&ndash;&gt;', '', html)
		html <- sub('&lt;!&ndash;/html_preserve&ndash;&gt;', '', html)
		return(HTML(html))
	}))
}
