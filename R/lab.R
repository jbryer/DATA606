#' Opens a lab.
#'
#' @param l the lab name (see \{code{\link{getLabs}} for available labs).
#' @param new_file where the lab template will be copied to.
#' @export
lab <- function(l, new_file = paste0(getwd(), '/', Sys.info()['user'], '-Lab', lab, '.Rmd')) {

}

#' Returns a list of available labs.
#'
#' @export
getLabs <- function() {
	labs <- list.dirs(path=paste0(find.package('IS606'), '/Labs'),
					  recursive=FALSE, full.names=FALSE)
	#return(substr(labs, 1, nchar(labs) - 4))
	return(labs)
}
