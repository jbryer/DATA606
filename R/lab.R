#' Starts a new lab.
#'
#' This will copy the lab and associated files to the `dest_dir` directory (the
#' default is the current working directory) and then open
#' that Rmd file to begin editing (probably in RStudio).
#'
#' @param l the lab name (see \code{\link{getLabs}} for available labs).
#' @param dest_dir where the lab template will be copied to.
#' @return the full path to the new Rmarkdown file for the lab.
#' @export
startLab <- function(l, dest_dir = getwd()) {
	path <- paste0(find.package('DATA606'), '/labs/', l)
	rmds <- list.files(path, pattern='.Rmd')
	if(length(rmds) != 1) {
		stop('Invalid lab specified. Use getLabs() to list available labs.')
	}
	new_file <- paste0(dest_dir, '/', l, '/', rmds[1])
	if(!file.exists(new_file)) {
		file.copy(path, dest_dir, recursive=TRUE, overwrite=FALSE)
	}
	file.edit(new_file)
	return(new_file)
}

#' Opens the HTML page for the lab in the default browser.
#'
#' @param l the lab to open.
#' @export
viewLab <- function(l) {
	path <- paste0(find.package('DATA606'), '/labs/', l)
	htmls <- list.files(path, pattern='.html')
	if(length(htmls) > 0) {
		system(paste0("open '", path, "/", htmls[1], "'"))
	} else {
		stop(paste0("Lab (", l, ") not found. Use getLabs() for list of available labs."))
	}
	invisible()
}

#' Returns a list of available labs.
#'
#' @export
getLabs <- function() {
	labs <- list.dirs(path=paste0(find.package('DATA606'), '/labs'),
					  recursive=FALSE, full.names=FALSE)
	#return(substr(labs, 1, nchar(labs) - 4))
	return(labs)
}
