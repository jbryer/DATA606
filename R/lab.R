#' Starts a new lab.
#'
#' This will copy the lab and associated files to the `dest_dir` directory (the
#' default is the current working directory), rename the Rmd file to include
#' `file.prefix` (the default is the current logged in username), and then open
#' that Rmd file to begin editting (probably in RStudio).
#'
#' @param l the lab name (see \{code{\link{getLabs}} for available labs).
#' @param dest_dir where the lab template will be copied to.
#' @param file.prefix the prefix to put before the Rmd file. This is typically
#'        your last name but be sure to check the assignment instructions.
#' @return the full path to the new Rmarkdown file for the lab.
#' @export
startLab <- function(l, dest_dir = getwd(),
					 file.prefix = paste0(Sys.info()['user'], '-')) {
	path <- paste0(find.package('DATA606'), '/labs/', l)
	file.copy(path, dest_dir, recursive=TRUE, overwrite=FALSE)
	rmds <- list.files(paste0(dest_dir, '/', l), pattern='.Rmd')
	new_file <- paste0(dest_dir, '/', l, '/', file.prefix, rmds[1])
	success <- file.exists(paste0(dest_dir, '/', l, '/', rmds[1]))
	if(success) {
		success <- file.rename(paste0(dest_dir, '/', l, '/', rmds[1]),
							   new_file)
		if(success) {
			file.edit(new_file)
			message(paste0("Setting working directory to ", path))
			setwd(path)
		} else {
			warning('Rmd file could not be automically renamed with your name.
					Please be sure to rename the file before submitting it.')
		}
	} else {
		stop('The lab did not copy! Not sure why, ask your instructor.')
	}
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
