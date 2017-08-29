#' DATA606 Statistics and Probabilty for Data Analytics
#'
#' @name DATA606-package
#' @docType package
#' @title R Package to support DATA606 Statistics and Probabilty for Data Analytics.
#' @author \email{jason@@bryer.org}
#' @keywords package shiny
#' @import shiny
#' @import openintro
NULL

#' Function executed when the package is loaded.
#'
#' @param libname the library name.
#' @param pkgname the package name (hopefully 'DATA606').
#' @name onAttach
.onAttach <- function(libname, pkgname) {
	cat("\nWelcome to CUNY DATA606 Statistics and Probability for Data Analytics",
		"\nThis package is designed to support this course. The text book used",
		"\nis OpenIntro Statistics, 3rd Edition. You can read this by typing",
		"\nvignette('os3') or visit www.OpenIntro.org.",
		"\n",
		"\nThe getLabs() function will return a list of the labs available.",
		"\n",
		"\nThe demo(package='DATA606') will list the demos that are available.",
		"\n")
}

#' Demonstrates R Functionality
#'
#' This overwrites the \code{\link{demo}} function in the \code{utils} package
#' setting by setting \code{ask=FALSE} and \code{echo=FALSE}.
#'
#' @inheritParams utils::demo
#' @export
demo <- function(topic, echo=FALSE, ask=FALSE, ...) {
	utils::demo(topic=topic, echo=echo, ask=ask, character.only = TRUE, ...)
}
