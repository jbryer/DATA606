% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab.R
\name{startLab}
\alias{startLab}
\title{Starts a new lab.}
\usage{
startLab(l, dest_dir = getwd())
}
\arguments{
\item{l}{the lab name (see \code{\link{getLabs}} for available labs).}

\item{dest_dir}{where the lab template will be copied to.}
}
\value{
the full path to the new Rmarkdown file for the lab.
}
\description{
This will copy the lab and associated files to the `dest_dir` directory (the
default is the current working directory) and then open
that Rmd file to begin editing (probably in RStudio).
}
