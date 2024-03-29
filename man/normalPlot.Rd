% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalPlot.R
\name{normalPlot}
\alias{normalPlot}
\title{Plot a normal distribution.}
\usage{
normalPlot(mean = 0, sd = 1, bounds = c(-1, 1), tails = FALSE)
}
\arguments{
\item{mean}{mean of the distribution.}

\item{sd}{standard deviation of the distribution.}

\item{bounds}{the bounds to shade.}

\item{tails}{should the tails be shaded.}
}
\description{
Plots a normal distribution with the area within the given bounds shaded.
The probability (i.e. area) under the normal curve for the given bounds
is also given.
}
\examples{
	normalPlot()
	normalPlot(bounds=c(-2,2))
	normalPlot(bounds=c(-3,3))
	normalPlot(tails=TRUE)
}
\seealso{
http://www.statmethods.net/advgraphs/probability.html
}
