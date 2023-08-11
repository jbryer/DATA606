#' Plot a normal distribution.
#'
#' Plots a normal distribution with the area within the given bounds shaded.
#' The probability (i.e. area) under the normal curve for the given bounds
#' is also given.
#'
#' @param mean mean of the distribution.
#' @param sd standard deviation of the distribution.
#' @param bounds the bounds to shade.
#' @param tails should the tails be shaded.
#' @seealso http://www.statmethods.net/advgraphs/probability.html
#' @export
#' @examples
#' 	normalPlot()
#' 	normalPlot(bounds=c(-2,2))
#' 	normalPlot(bounds=c(-3,3))
#' 	normalPlot(tails=TRUE)
normalPlot <- function(mean=0, sd=1, bounds=c(-1,1), tails=FALSE) {
	warning('This function is deprecated. Please use the normal_plot function instead.')

	normal_plot(mean = mean, sd = sd, bounds = bounds,
				tails = ifelse(tails, 'two.sided', 'no'))
}
