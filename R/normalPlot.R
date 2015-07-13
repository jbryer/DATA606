#' Plot a normal distribution.
#'
#' Plots a nomral distribution with the area within the given bounds shaded.
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
	x <- seq(-4,4,length=100)*sd + mean
	hx <- dnorm(x,mean,sd)

	plot(x, hx, type="n", xlab="x-Axis", ylab="",
		 main="Normal Distribution", axes=FALSE)
	lines(x, hx)

	if(tails) {
		i.low <- x <= bounds[1]
		i.high <- x >= bounds[2]
		polygon(c(x[i.low],bounds[1]), c(hx[i.low], 0), col="red")
		polygon(c(bounds[2],x[i.high]), c(0,hx[i.high]), col="red")
	} else {
		i <- x >= bounds[1] & x <= bounds[2]
		polygon(c(bounds[1],x[i],bounds[2]), c(0,hx[i],0), col="red")
		area <- pnorm(bounds[2], mean, sd) - pnorm(bounds[1], mean, sd)
		if(diff(bounds) > 0) {
			result <- paste("P(",bounds[1],"< x <",bounds[2],") =",
							signif(area, digits=3))
			mtext(result,3)
		}
	}
	axis(1)
}
