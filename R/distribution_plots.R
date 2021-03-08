#' Plots a distribution
#'
#' @param fun the density function (e.g. dnorm)
#' @param color the color to fill in the area.
#' @param limits the limits of the plot.
#' @param cv the critical value(s) to shade.
#' @param tails
#' @param ... other parameters passed to \code{fun}.
#' @export
distribution_plot <- function(fun,
							  cv,
							  tails = c('no', 'two.sided', 'less', 'greater'),
							  limits,
							  color = 'steelblue',
							  ...) {
	p <- ggplot() +
		geom_function(fun = fun,
					  args = list(...),
					  xlim = limits) +
		xlim(limits)

	title <- ''
	if(tails[1] == 'no') {
		area <- integrate(fun, cv[1], cv[2], ...)$value
		title <- paste("P(", cv[1], "< x <", cv[2], ")",
					   ifelse(area < 0.01,
					   	      '< 0.01',
					   	      paste0('≈ ', signif(area, digits = 3))))

		p <- p + stat_function(fun = fun,
					  geom = 'area',
					  xlim = cv,
					  args = list(...),
					  fill = color)
	} else if(tails[1] == 'two.sided') {
		if(length(cv) > 1) {
			warning('cv should have one value for tails = "two.sided"')
		}
		cv <- c(cv[1], -1 * cv[1])
		cv <- cv[order(cv)]
		area <- 1 - integrate(fun, cv[1], cv[2], ...)$value
		title <- paste("P(x < ", cv[1], " & x >", cv[2], ")",
					   ifelse(area < 0.01,
					   	   '< 0.01',
					   	   paste0('≈ ', signif(area, digits = 3))))
		p <- p + stat_function(fun = fun,
							   geom = 'area',
							   xlim = c(limits[1], cv[1]),
							   args = list(...),
							   fill = color)
		p <- p + stat_function(fun = fun,
							   geom = 'area',
							   xlim = c(cv[2], limits[2]),
							   args = list(...),
							   fill = color)
	} else if(tails[1] == 'less') {
		if(length(cv) > 1) {
			warning('cv should have one value for tails = "less"')
		}
		area <- integrate(fun, -Inf, cv[1], ...)$value
		title <- paste("P(x < ", cv[1], ")",
					   ifelse(area < 0.01,
					   	   '< 0.01',
					   	   paste0('≈ ', signif(area, digits = 3))))
		p <- p + stat_function(fun = fun,
							   geom = 'area',
							   xlim = c(limits[1], cv[1]),
							   args = list(...),
							   fill = color)
	} else if(tails[1] == 'greater') {
		if(length(cv) > 1) {
			warning('cv should have one value for tails = "greater"')
		}
		area <- integrate(fun, cv[1], Inf, ...)$value
		title <- paste("P(x > ", cv[1], ")",
					   ifelse(area < 0.01,
					   	   '< 0.01',
					   	   paste0('≈ ', signif(area, digits = 3))))
		p <- p + stat_function(fun = fun,
							   geom = 'area',
							   xlim = c(cv[1], limits[2]),
							   args = list(...),
							   fill = color)
	} else {
		stop('Tails parameter not set or unknown.')
	}

	p <- p + ggtitle(title) + xlab('') + ylab('') +
		theme(axis.text.y = element_blank(),
			  axis.ticks.y = element_blank())

	return(p)
}

# binomial_plot <- function(size, prob, log = FALSE,
# 						  cv = 2,
# 						  tail = TRUE,
# 						  limits = c(0, 10),
# 						  color = 'steelblue') {
# 	cv <- cv
# 	if(!tail) { cv < c(0, cv) }
# 	# Doesn't work with the distribution_plot framework
# 	# distribution_plot(fun = dbinom,
# 	# 				  x = 1:size,
# 	# 				  size = size,
# 	# 				  prob = prob,
# 	# 				  log = log,
# 	# 				  tails = ifelse(tail, 'greater', 'no'),
# 	# 				  cv = cv,
# 	# 				  limits = limits,
# 	# 				  color = color)
# }

#' Plot a beta distribution
#'
#' @export
beta_plot <- function(shape1, shape2,
					  cv = .5,
					  tail = TRUE,
					  limits = c(0, 1),
					  color = 'steelblue') {
	distribution_plot(fun = dbeta,
					  shape1 = shape1,
					  shape2 = shape2,
					  tails = 'no',
					  cv = c(0, cv),
					  limits = limits,
					  color = color)
}

#' Plot a chi-squared distribution
#'
#' @inheritParams distribution_plot
#'
#' @seealso [stats::dchisq()]
#' @export
#' @md
chisquare_plot <- function(df = 2,
					   cv = 2,
					   tails = 'greater',
					   limits = c(0, 10),
					   color = 'steelblue') {
	if(tails != 'greater') { cv = c(0, cv) }
	distribution_plot(fun = dchisq,
					  df = df,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}

#' Plot a F distribution.
#'
#' @param cf critical value (or F-statistic).
#' @inheritParams distribution_plot
#' @seealso [stats::df()]
#' @export
#' @md
F_plot <- function(df1 = 1, df2 = 5,
				   cv = 2,
				   tails = 'greater',
				   limits = c(0, 10),
				   color = 'steelblue') {
	if(tails != 'greater') { cv = c(0, cv) }
	distribution_plot(fun = df,
					  df1 = df1,
					  df2 = df2,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}

#' Plot a student t distribution.
#'
#' @inheritParams distribution_plot
#' @export
t_plot <- function(df = 10,
				   cv = c(-1, 1),
				   tails = 'no',
				   limits = c(mean - 3 * sd, mean + 3 * sd),
				   color = 'steelblue') {
	distribution_plot(fun = dt,
					  df = df,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}

#' Plot a normal distribution.
#'
#' Plots a normal distribution with the area within the given cv shaded.
#' The probability (i.e. area) under the normal curve for the given cv
#' is also given.
#'
#' @param mean mean of the distribution.
#' @param sd standard deviation of the distribution.
#' @inheritParams distribution_plot
#' @export
#' @examples
#' 	normal_plot()
#' 	normal_plot(cv=c(-2,2))
#' 	normal_plot(cv=c(-3,3))
#' 	normal_plot(tails=TRUE)
normal_plot <- function(mean = 0,
						sd = 1,
						cv = c(-1, 1),
						tails = 'no',
						limits = c(mean - 3 * sd, mean + 3 * sd),
						color = 'steelblue') {
	distribution_plot(fun = dnorm,
					  mean = mean,
					  sd = sd,
					  cv = cv,
					  tails = tails,
					  limits = limits,
					  color = color)
}
