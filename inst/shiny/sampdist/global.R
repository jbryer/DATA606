library(shiny)
#library(shinythemes)

# BCD COMMENT: THis app was originally created by Kyle Hardman.  See the "about" tab for a link.

# reduced global.R components to a minimum set that are not reactive

setScreenMar <- function(screen) {
	screen(screen)
	par(mar = c(6, 4, 2, 1))
}

getSSCellMatrix <- function(nrow, ncol) {
	screenCells <- matrix(0, nrow = nrow * ncol, ncol = 4)
	for (i in 1:nrow) {
		for (j in 1:ncol) {
			xj <- j
			yi <- 4 - i

			x <- c((xj - 1) / ncol, xj / ncol)
			y <- c((yi - 1) / nrow, yi / nrow)

			screenCells[(i - 1) * ncol + j,] <- c(x[1], x[2], y[1], y[2])
		}
	}
	screenCells
}

mergeCells <- function(cells, i1, i2) {
	dim1 <- cells[i1, ]
	dim2 <- cells[i2, ]

	mins <- pmin(dim1, dim2)
	maxs <- pmax(dim1, dim2)

	cells[i1,] <- c(mins[1], maxs[2], mins[3], maxs[4])

	cells[-i2,]
}

getScreenCells <- function() {
	cellMat <- getSSCellMatrix(3, 4)
	cellMat <- mergeCells(cellMat, 7, 8)
	cellMat <- mergeCells(cellMat, 5, 6)
	cellMat <- mergeCells(cellMat, 3, 4)
	cellMat <- mergeCells(cellMat, 1, 2)
	cellMat <- mergeCells(cellMat, 2, 4)
	cellMat <- mergeCells(cellMat, 1, 3)
	finalCellMat <- cellMat
}

countsInBreaks <- function(x, breaks) {
	rval <- rep(NA, length(breaks) - 1)
	for (i in 1:(length(breaks) - 1)) {
		rval[i] <- sum(x >= breaks[i] & x < breaks[i + 1])
	}
	rval
}

longBreaks <- function(x, n) {
	breaks <- pretty(x, n = n)

	d <- breaks[2] - breaks[1]
	l <- length(breaks)
	breaks <- c(rep(min(breaks), l), breaks, rep(max(breaks), l)) + (d * c(-(l:1), rep(0, l), 1:l))

	breaks
}


getStatFun <- function(newStatFunName) {
	statFunName <- newStatFunName
	if (statFunName == "mean") {
		statFun <- mean
	} else if (statFunName == "median") {
		statFun <- median
	} else if (statFunName == "standard deviation") {
		statFun <- sd
	} else if (statFunName == "variance") {
		statFun <- function(x) {
			var(as.vector(x))
		}
	} else if (statFunName == "range") {
		statFun <- function(x) {
			max(x) - min(x)
		}
	} else {
		stop("Invalid stat function selected in getStatFun().")
	}
	return(statFun)
}

getDistInfo <- function(normMu = 100, normSigma = 15, expRate = 1 / 10,
						unifLower = 1, unifUpper = 6
) {
	distInfo = list()

	#Set up the normal distribution
	distInfo$normal = list()

	distInfo$normal$mu = normMu
	distInfo$normal$sigma = normSigma
	distInfo$normal$densFun = function(x) {
		dnorm(x, normMu, normSigma)
	}
	distInfo$normal$realizationFun = function(count) {
		rnorm(count, normMu, normSigma)
	}

	distInfo$normal$stats = list()
	distInfo$normal$stats$mean = function() {
		normMu
	}
	distInfo$normal$stats$median = function() {
		normMu
	}
	distInfo$normal$stats$variance = function() {
		normSigma ^ 2
	}
	distInfo$normal$stats[["standard deviation"]] = function() {
		normSigma
	}
	distInfo$normal$stats$range = function() {
		Inf
	}

	#Set up the exponential distribution
	distInfo$exponential = list()

	distInfo$exponential$rate = 1 / 10
	distInfo$exponential$densFun = function(x) {
		dexp(x, rate = expRate)
	}
	distInfo$exponential$realizationFun = function(count) {
		rexp(count, rate = expRate)
	}

	distInfo$exponential$stats = list()
	distInfo$exponential$stats$mean = function() {
		1 / expRate
	}
	distInfo$exponential$stats$median = function() {
		1 / expRate * log(2)
	}
	distInfo$exponential$stats$variance = function() {
		1 / expRate ^ 2
	}
	distInfo$exponential$stats[["standard deviation"]] = function() {
		1 / expRate
	}
	distInfo$exponential$stats$range = function() {
		Inf
	}

	#Set up the uniform distribution
	distInfo$uniform = list()

	distInfo$uniform$lowerBound = unifLower
	distInfo$uniform$upperBound = unifUpper
	distInfo$uniform$densFun = function(x) {
		dunif(x, unifLower, unifUpper)
	}
	distInfo$uniform$realizationFun = function(count) {
		runif(count, unifLower, unifUpper)
	}

	distInfo$uniform$stats = list()
	distInfo$uniform$stats$mean = function() {
		(unifUpper + unifLower) / 2
	}
	distInfo$uniform$stats$median = function() {
		(unifUpper + unifLower) / 2
	}
	distInfo$uniform$stats$variance = function() {
		1 / 12 * (unifUpper - unifLower) ^ 2
	}
	distInfo$uniform$stats[["standard deviation"]] = function() {
		sqrt(distInfo$uniform$stats$variance())
	}
	distInfo$uniform$stats$range = function() {
		unifUpper - unifLower
	}

	return(distInfo)
}

