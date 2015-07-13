set.seed(2112)

sample(c(-1,1), size=1, replace=TRUE)

coins <- sample(c(-1,1), size=100, replace=TRUE)
coins

cumsum(coins)
cumsum(coins)[length(coins)]

nsamples <- 1000
n <- 100
trials <- integer(nsamples)
for(i in seq_len(nsamples)) {
	coins <- sample(c(-1,1), size=100, replace=TRUE)
	trials[i] <- cumsum(coins)[length(coins)]
}

mean(trials)
sd(trials)

hist(trials)
qqnorm(trials); qqline(trials)

sd1 <- trials[trials >= mean(trials) - sd(trials) &
	          trials <= mean(trials) + sd(trials)]
sd2 <- trials[trials >= mean(trials) - 2 * sd(trials) &
			  trials <= mean(trials) + 2 * sd(trials)]
sd3 <- trials[trials >= mean(trials) - 3 * sd(trials) &
			  trials <= mean(trials) + 3 * sd(trials)]

length(sd1) / length(trials)
length(sd2) / length(trials)
length(sd3) / length(trials)

source('R/normalPlot.R')

normalPlot(mean=mean(trials), sd=sd(trials), 
		   bounds=c(mean(trials) - sd(trials),
		   		    mean(trials) + sd(trials)))

normalPlot(bounds=c(-1,1))
normalPlot(bounds=c(-2,2))
normalPlot(bounds=c(-3,3))




# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
  ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
  labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
