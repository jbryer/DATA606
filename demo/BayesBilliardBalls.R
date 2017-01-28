set.seed(2112)
ball <- runif(1)
ball

(balls[1] <- ifelse(runif(1) < ball, 'left', 'right'))

prior <- runif(1000)
hist(prior, main='Prior Distribution')

posterior <- character(length(prior))
for(i in seq_along(prior)) {
	posterior[i] <- ifelse(runif(1) < prior[i], 'left', 'right')
}
hist(prior[posterior == balls[1]], main='Posterior Distribution')
mean(prior[posterior == balls[1]])
fivenum(prior[posterior == balls[1]])[c(2,4)] # 50% chance the position is between these values
quantile(prior[posterior == balls[1]], c(0.25, 0.75))
quantile(prior[posterior == balls[1]], c(0.05, 0.95))

# Try with two balls
# We'll consider 0 = left and 1 = right
(balls[2] <- ifelse(runif(1) < ball, 'left', 'right'))
posterior2 <- matrix(nrow=length(prior), ncol=2)
for(i in seq_along(prior)) {
	posterior2[i,1] <- ifelse(runif(1) < prior[i], 0, 1)
	posterior2[i,2] <- ifelse(runif(1) < prior[i], 0, 1)
}
posterior2.sum <- apply(posterior2, 1, sum)
hist(prior[posterior2.sum == 0])
mean(prior[posterior2.sum == 0])
fivenum(prior[posterior2.sum == 0])[c(2,4)] # 50% chance the position is between these values


# Using the posterior as the prior for the second iteration
prior2 <- prior[posterior == balls[1]]
posterior2 <- character(length(prior))
for(i in seq_along(prior2)) {
	posterior2[i] <- ifelse(runif(1) < prior2[i], 'left', 'right')
}
hist(prior2[posterior2 == balls[2]])
mean(prior2[posterior2 == balls[2]])
fivenum(prior2[posterior2 == balls[2]])[c(2,4)]
