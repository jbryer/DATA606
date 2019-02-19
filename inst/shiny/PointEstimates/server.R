library(shiny)
library(ggplot2)

cols <- c('Population' = '#cfcfcf',
		  'Frequentist' = '#1b9e77',
		  'Prior' = '#7570b3',
		  'Bayesian' = '#d95f02',
		  'Bootstrap' = '#386cb0')

n_bayes_samples <- 1000
n_bootstrap_samples <- 1000

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
	getSample <- reactive({
		input$resample
		thedata <- data.frame(i = 1:input$sampleSize,
							  x = rnorm(input$sampleSize,
							  		    mean = input$populationMean,
							  		    sd = input$populationSD),
							  stringsAsFactors = FALSE)
		return(thedata)
	})

	output$plot <- renderPlot({
		thedata <- getSample()
		mean_sample <- mean(thedata$x)
		sd_sample <- sd(thedata$x)

		# Bayesian estimates
		prior <- rnorm(n_bayes_samples, mean = 0, sd = sd_sample)
		likfun <- function(theta) {
			sapply( theta, function(t) prod( dnorm(thedata$x, t, sd_sample) ) )
			# TODO: Re-evaluate the prior SD. For now, assuming the same SD as the sample
		}
		tmp <- likfun(prior)
		posterior <- sample(prior, n_bayes_samples, replace=TRUE, prob=tmp )
		bayes_estimate <- mean(posterior)
		bayes_interval <- quantile(posterior, c(0.025, 0.975))

		bootstrap_samples <- numeric(n_bootstrap_samples)
		for(i in seq_len(n_bootstrap_samples)) {
			tmp <- sample(thedata$x, nrow(thedata), replace = TRUE)
			bootstrap_samples[i] <- mean(tmp)
		}
		bootstrap_estimate <- mean(bootstrap_samples)
		bootstrap_interval <- quantile(bootstrap_samples, c(0.025, 0.975))

		# Position of labels below the line y = 0
		label.pos.x <- max(mean_sample + 1.96 * (sd_sample) / sqrt(input$sampleSize),
						   bayes_interval[2],
						   bootstrap_interval[2])

		# Setup the plot
		p <- ggplot(thedata, aes(x = x)) +
			geom_vline(xintercept = input$populationMean, linetype = 2) +
			geom_vline(xintercept = 0) +
			geom_hline(yintercept = -0.5) +
			geom_vline(xintercept = mean_sample, linetype = 3) +
			geom_point(y = -0.4, alpha = 0.25) +
			xlim(min(c(input$populationMean - 3 * input$populationSD, thedata$x)),
				 max(c(input$populationMean + 3 * input$populationSD, thedata$x)) )

		if(input$showPopulation) {
			p <- p +
				stat_function(fun = dnorm,
							  args = list(mean = input$populationMean,
							  			  sd = input$populationSD),
							  color = cols['Population'], fill = cols['Population'],
							  geom = 'area', alpha = 0.25)
		}

		##### Frequentist
		if(input$showFrequentist) {
			p <- p +
				stat_function(fun = dnorm,
							  args = list(mean = mean_sample,
							  			sd = (sd_sample) / sqrt(input$sampleSize) ),
							  color = cols['Frequentist'], fill = cols['Frequentist'],
							  geom = 'area', alpha = 0.25) +
				geom_segment(x = mean_sample - 1.96 * (sd_sample / sqrt(input$sampleSize)),
							 xend = mean_sample + 1.96 * (sd_sample) / sqrt(input$sampleSize),
							 y = -0.1, yend = -0.1,
							 color = cols['Frequentist'], alpha = 0.25, size = 1) +
				geom_point(x = mean_sample, y = -0.1, color = cols['Frequentist'], size = 3) +
				geom_text(x = label.pos.x, y = -0.1, hjust = -0.1, size = 4,
						  label = 'Frequentist Estimate',
						  color = cols['Frequentist'])
		}

		##### Boostrap
		if(input$showBootstrap) {
			p <- p +
				geom_density(data = data.frame(x = bootstrap_samples),
							 aes(x = x), alpha = 0.25,
							 color = cols['Bootstrap'], fill = cols['Bootstrap']) +
				geom_segment(x = bootstrap_interval[1],
							 xend = bootstrap_interval[2],
							 y = -0.2, yend = -0.2,
							 color = cols['Bootstrap'], size = 1) +
				geom_point(x = bootstrap_estimate, y = -0.2, color = cols['Bootstrap'], size = 3) +
				geom_text(x = label.pos.x, y = -0.2, hjust = -0.1, size = 4,
						  label = 'Bootstrap Estimate', color = cols['Bootstrap'])
		}

		##### Bayesian
		if(input$showBayesianPrior) {
			p <- p +
				stat_function(fun = dnorm,
							  args = list(mean = 0, sd = sd_sample),
							  color = cols['Prior'], fill = cols['Prior'],
							  geom = 'area', alpha = 0.25)
		}

		if(input$showBayesianPosterior) {
			p <- p +
				# stat_function(fun = dnorm,
				# 		  args = list(mean = bayes_estimate, sd = sd(posterior)),
				# 		  color = cols['Bayesian'], fill = cols['Bayesian'],
				# 		  geom = 'area', alpha = 0.24) +
				geom_density(data = data.frame(x = posterior),
							 aes(x = x), alpha = 0.25,
							 color = cols['Bayesian'], fill = cols['Bayesian']) +
				geom_segment(x = bayes_interval[1],
							 xend = bayes_interval[2],
							 y = -0.3, yend = -0.3,
							 color = cols['Bayesian'], size = 1) +
				geom_point(x = bayes_estimate, y = -0.3, color = cols['Bayesian'], size = 3) +
				geom_text(x = label.pos.x, y = -0.3, hjust = -0.1, size = 4,
						  label = 'Bayesian Estimate', color = cols['Bayesian'])
		}

		p <- p + ylab("") + xlab('Estimate') +
			theme_minimal() +
			theme(axis.text.y = element_blank())

		##### Annotations
		if(input$showLegend) {
			xrange <- layer_scales(p)$x$range$range
			yrange <- layer_scales(p)$y$range$range
			x_annotate <- xrange[2] - 2 * (diff(xrange) / 5)
			y_annotate <- yrange[2]
			if(input$populationMean < 0) {
				x_annotate <- xrange[1]
			}

			p <- p + annotate("text", hjust = 0, x = x_annotate, y = y_annotate,
						 label = "Prior = P(mean difference)", color = cols['Prior']) +
				annotate("text", hjust = 0, x = x_annotate, y = y_annotate,
						 label = "\n\nPosterior = P(mean difference | data)", color = cols['Bayesian']) +
				annotate("text", hjust = 0, x = x_annotate, y = y_annotate,
						 label = "\n\n\n\nLikelihood = P(data | mean difference)", color = cols['Frequentist']) +
				annotate("text", hjust = 0, x = x_annotate, y = y_annotate,
						 label = paste0("\n\n\n\n\n\n\n\nPopulation mean = ", input$populationMean,
						 			   "\nSample mean = ", round(mean_sample, digits = 2),
						 			   "\nSample SD = ", round(sd_sample, digits = 2)))
		}

		return(p)
	})
})
