img.8ball <- rasterGrob(readPNG('ball.png'), interpolate=TRUE)

# NOTE: Throughout 0 = left, 1 = right
getPosterior <- function(position, prior) {
	posterior <- as.integer(runif(length(prior)) > prior)
	return(prior[posterior == position])
}

init.ball <- runif(1)
init.balls <- runif(1)
init.priors <- list(runif(init.samples))
init.posteriors <- list(getPosterior(as.integer(init.balls[1] > init.ball), init.priors[[1]]))

shinyServer(function(input, output) {
	billiards <- reactiveValues(
		ball = init.ball,
		balls = init.balls,
		priors = init.priors,
		posteriors = init.posteriors
	)

	observeEvent(input$nextIteration, {
		newBall <- runif(2)[2] # HACK: on initial run, runif would give the same
		                       #       result as init.ball above
		nextPos <- length(billiards$balls) + 1
		prior <- billiards$posteriors[[(nextPos - 1)]]
		# Replicate the prior so the length is close to the number of desired
		# samples. Better method would be to sample a new distribution with
		# same center and spread of the last posterior.
		prior <- sample(prior, input$samples, replace=TRUE)
		posterior <- getPosterior(as.integer(newBall > billiards$ball), prior)
		billiards$balls[nextPos] <- newBall
		billiards$priors[[nextPos]] <- prior
		billiards$posteriors[[nextPos]] <- posterior
	})

	observeEvent(input$startOver, {
		billiards$ball <- runif(1)
		billiards$balls <- runif(1)
		billiards$priors <- list(runif(isolate(input$samples)))
		billiards$posteriors <- list(getPosterior(as.integer(billiards$balls[1] > billiards$ball),
												  billiards$priors[[1]]))
	})

	table <- reactive({
		tab <- data.frame(Iteration = integer(),
						  Position=character(),
						  Mean = numeric(),
						  Median = numeric(),
						  Min = numeric(),
						  Max = numeric())
		conf.level <- input$confidence / 100
		for(i in seq_len(length(billiards$balls))) {
			conf <- quantile(billiards$posteriors[[i]],
							 c(0.5 - (conf.level / 2),
							   0.5 + (conf.level / 2)))
			tab <- rbind(tab, data.frame(
				Iteration = i,
				Position = ifelse(billiards$balls[i] < billiards$ball, 'left', 'right'),
				Mean = mean(billiards$posteriors[[i]]),
				Median = median(billiards$posteriors[[i]]),
				Min = conf[1],
				Max = conf[2]
			))
		}
		tab
	})

	output$summary.text <- renderText({
		paste0('The ', ordinal(length(billiards$balls)), ' ball landed to the ',
			   ifelse(billiards$balls[length(billiards$balls)] < billiards$ball,
			   	   'left', 'right'), ' of the 8-ball')
	})

	output$summary <- renderTable({ table()	}, include.rownames=FALSE)

	output$plot.title <- renderText({
		paste0('Mean and ', input$confidence, '% Confidence Intervals')
	})

	output$plot <- renderPlot({
		df <- data.frame(iter = seq_along(billiards$balls),
						 loc = billiards$balls)
		df$color <- ifelse(df$loc < billiards$ball, 'left', 'right')
		ggplot(df, aes(x=loc, color=color, y=0.025)) +
			geom_rect(xmin=0, xmax=1, ymin=0, ymax=1, color='darkgreen', fill='darkgreen') +
			annotation_custom(img.8ball, ymin=0, ymax=0.1,
							  xmin=(billiards$ball-0.05), xmax=(billiards$ball+0.05)) +
			geom_point(size=4, color='white') +
			geom_point(size=3) +
			geom_text(aes(label=iter), y=0.007, color='white') +
			geom_text(label=round(billiards$ball, digits=2),
					  x=billiards$ball, y=0.08, color='white') +
			xlim(c(0,1)) + ylim(c(0, .1)) +
			xlab('Position') + ylab('') +
			scale_color_manual('', values=c('left'='red', 'right'='blue')) +
			theme(axis.text.y=element_blank(), axis.title.y=element_blank(),
				  axis.ticks.y=element_blank(), axis.line.y=element_blank(),
				  legend.position='none',
				  panel.background=element_rect(fill='white'),
				  panel.grid=element_blank())
	})

	output$conf.plot <- renderPlot({
		tab <- table()
		p <- ggplot(tab, aes(x=Mean, y=factor(Iteration))) +
			geom_errorbarh(aes(xmin=Min, xmax=Max), color='darkgreen') +
			geom_point(size=3) +
			xlim(c(0,1)) + ylab('Iteration') +
			theme(legend.position='none')
		if(input$show8ball) {
			p <- p + geom_vline(xintercept=billiards$ball)
		}
		return(p)
	})

	output$dist.plot.ui <- renderUI({
		output$dist.plot <- renderPlot({
			df <- data.frame(Iteration=integer(),
							 dist=character(),
							 prob=numeric())
			for(i in seq_along(billiards$balls)) {
				df <- rbind(df, data.frame(Iteration = rep(i,
													  length(billiards$priors[[i]])),
										   dist = rep('Prior', length(billiards$priors[[i]])),
										   prob = billiards$priors[[i]]))
				df <- rbind(df, data.frame(Iteration = rep(i,
													  length(billiards$posteriors[[i]])),
										   dist = rep('Posterior', length(billiards$posteriors[[i]])),
										   prob = billiards$posteriors[[i]]))
			}
			df$Iteration <- factor(df$Iteration, levels=rev(unique(df$Iteration)),
								   labels = paste0('Iteration ', rev(unique(df$Iteration))))
			if(input$plotType == 'Density') {
				p <- ggplot(df, aes(x=prob, group=dist, fill=dist)) +
					geom_density() + xlim(c(0,1)) +
					facet_wrap(~ Iteration + dist, ncol=2) +
					xlab('Probability') + ylab('Density')
			} else {
				p <- ggplot(df, aes(x=prob, group=dist, fill=dist)) +
					geom_histogram() + xlim(c(0,1)) +
					facet_wrap(~ Iteration + dist, ncol=2) +
					xlab('Probability') + ylab('Count')
			}
			p <- p + theme(legend.position='none')
			if(input$show8ball) {
				p <- p + geom_vline(xintercept=billiards$ball)
			}
			return(p)
		})
		plotOutput('dist.plot', height=(length(billiards$balls) * 200))
	})

	output$about <- renderRmd('about.Rmd', input)
})
