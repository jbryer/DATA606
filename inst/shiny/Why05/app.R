library(shiny)
library(tidyverse)
library(MASS)

n <- 16 # Number of observations per sample
nSamples <- 15 # Number of samples to take

ui <- fluidPage(
    title = "Why p < 0.05?",
    titlePanel("Why p < 0.05?"),
    fluidRow(
    	column(12,
    		   p('For each plot determine whether there is a clear, upward trend of the data.'),
    		   p('H_0: There is no real trend. Any apparent trend is due to chance', br(),
    		     'H_1: There is a real, upward trend')
    		   )
    ),
    hr(),
    uiOutput('ui_input')
)

server <- function(input, output) {
	current_iteration <- reactiveVal(1)
	r <- NULL
	samples <- list()
	results <- data.frame()

	initialize <- function() {
		current_iteration(1)
		r <- seq(0.1, 0.6, length.out = nSamples) %>% sample()
		samples <- list()
		results <<- data.frame()
		for(i in 1:nSamples) {
			sim.r <- r[i]
			Sigma <- matrix(c(1,sim.r,sim.r,1),2,2)
			sample <- mvrnorm(n = n,
							  mu = rep(0, 2),
							  Sigma = Sigma,
							  empirical = TRUE) %>%
				as.data.frame(sample)
			names(sample) <- c('X', 'Y')
			test <- cor.test(sample[,1], sample[,2])

			sample$Iteration <- i
			sample$r <- cor(sample$X, sample$Y)
			sample$p <- test$p.value
			samples[[i]] <<- list(sample = sample,
								 r = sim.r,
								 p = test$p.value)
			results <<- rbind(results, data.frame(
				r = sim.r,
				p = test$p.value,
				response = NA
			))
		}
	}

	observeEvent(input$start_over, {
		initialize()
	})

	observeEvent(input$positive, {
		results[current_iteration(),]$response <<- TRUE
		current_iteration(current_iteration() + 1)
	})

	observeEvent(input$negative, {
		results[current_iteration(),]$response <<- FALSE
		current_iteration(current_iteration() + 1)
	})

	output$progress <- renderText({
		progress <- round( (current_iteration()-1) / nSamples * 100)
		paste0('<div class="progress">',
			   '<div class="progress-bar" role="progressbar" style="width: ',
			   progress, '%;" aria-valuenow="', progress, '" aria-valuemin="0" aria-valuemax="100">',
			   progress, '%</div></div>')
	})

    output$scatter_plot <- renderPlot({
    	if(length(samples) < nSamples) {
    		initialize()
    	}

    	p <- NULL
    	if(current_iteration() <= nSamples) {
	    	sample <- samples[[current_iteration()]]$sample
    		p <- ggplot(sample, aes(x = X, y = Y)) +
    			geom_point() +
    			coord_equal() +
    			xlim(c(-3, 3)) + ylim(c(-3, 3))
    	} else {
    		allsamples <- data.frame()
    		for(i in 1:nSamples) {
    			allsamples <- rbind(allsamples, samples[[i]]$sample)
    		}
    		p <- allsamples %>% dplyr::arrange(r) %>%
    			ggplot( aes(x = X, y = Y, color = p <= 0.05)) +
    			geom_point() + facet_wrap(~ round(p, digits = 3)) +
    			coord_equal()
    	}
    	return(p)
    })

    output$results_table <- renderTable({
    	results %>% dplyr::arrange(p) %>% return()
    })

	output$ui_input <- renderUI({
		ui <- list()
		if(current_iteration() <= nSamples) {
			ui[[length(ui)+1]] <- fluidRow(
				column(4, actionButton('positive', 'There is an upward trend')),
				column(4, actionButton('negative', 'There is no trend')),
				column(4, actionButton('start_over', 'Start Over'))
			)
			ui[[length(ui)+1]] <- br()
			ui[[length(ui)+1]] <- fluidRow(
				column(12, htmlOutput('progress'))
			)
		} else {
			ui[[length(ui)+1]] <- fluidRow(
				column(8),
				column(4, actionButton('start_over', 'Start Over'))
			)
			ui[[length(ui)+1]] <- tableOutput('results_table')
		}
		ui[[length(ui)+1]] <- plotOutput('scatter_plot')

		return(ui)
	})
}

# Run the application
shinyApp(ui = ui, server = server)
