library(shiny)
library(tidyverse)

x_range <- c(-4, 4)
funs <- list(
	Normal = function(x, mean = 0, sigma = 1) {
		1 / (sigma * sqrt(2 * pi)) * exp(1)^(-1/2 * ( (x - mean) / sigma )^2)
	}

	# Binomial = function(x, trials = 2, p = 0.5) {
	# 	choose(n, x) * p^x * (1 - p)^(n - x)
	# }
)

riemann <- function(f, min, max, n = 2, ...) {
	width <- (max - min) / n
	boxes <- tibble(
		xmin = seq(min, min + (n-1) * width, by = width),
		height = f(xmin, ...),
		area = height * width
	)
	return(boxes)
}

ui <- fluidPage(
    titlePanel("Reimann Sums"),

    sidebarLayout(
        sidebarPanel(
        	selectInput('fun',
        				label = 'Function',
        				choices = names(funs)),
        	conditionalPanel(
        		condition = "input.fun == 'Binomial'",
        		sliderInput('trials', label = 'Number of trials',
        					min = 1, max = 100, value = 2),
        		numericInput('p_success', label = 'Probability of success',
        					 value = 0.5, min = 0, max = 1, step = 0.1)
        	),
        	sliderInput("range",
        				label = 'Range',
        				min = -4, max = 4,
        				value = c(0, 2),
        				step = 0.1),
        	sliderInput("boxes",
        				label = 'Number of boxes',
        				min = 1, max = 200,
        				step = 1, value = 3)
        ),

        mainPanel(
           plotOutput("plot"),
           p('Using the integrate function:', textOutput('area'))
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
    	width <- abs(diff(input$range) / input$boxes)
    	df <- NULL
    	boxes <- NULL
    	if(input$fun == 'Binomial') {
    		boxes <- riemann(funs[[input$fun]],
    						 min = input$range[1],
    						 max = input$range[2],
    						 n = input$boxes,
    						 trials = input$trials,
    						 p = input$p_success)
    		df <- tibble(x = seq(0, 3, by = 0.01),
    					 y = funs[[input$fun]](x, trials = input$trials, p = input$p_success))
    	} else {
    		boxes <- riemann(funs[[input$fun]],
    						 min = input$range[1],
    						 max = input$range[2],
    						 n = input$boxes)
    		df <- tibble(x = seq(x_range[1], x_range[2], by = 0.01),
	    				 y = funs[[input$fun]](x))
    	}
    	ggplot() +
    		geom_rect(data = boxes, aes(xmin = xmin, ymin = 0, xmax = xmin + width, ymax = height),
    				  alpha = 0.5, color = 'black') +
    		geom_path(data = df, aes(x = x, y = y)) +
    		xlim(c(x_range[1], x_range[2])) +
    		ggtitle(paste0('Aera ≈ ', prettyNum(sum(boxes$area)), digits = 4))

    })

    output$area <- renderPrint({
    	integrate(funs[[input$fun]], input$range[1], input$range[2])
    })
}

shinyApp(ui = ui, server = server)
