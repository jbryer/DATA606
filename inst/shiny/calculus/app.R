library(shiny)
library(tidyverse)
library(Deriv)

default_fun <- c('Normal' = '1 / (sqrt(2 * pi)) * exp(1)^(-1/2 * (x)^2)',
				 'Cubic' = '0.25 * x^3 + 0.25 * x^2 + 0.49 * x + 5')

if(FALSE) { # Test
	f <- eval(parse(text = paste0('f <- function(x) { ', default_fun[1], '}')))
	f(2)
}

x_range <- c(-4, 4)

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
    titlePanel("Calculus Demo"),

    sidebarLayout(
        sidebarPanel(
        	selectInput('calc_type', label = 'Choose...', choices = c('Integrals', 'Derivatives')),
        	selectInput('functions', label = 'Functions', choices = names(default_fun)),
        	uiOutput('fun'),
        	uiOutput('min_input'),
        	uiOutput('max_input'),
        	hr(),
        	conditionalPanel(condition = 'input.calc_type == "Integrals"',
	        	uiOutput('range_input'),
	        	sliderInput("boxes",
	        				label = 'Number of boxes',
	        				min = 1, max = 200,
	        				step = 1, value = 3)
        	),
        	conditionalPanel(condition = 'input.calc_type == "Derivatives"',
        		uiOutput('point_input'),
        		# numericInput('interval', label = 'Interval',
        		# 			 min = -5, max = 5, value = -1, step = 0.1)
        		sliderInput('interval', label = 'Interval',
        					 min = -5, max = 5, value = -1, step = 0.1)
        	)
        ),

        mainPanel(
        	textOutput('error_msg'),
        	plotOutput("plot"),
        	conditionalPanel(condition = 'input.calc_type == "Integrals"',
        					 p('Using the integrate function:', textOutput('area'))
        	),
        	conditionalPanel(condition = 'input.calc_type == "Derivatives"'
        	)
        )
    )
)

server <- function(input, output) {
	error_message <- reactiveVal('')

	output$error_msg <- renderText({
		error_message()
	})

	getFunction <- reactive({
		tryCatch({
			eval(parse(text = paste('f <- function(x) { return(' , input$fun , ')}', sep='')))
			error_message('')
		}, error = function(e) {
			print(e)
			error_message('Error in function specification.')
		})
		return(f)
	})

	output$min_input <- renderUI({
		numericInput('view_min', label = 'Plot min', value = -4, step = 1,
					 min = -100, max = 0)
	})

	output$max_input <- renderUI({
		numericInput('view_max', label = 'Plot max', value = 4, step = 1,
					 min = 0, max = 100)
	})

	output$range_input <- renderUI({
		req(input$view_min); req(input$view_max)
		sliderInput("range",
					label = 'Range',
					min = input$view_min, max = input$view_max,
					value = c(0, 2),
					step = 0.1)
	})

	output$point_input <- renderUI({
		req(input$view_min); req(input$view_max)
		sliderInput('point',
					label = 'Point',
					value = input$view_min + (input$view_max - input$view_min) / 4,
					min = input$view_min,
					max = input$view_max,
					step = 0.2)
	})

	output$fun <- renderUI({
		textInput('fun', label = 'Function', value = default_fun[input$functions])
	})

	output$plot <- renderPlot({
		req(input$view_min); req(input$view_max)
		f <- getFunction()
		if(is.null(f)) { return(); }

		p <- NULL
		if(input$calc_type == 'Integrals') {
			req(input$range); req(input$boxes)

	    	width <- abs(diff(input$range) / input$boxes)
	    	boxes <- riemann(f,
							 min = input$range[1],
							 max = input$range[2],
							 n = input$boxes)
	    	p <- ggplot() +
	    		geom_rect(data = boxes, aes(xmin = xmin, ymin = 0, xmax = xmin + width, ymax = height),
	    				  alpha = 0.5, color = 'black') +
	    		stat_function(fun = f) +
	    		xlim(c(input$view_min, input$view_max)) +
	    		ggtitle(paste0('Aera ≈ ', prettyNum(sum(boxes$area)), digits = 4))
		} else if(input$calc_type == 'Derivatives') {
			req(input$point); req(input$interval)

			# dx <- Deriv(f, "x") # TODO: Why doesn't this work!

			df_segment <- data.frame(x1 = input$point + input$interval,
									 x2 = input$point,
									 y1 = f(input$point + input$interval),
									 y2 = f(input$point) )

			segment_slope <- (df_segment$y2 - df_segment$y1) / (df_segment$x2 - df_segment$x1)

			p <- ggplot() +
				stat_function(fun = f) +
				# geom_abline(slope = dx(x)[1], intercept = f(x) - dx(x)[1] * x, color = 'blue') +
				geom_segment(data = df_segment,
							 aes(x = x1, y = y1, xend = x2, yend = y2),
							 color = 'darkgreen', linetype = 2) +
				geom_abline(slope = segment_slope,
							intercept = f(input$point) - segment_slope * input$point,
							color = 'darkgreen') +
				geom_point(aes(x = df_segment$x1, y = df_segment$y1), color = 'darkgreen', size = 2) +
				geom_point(aes(x = df_segment$x2, y = df_segment$y2), color = 'darkgreen', size = 2) +
				geom_point(aes(x = input$point, y = f(input$point)), color = 'blue', size = 3) +
				xlim(c(input$view_min, input$view_max)) +
				ggtitle(paste0('Slope ≈ ', segment_slope))
		}
		return(p)
    })

    output$area <- renderPrint({
    	req(input$range)
    	f <- getFunction()
    	if(is.null(f)) { return(); }
    	integrate(f, input$range[1], input$range[2])
    })
}

shinyApp(ui = ui, server = server)
