library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
    titlePanel("Correlation Exploration"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("position",
                        "Row (observation):",
                        min = 1,
                        max = n,
            			step = 1,
                        value = 1,
            			animate = TRUE),
            numericInput('n', label = 'Sample Size', min = 5, max = 500, value = 40),
            actionButton('resample', 'Resample')
        ),
        mainPanel(
        	tabsetPanel(
        		tabPanel('Scatter Plot',
        				 plotOutput('scatter_plot')),
        		tabPanel('p-values',
        				 plotOutput('p_plot')),
        		tabPanel('Data',
        				 tableOutput('thedata'))
        	)
        )
    )
)

# Define Server
server <- function(input, output) {
	getData1 <- reactive({
		input$resample
		df <- data.frame(#a = runif(n, min = 0, 5),
			a = rnorm(input$n),
			b = rep(0, input$n))
		df <- df[order(df$a),]
		return(df)
	})

	getData <- reactive({
		df <- getData1()
		df$b <- 0
		df[input$position,]$b <- 1
		return(df)
	})

	output$scatter_plot <- renderPlot({
    	df <- getData()
    	test <- cor.test(df$a, df$b)
    	ggplot(df, aes(x = b, y = a)) +
    		geom_point() +
    		geom_smooth(formula = y ~ x, method = 'lm', se = FALSE) +
    		ggtitle(paste0('cor = ', round(test$statistic, digits = 2),
    					   ' p = ', round(test$p.value, digits = 3)))
    })

	output$p_plot <- renderPlot({
		df <- getData()
		df$p <- NA_real_
		for(i in 1:nrow(df)) {
			df$b <- 0
			df[i,]$b <- 1
			test <- cor.test(df$a, df$b)
			df[i,]$p <- test$p.value
		}

		ggplot(df, aes(x = a, y = p, color = p < 0.05)) +
			geom_point() +
			geom_hline(yintercept = 0.05)
	})

    output$thedata <- renderTable({
    	df <- getData()
    	df$Row <- 1:nrow(df)
    	return(df)
    })
}

# Run App
shinyApp(ui = ui, server = server)
