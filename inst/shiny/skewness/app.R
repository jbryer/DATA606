library(shiny)
library(shiny)
library(fGarch)

N <- 10000

ui <- fluidPage(
    titlePanel("Skewness"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("skewness",
                        "Skewness",
                        min = 1,
                        max = 10,
                        value = 1,
            			step = 0.25),
            numericInput('mean',
            			 "Mean",
            			 value = 0),
            numericInput('sd',
            			 'Standard Deviation',
            			 value = 1,
            			 min = 0.5,
            			 step = 1)
        ),

        mainPanel(
           plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
    	# x <- rnbinom(N, 10, .1)
    	x <- rsnorm(N,
    				mean = input$mean,
    				sd = input$sd,
    				xi = input$skewness)

    	ggplot(data.frame(x = x), aes(x = x)) +
    		geom_histogram(aes(y = ..density..), bins = 20, fill = 'grey70') +
    		geom_density() +
    		geom_vline(data = data.frame('Measure' = c('Median', 'Mean'),
    									 'Value' = c(median(x), mean(x))),
    				   aes(xintercept = Value, color = Measure), size = 1.5) +
    		ggtitle(paste0('mean = ', prettyNum(mean(x), digits = 3),
    					  '\nmedian = ', prettyNum(median(x), digits = 3)))

    })
}

shinyApp(ui = ui, server = server)
