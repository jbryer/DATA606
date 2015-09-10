if(!require(shiny) | !require(shinyIncubator) | !require(ggplot2)) {
	install.packages(c('devtools','ggplot2','shiny'))
	require(devtools)
	install_github("shiny-incubator", "rstudio")
}

shinyUI(pageWithSidebar(
	# Application title
	headerPanel("Gambler's Run"),
	
	# Sidebar with controls to select the variable to plot against mpg
	# and to specify whether outliers should be included
	sidebarPanel(
		helpText(paste0("Selection the number of games to play (x axis), ",
						"the odds of winning, and the number of runs ",
						"(i.e. number of lines).")),
		sliderInput("games", "Number of games:", 
					min=2, max=1000, value=100),
		sliderInput("odds", "Odds of winning (1:n):",
		            min=2, max=12, value=2),
		sliderInput("runs", "Number of runs:",
					min=1, max=20, value=1),
		br(),
		actionButton('reload.data','Start Over') #From shinyIncubator package
	),
	
	# Show the caption and plot of the requested variable against mpg
	mainPanel(
		tabsetPanel(
			tabPanel("Plot",
					 h3(textOutput("results")),
					 plotOutput("plot")
			),
			tabPanel("Table", 
					 tableOutput("table")
			)
			#tabPanel("Edit",
			#		 matrixInput("foo", "foo", mtcars))
		)
	)
))
