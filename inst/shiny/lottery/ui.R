require(shiny)
require(ggplot2)

shinyUI(pageWithSidebar(
	# Application title
	headerPanel("Lottery Tickets"),

	sidebarPanel(
		helpText(paste0('This application will simulate buying a series of lottery ',
						'tickets. For example, the default starting point of 365 ',
						'is meant to simulate buying one lottery ticket a day for ',
						'a year. The "Odds" tab provides the exact odds of winning ',
						'each ticket. Clicking the "New Run" button will simulate ',
						'another "year" of buying tickets showing wins along the way ',
						'and the total winnings or losses at the end. Past runs ',
						'will be saved and plotted in light grey to show how the ',
						'current run compares to previous runs.')),
		sliderInput("games", "Number of tickets:",
					min=2, max=365*10, value=365),
		br(),
		actionButton('reload.data','New Run'),
		actionButton('reload10', '10 Runs')
	),

	mainPanel(
		tabsetPanel(
			tabPanel("Plot",
					 h3(textOutput("results")),
					 plotOutput("plot")
			),
			tabPanel("Histogram",
					 plotOutput('histogram')
			),
			tabPanel("Odds",
					 tableOutput("tickets")
			)
		)
	)
))
