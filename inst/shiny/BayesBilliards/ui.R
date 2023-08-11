shinyUI(navbarPage(title='Bayes Billiards Balls',

	tabPanel('Home',

		sidebarLayout(
			sidebarPanel(width=3,
				sliderInput('confidence', 'Confidence Range:',
							min=5, max=95, value=50),
				checkboxInput('show8ball', 'Show Pool Table', value=FALSE),
				radioButtons('plotType', 'Distribution Plot Type',
							 c('Density', 'Histogram'), selected='Density'),
				actionButton('nextIteration', 'Next Ball'),
				actionButton('startOver', 'Start Over'),
				checkboxInput('setsamples', 'Adjust number of samples per iteration'),
				conditionalPanel('input.setsamples == true',
					sliderInput('samples', 'Number of samples:',
								min=50, max=10000, value=init.samples)
				)
			),

			mainPanel(width=9,
				conditionalPanel(
					condition="input.show8ball == true",
					plotOutput('plot', height='175')
				),
				fluidRow(
					column(width=6,
						   h4(textOutput('summary.text')), br(),
						   h4('Summary Table'), tableOutput('summary')),
					column(width=6,
						   h4(textOutput('plot.title')), plotOutput('conf.plot'))
				),
				h4('Distributions'),
				uiOutput('dist.plot.ui')
			)
		)
	),

	tabPanel('About', htmlOutput('about'))
))
