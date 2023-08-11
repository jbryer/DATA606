shinyUI(navbarPage(title='Bayes Billiards Balls',
	tabPanel('Home',

		sidebarLayout(
			sidebarPanel(width=3,
				sliderInput('confidence', 'Confidence Range:',
							min=5, max=95, value=50),
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

			mainPanel(width=9
			)
		)
	),
	tabPanel('About', htmlOutput('about'))
))
