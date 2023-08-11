library(dplyr)
library(tidyr)
# library(quantmod) # for getSymbols for share price data
# library(ggseas)   # for nzbop data
library(ggplot2)
library(reshape2)
library(readxl)
library(shiny)

source('dualplot.R')

# Import some of that data into R and create a numeric TimePeriod variable from the original
# string that shows year and month:
forex <- read_excel("graphdata.xlsx", sheet = "8_NZDUSD", skip = 4) %>%
	mutate(year = as.numeric(substring(DATE, 1, 4)),
		   month = as.numeric(substring(DATE, 6, 7)),
		   TimePeriod = year + (month - 0.5) / 12) %>%
	select(-DATE, -year, -month)
names(forex)[1:2] <- c("NZDUSD", "TWI")

server <- function(input, output, session) {
	output$dualPlot <- renderPlot({
		dualplot(x1 = forex$TimePeriod,
				 y1 = forex$NZDUSD,
				 y2 = forex$TWI,
				 colgrid = "grey90",
				 ylab1 = 'NZDUSD',
				 ylab2 = 'TWI',
				 ylim1 = c(input$NZDUSD.ymin, input$NZDUSD.ymax),
				 ylim2 = c(input$TWI.ymin, input$TWI.ymax))

	})

	output$scatterPlot <- renderPlot({
		ggplot(forex, aes(x=NZDUSD, y=TWI)) + geom_point() +
			xlim(input$NZDUSD.ymin, input$NZDUSD.ymax) +
			ylim(input$TWI.ymin, input$TWI.ymax) +
			geom_smooth(method='lm')
	})

	output$correlation <- renderPrint({
		print(cor.test(forex$NZDUSD, forex$TWI))
	})

	output$zscorePlot <- renderPlot({
		forex$NZDUSD.z <- (forex$NZDUSD - mean(forex$NZDUSD)) / sd(forex$NZDUSD)
		forex$TWI.z <- (forex$TWI - mean(forex$TWI)) / sd(forex$TWI)
		forex.melted <- melt(forex[,c('TimePeriod','NZDUSD.z', 'TWI.z')], id.vars='TimePeriod')

		z.mapping <- data.frame(z = seq(-2, 2, by=.5), NZDUSD = NA, TWI = NA)
		z.mapping$NZDUSD <- mean(forex$NZDUSD) + z.mapping$z * sd(forex$NZDUSD)
		z.mapping$TWI <- mean(forex$TWI) + z.mapping$z * sd(forex$TWI)

		ggplot(forex.melted, aes(x=TimePeriod, y=value, group=variable, color=variable)) +
			geom_line() + ylab('Z-Score') +
			scale_y_continuous(breaks = z.mapping$z,
							   labels = paste0('z-score = ', z.mapping$z,
							   				'\nNZDUSD = ', round(z.mapping$NZDUSD, digits=2),
							   				'\nTWI = ', round(z.mapping$TWI, digits=2)))
	})

	output$densityPlot <- renderPlot({
		forex.melted <- melt(forex[,c('TimePeriod','NZDUSD', 'TWI')], id.vars='TimePeriod')

		ggplot(forex.melted, aes(x=value)) + geom_density() +
			facet_wrap(~ variable, ncol=1, scales='free')
	})

	observeEvent(input$resetSliders, {
		updateSliderInput(session, 'NZDUSD.ymin', value=min(forex$NZDUSD))
		updateSliderInput(session, 'NZDUSD.ymax', value=max(forex$NZDUSD))
		updateSliderInput(session, 'TWI.ymin', value=min(forex$TWI))
		updateSliderInput(session, 'TWI.ymax', value=max(forex$TWI))
	})
}

ui <- fluidPage(
		sidebarLayout(
			sidebarPanel(
				sliderInput("NZDUSD.ymin", "NZDUSD minimum:", min = 0, max = 1,
							value = min(forex$NZDUSD), step=0.05),
				sliderInput("NZDUSD.ymax", "NZDUSD minimum:", min = 0, max = 1,
							value = max(forex$NZDUSD), step=0.05),
				hr(),
				sliderInput("TWI.ymin", "TWI minimum:", min = 0, max = 200,
							value = min(forex$TWI)),
				sliderInput("TWI.ymax", "TWI minimum:", min = 0, max = 200,
							value = max(forex$TWI)),
				actionButton('resetSliders', 'Set Sliders to Default')
			),
			mainPanel(tabsetPanel(
				tabPanel('Dual Scales',
						 plotOutput("dualPlot")
				),
				tabPanel('Scatter Plot',
						 plotOutput('scatterPlot')
				),
				tabPanel('Correlation',
						 verbatimTextOutput('correlation')),
				tabPanel('Density',
						 plotOutput('densityPlot')),
				tabPanel('Z-Score',
						 plotOutput('zscorePlot'))
			))
		)
)

shinyApp(ui = ui, server = server)
