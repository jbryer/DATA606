if(!require(devtools) | !require(shiny) | !require(shinyIncubator) | !require(ggplot2)) {
	install.packages(c('devtools','ggplot2','shiny'))
	require(devtools)
	install_github("shiny-incubator", "rstudio")
	require(shiny)
	require(shinyIncubator)
	require(ggplot2)
}

shinyServer(function(input, output) {
	data <- reactive({
		if(input$reload.data > 0 | TRUE) {
			data <- data.frame(x=integer(), y=integer(), run=integer())
			for(i in seq_len(input$runs)) {
				vals <- sample(input$odds, input$games, replace=TRUE)
				vals <- ifelse(vals==1, 1, -1)
				data <- rbind(data, data.frame(
					x = 1:length(vals), 
					y = cumsum(vals),
					run = rep(i, length(vals)) ) )
			}
			return(data)
		}
	})
	
	output$table <- renderTable({
		data()
	})
	
	output$plot <- renderPlot({
		mydata <- data()
		range <- c( -max(abs(mydata$y)), max(abs(mydata$y)))
		p <- ggplot(mydata, aes(x=x, y=y, color=run, group=run)) + 
			geom_hline(yintercept=0, colour='blue') + 
			geom_line() + 
			ylim(range) +
			ylab('Cumulative Sum') + 
			xlab('Game Sequence') +
			theme(legend.position='none')
		print(p)
		
	}, height=400)
	
	output$results <- renderText({
		mydata <- data()
		total <- mean(mydata[mydata$x == max(mydata$x),'y'])
		return(paste0('Average ', ifelse(total < 0, 'losses', 'winnings'), ' after ',
			   input$games, ' games is ', prettyNum(abs(total), digits=1)))
	})
})
