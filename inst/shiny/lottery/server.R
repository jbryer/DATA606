require(shiny)
require(shinyIncubator)
require(ggplot2)

theme_update(panel.background=element_blank(), 
			 panel.grid.major=element_blank(), 
			 panel.border=element_blank())

tickets <- as.data.frame(rbind(
	c(    '$1',    1,     15),
	c(    '$2',    2,     11),
	c(    '$4',    4,     62),
	c(    '$5',    5,    100),
	c(   '$10',   10,    143),
	c(   '$20',   20,    250),
	c(   '$30',   30,    562),
	c(   '$50',   50,   3482),
	c(  '$100',  100,   6681),
	c(  '$500',  500,  49440),
	c('$1500',  1500, 375214),
	c('$2500',  2500, 618000)
), stringsAsFactors=FALSE)
names(tickets) <- c('Winnings', 'Value', 'Odds')
tickets$Value <- as.integer(tickets$Value)
tickets$Odds <- as.integer(tickets$Odds)

shinyServer(function(input, output) {
	data <- NULL
	#totals <- data.frame
	
	newrun <- reactive({
		if(input$reload.data > 0 | TRUE) {
			odds <- sample(max(tickets$Odds), input$games, replace=TRUE)
			vals <- rep(-1, length(odds))
			for(i in 1:nrow(tickets)) {
				#Subtract the cost of the ticket
				vals[odds %% tickets[i,'Odds'] == 0] <- tickets[i,'Value'] - 1 
			}
			df <- data.frame(Odds=odds, Value=vals, x=1:length(vals))
			df$y <- cumsum(df$Value)
			if(is.null(data)) {
				data <<- df
				data$run <<- 1
			} else {
				df$run <- max(data$run) + 1
				data <<- rbind(data, df)
			}
			return(list(history=data, current=df))
		}
	})
	
	output$tickets <- renderTable({
		tickets
	})
	
	output$plot <- renderPlot({
		mydata <- newrun()$current
		history <- newrun()$history
		range <- c(-max(abs(c(mydata$y, history$y))), 
					max(abs(c(mydata$y, history$y))))
		p <- ggplot() + 
			geom_line(data=history, aes(x=x, y=y, group=run), color='black', alpha=.2) +
			geom_hline(yintercept=0, colour='blue') + 
			geom_line(data=mydata, aes(x=x, y=y)) + 
			geom_point(data=mydata[mydata$Value > 0,], 
					   aes(x=x,y=y,color=paste0('$', (Value+1))),
					   size=2, vjust=-1) +
			scale_color_brewer('Winnning Value', labels=tickets$Winnings,
							   breaks=tickets$Winnings, palette='Dark2') +
			ylim(range) +
			ylab('Cumulative Win/Loss in Dollars') + 
			xlab('Game Sequence')
		print(p)
		
	}, height=400)
	
	output$results <- renderText({
		mydata <- newrun()$current
		total <- mean(mydata[mydata$x == max(mydata$x),'y'])
		return(paste0('Average ', ifelse(total < 0, 'losses', 'winnings'), ' after ',
					  nrow(mydata), ' games is $', prettyNum(abs(total), digits=1)))
	})
})
