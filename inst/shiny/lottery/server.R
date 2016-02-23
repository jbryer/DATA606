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
	data <- reactiveValues(
		history = NULL,
		current = NULL
	)

	newrun <- function(games) {
		odds <- sample(max(tickets$Odds), games, replace=TRUE)
		vals <- rep(-1, length(odds))
		for(i in 1:nrow(tickets)) {
			#Subtract the cost of the ticket
			vals[odds %% tickets[i,'Odds'] == 0] <- tickets[i,'Value'] - 1
		}
		df <- data.frame(Odds=odds, Value=vals, x=1:length(vals))
		df$y <- cumsum(df$Value)
		if(is.null(data$history)) {
			df$run <- 1
			data$history <- df
			data$current <- df
		} else {
			df$run <- max(data$history$run) + 1
			data$history <- rbind(data$history, df)
			data$current <- df
		}
		invisible(df)
	}

	getData <- reactive({
		if(is.null(data$history)) {
			newrun(365)
		}
		return(list(current = data$current, history = data$history))
	})

	observeEvent(input$reload10, {
		for(i in 1:10) { newrun(input$games) }
	})

	observeEvent(input$reload.data, {
		newrun(input$games)
	})

	output$tickets <- renderTable({
		tickets
	})

	output$plot <- renderPlot({
		mydata <- getData()$current
		history <- getData()$history
		range <- c(-max(abs(c(mydata$y, history$y))),
					max(abs(c(mydata$y, history$y))))
		p <- ggplot() +
			geom_line(data=history, aes(x=x, y=y, group=run), color='black', alpha=.2) +
			geom_hline(yintercept=0, colour='blue') +
			geom_line(data=mydata, aes(x=x, y=y)) +
			geom_point(data=mydata[mydata$Value > 0,],
					   aes(x=x,y=y,color=paste0('$', (Value+1))),
					   size=2) +
			scale_color_brewer('Winnning Value', labels=tickets$Winnings,
							   breaks=tickets$Winnings, palette='Dark2') +
			ylim(range) +
			ylab('Cumulative Win/Loss in Dollars') +
			xlab('Game Sequence')
		print(p)
	}, height=400)

	output$histogram <- renderPlot({
		history <- getData()$history
		history <- history[!duplicated(history$run, fromLast=TRUE),]
		history$Winning <- ifelse(history$y > 0, 'Yes','No')
		avg <- mean(history$y)
		p <- ggplot(history, aes(x=y, fill=Winning)) +
			geom_histogram(binwidth=20) +
			geom_vline(xintercept=0) +
			geom_vline(xintercept=avg, linetype=2) +
			xlim(c(min(history$y) - 50, max(max(history$y), 0) + 50)) +
			scale_fill_manual(values=c('No'='red','Yes'='green')) +
			xlab(paste0('Winning / Loss (Average ', ifelse(avg < 0, 'loss', 'winning'),
						' of $', round(abs(avg)), ')')) +
				 	ylab('Count') +
			ggtitle(paste0('Distribution of Winnings and Losses After a Sequence of ',
						   input$games, ' Tickets\n(n = ', max(history$run), ')'))
		print(p)
	}, height=400)

	output$results <- renderText({
		mydata <- getData()$current
		total <- mean(mydata[mydata$x == max(mydata$x),'y'])
		return(paste0('Average ', ifelse(total < 0, 'losses', 'winnings'), ' after ',
					  nrow(mydata), ' games is $', prettyNum(abs(total), digits=1)))
	})
})
