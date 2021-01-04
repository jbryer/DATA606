library(shiny)
library(psych)
library(reshape2)
library(ggplot2)

ui <- fluidPage(

    # Application title
    titlePanel("Resampling Distributions"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,
        	numericInput('meanA', 'Group A Mean:', value = 10, step = 1),
        	numericInput('meanB', 'Group B Mean:', value = 11, step = 1),
        	numericInput('sd', 'Standard Deviation:', value = 4, step = 1),
        	numericInput('nA', 'Group A n:', value = 30, min = 2, max = 10000),
        	numericInput('nB', 'Group B n:', value = 30, min = 2, max = 10000),
            sliderInput("samples",
                        "Number of Samples:",
                        min = 100,
                        max = 10000,
                        value = 100,
            			step = 100),
        	checkboxInput('showOriginal', label = 'Show Distributions', value = TRUE),
        	checkboxInput('showCLT', label = 'Show CLT Distribution', value = TRUE),
        	checkboxInput('showBootstrap', label = 'Show Bootstrap', value = FALSE),
        	checkboxInput('showPermutation', label = 'Show Permutation Null', value = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 9,
           plotOutput("plot")
        )
    )
)

server <- function(input, output) {
	formu <- value ~ group

	getData <- reactive({
		thedata <- data.frame(
			group = c(rep('A', input$nA), rep('B', input$nB)),
			value = c(rnorm(input$nA, mean = input$meanA, sd = input$sd),
					  rnorm(input$nB, mean = input$meanB, sd = input$sd))
		)
		return(thedata)
	})

	getSamples <- reactive(({
		thedata <- getData()

		x.var <- all.vars(formu)[2]
		y.var <- all.vars(formu)[1]

		t.result <- t.test(formu, data = thedata)
		test.stat <- unname(diff(t.result$estimate))
		test.se <- sd(thedata[,y.var]) / sqrt(nrow(thedata))

		df <- data.frame(iter = 1:input$samples)
		df$bootstrap <- as.numeric(NA)
		df$permutation <- as.numeric(NA)
		withProgress(message = 'Resampling...', value = 0, min = 0, max = nrow(df), {
			for(i in 1:nrow(df)) {
				boot.sample <- sample(nrow(thedata), size = nrow(thedata), replace = TRUE)
				boot.t.result <- t.test(formu, data = thedata[boot.sample,])

				# NOTE: There are n! possible permutations, therefore we cannot try them
				# all. Here, we are randomly shuffling the dependent variable. Technically,
				# we should not reuse any combination of the dependent variable, but given
				# the small percentage of combinations tried the chance of duplicates is small
				# and is ignored for simplicity.
				perm.sample <- sample(0:1, size = nrow(thedata), replace = TRUE)
				perm.estimate <- diff(describeBy(thedata[,y.var],
												 group = perm.sample,
												 mat = TRUE,
												 skew = FALSE)$mean)

				df[i,]$bootstrap <- diff(boot.t.result$estimate)
				df[i,]$permutation <- perm.estimate

				setProgress(i, detail = paste(round(i / nrow(df) * 100), '%'))
			}
		})
		return(df)
	}))

    output$plot <- renderPlot({
    	thedata <- getData()
    	df <- getSamples()

    	x.var <- all.vars(formu)[2]
    	y.var <- all.vars(formu)[1]

    	t.result <- t.test(formu, data = thedata)
    	test.stat <- unname(diff(t.result$estimate))
    	test.se <- sd(thedata[,y.var]) / sqrt(nrow(thedata))

    	# tmp <- melt(df, id.vars = 'iter')
    	# tmp <- rbind(tmp,
    	# 			 data.frame(iter = 1, variable = 'original', value = thedata[,y.var]),
    	# 			 data.frame(iter = 1, variable = 'A', value = thedata[thedata[,x.var] == 'A', y.var]),
    	# 			 data.frame(iter = 1, variable = 'B', value = thedata[thedata[,x.var] == 'B', y.var]))

    	p <- ggplot()
    	p <- p + geom_vline(xintercept = diff(t.result$estimate), linetype = 2)
    	p <- p + geom_vline(xintercept = mean(df$bootstrap), linetype = 3)

    	if(input$showOriginal) {
    		tmp <- rbind(data.frame(iter = 1, variable = 'original', value = thedata[,y.var]),
    					 data.frame(iter = 1, variable = 'A', value = thedata[thedata[,x.var] == 'A', y.var]),
    					 data.frame(iter = 1, variable = 'B', value = thedata[thedata[,x.var] == 'B', y.var]))
    		p <- p + geom_density(data = tmp, aes(x = value, color = variable))
    	}
    	if(input$showCLT) {
    		p <- p + stat_function(fun = dnorm, n = 1001, args = list(mean = test.stat, sd = test.se), color = 'black')
    	}
    	if(input$showBootstrap) {
    		tmp2 <- melt(df, id.vars = 'iter')
    		p <- p + geom_density(data = tmp2[tmp2$variable == 'bootstrap',],
    							  aes(x = value, color = variable))
    	}
    	if(input$showPermutation) {
    		tmp3 <- melt(df, id.vars = 'iter')
    		p <- p + geom_density(data = tmp3[tmp3$variable == 'permutation',],
    							  aes(x = value, color = variable))
    	}

     	return(p)
    }, height = 600)
}

shinyApp(ui = ui, server = server)
