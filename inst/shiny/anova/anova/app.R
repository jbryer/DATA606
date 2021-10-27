library(shiny)
library(tidyverse)
library(psych)

ui <- fluidPage(
    titlePanel("Graphical One-Way Analysis of Variance (ANOVA)"),

    sidebarLayout(
        sidebarPanel(
        	selectInput('dataset',
        				'Select a dataset: ',
        				choices = c('handwashing', 'anorexia', 'npk', 'simulate'),
        				selected = 'handwashing'),
        	hr(),
        	checkboxGroupInput('plot_features',
        					   'Plot Features:',
        					   choices = c(
        					   		'Unit Line' = 'unit_line',
        					   		'Grand Mean' = 'grand_mean',
        					   		'Group Standard Deviation' = 'group_sd',
        					   		'Group Variance' = 'group_variances',
        					   		'Mean Square Within (Error)' = 'ms_within',
        					   		'Mean Square Between (Treatment)' = 'ms_between'
        					   )),
        	hr(),
        	conditionalPanel('input.dataset == "simulate"',
	        	sliderInput('k',
	        				'Number of groups:',
	        				min = 2, max = 10,
	        				value = 3, step = 1),
	        	numericInput('n',
	        				 'n per group:',
	        				 min = 2, max = 1000,
	        				 value = 10, step = 1),
	        	uiOutput('mean_ui'),
	        	numericInput('sd',
	        				 'Standard Deviation:',
	        				 value = 3),
	        	actionButton('resample', 'Resample')
        	),
        	conditionalPanel('input.dataset != "simulate"',
        		uiOutput('mean_adjust_ui')
        	)
        ),

        mainPanel(
           plotOutput("plot", height = '600px')
        )
    )
)

server <- function(input, output, session) {

	getData <- reactive({
		req(input$dataset)
		input$resample

		df <- NULL
		if(input$dataset == 'simulate') {
			req(input$n)
			req(input$k)
			req(input$mean1)
			group_means <- numeric(input$k)
			for(i in 1:input$k) {
				group_means[i] <- input[[paste0('mean', i)]]
			}
			df <- data.frame(
				Group = rep(LETTERS[1:input$k], each = input$n),
				Value = as.numeric(sapply(group_means, FUN = function(x) { rnorm(input$n, mean = x, sd = input$sd) }))
			)
		} else if(input$dataset == 'handwashing') {
			hand <- structure(list(
				Bacterial.Counts = c(74, 84, 70, 51, 135,
									 51, 164, 5, 102, 110, 88, 19, 124, 67, 111, 18, 105,
									 119, 73, 58, 139, 108, 119, 50, 170, 207, 20, 82,
									 87, 102, 95, 17),
				Method = c("Water", "Soap", "Antibacterial Soap",
						   "Alcohol Spray", "Water", "Soap", "Antibacterial Soap", "Alcohol Spray",
						   "Water", "Soap", "Antibacterial Soap", "Alcohol Spray", "Water",
						   "Soap", "Antibacterial Soap", "Alcohol Spray", "Water", "Soap",
						   "Antibacterial Soap", "Alcohol Spray", "Water", "Soap", "Antibacterial Soap",
						   "Alcohol Spray", "Water", "Soap", "Antibacterial Soap", "Alcohol Spray",
						   "Water", "Soap", "Antibacterial Soap", "Alcohol Spray")),
				class = "data.frame", row.names = c(NA, -32))
			df <- data.frame(
				Group = hand$Method,
				Value = as.integer(hand$Bacterial.Counts)
			)
		} else if(input$dataset == 'anorexia') {
			data(anorexia, package = 'MASS')
			df <- data.frame(
				Group = anorexia$Treat,
				Value = anorexia$Postwt - anorexia$Prewt
			)
		} else if(input$dataset == 'npk') {
			data(npk, package = 'datasets')
			df <- data.frame(
				Group = npk$block,
				Value = npk$yield
			)
		}

		return(df)
	})

	output$mean_ui <- renderUI({
		inputs <- list()
		for(i in 1:input$k) {
			inputs[[paste0('mean_', i)]] <- numericInput(paste0('mean', i),
														 paste0('Group ', LETTERS[i], ' mean:'),
														 value = i,
														 step = 1)
		}
		return(inputs)
	})

	getGroupName <- function(name) {
		gsub(' ', '_', name)
	}

	output$mean_adjust_ui <- renderUI({
		input$dataset
		inputs <- list()
		isolate(df <- getData())
		grand_sd <- sd(df$Value)
		for(i in unique(df$Group)) {
			value <- mean(df[df$Group == i,]$Value)
			isolate(
				if(!is.null(input[[paste0('mean_adjust_', getGroupName(i))]])) {
					value <- input[[paste0('mean_adjust_', getGroupName(i))]]
				}
			)

			inputs[[paste0('mean_adjust_', getGroupName(i))]] <- sliderInput(paste0('mean_adjust_', getGroupName(i)),
															   paste0('Adjust mean for ', i, ' by:'),
															   value = value,
															   min = round(-1 * grand_sd * 4),
															   max = round(grand_sd * 4) )
		}

		inputs[['adjust_reset']] <- actionButton('adjust_reset', 'Reset Adjustments')

		return(inputs)
	})

	observeEvent(input$adjust_reset, {
		isolate(df <- getData())
		for(i in unique(df$Group)) {
			value <- mean(df[df$Group == i,]$Value)
			updateSliderInput(
				session = session,
				inputId = paste0('mean_adjust_', getGroupName(i)),
				value = value)
		}
	})

    output$plot <- renderPlot({
    	df <- getData()


    	if(input$dataset != 'simulate') {
    		for(i in unique(df$Group)) {
    			if(!is.null(input[[paste0('mean_adjust_', getGroupName(i))]])) {
    				df[df$Group == i,]$Value <- df[df$Group == i,]$Value +
    					input[[paste0('mean_adjust_', getGroupName(i))]] - mean(df[df$Group == i,]$Value)
    			}
    		}
    	}


    	desc <- describeBy(df$Value, group = df$Group, mat = TRUE, skew = FALSE)
    	names(desc)[2] <- 'Group'
    	desc$Var <- desc$sd^2
    	desc$contrast <- (desc$mean - mean(desc$mean)) #/ sd(desc$mean)

    	df <- merge(df, desc[,c('Group', 'contrast', 'mean')],
    				by = 'Group', all.x = TRUE)

    	k <- length(unique(df$Group))
    	n <- nrow(df)

    	grand_mean <- mean(df$Value)
    	grand_var <- var(df$Value)
    	pooled_var <- mean(desc$Var)

    	ss_total <- sum((df$Value - grand_mean)^2)
    	df_between <- k - 1
    	ss_between <- sum(desc$n * (desc$mean - grand_mean)^2)
    	MS_between <- ss_between / df_between

    	df_within <- n - k
    	ss_within <- ss_total - ss_between
    	MS_within <- ss_within / df_within

    	F_stat <- MS_between / MS_within

    	p <- 1 - pf(F_stat, df_between, df_within)

    	slope <- (desc[1,]$mean - desc[2,]$mean) / (desc[1,]$contrast - desc[2,]$contrast)
    	intercept <- desc[1,]$mean - slope * desc[1,]$contrast

    	df_rect <- data.frame(
    		`Mean Square` = c('Between', 'Within'),
    		# contrast = NA,
    		# Value = NA,
    		xmin = c(-1* sqrt(MS_between),
    				 -1 *sqrt(MS_within)),
    		xmax = c(    sqrt(MS_between),
    					 sqrt(MS_within)),
    		ymin = c(grand_mean - 1 * sqrt(MS_between),
    				 grand_mean - 1 * sqrt(MS_within)),
    		ymax = c(grand_mean +     sqrt(MS_between),
    				 grand_mean +     sqrt(MS_within)) )

    	df_rect_within <- df %>%
    		mutate(square = (Value - mean)^2) %>%
    		group_by(Group) %>%
    		summarize(contrast = mean(Value) - grand_mean,
    				  mean = mean(Value),
    				  MS = sum(square) / (n() - 1)) %>%
    		mutate(xmin = contrast - sqrt(MS),
    			   xmax = contrast + sqrt(MS),
    			   ymin = mean - sqrt(MS),
    			   ymax = mean + sqrt(MS))

    	df_subscript <- paste0(df_between, ', ', df_within)
    	title <- bquote(F[.(df_subscript)] == .(prettyNum(F_stat, digits = 3)) ~ '; p' ~ .(ifelse(p < 0.01, ' < 0.01', paste0(' = ', prettyNum(p, digits = 3)))))

    	p <- ggplot()
    	if('group_variances' %in% input$plot_features) {
	    	p <- p + geom_rect(data = df_rect_within,
	    					   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = Group, color = Group),
	    					   alpha = 0.05, linetype = 2)
    	}
    	if('group_sd' %in% input$plot_features) {
	    	p <- p + geom_segment(data = desc,
	    						  aes(x = contrast, xend = contrast, y = mean - sd, yend = mean + sd),
	    						  alpha = 0.6)
    	}

    	if('ms_within' %in% input$plot_features) {
    		p <- p + geom_rect(data = df_rect[2,],
    						   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
    						   alpha = 0.4, fill = '#fdc086')

    	}

    	if('ms_between' %in% input$plot_features) {
    		p <- p + geom_rect(data = df_rect[1,],
    						   aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
    						   alpha = 0.1, fill = '#7fc97f')
    	}

    		# geom_hline(yintercept = c(grand_mean - sd(df$Value), grand_mean + sd(df$Value)),
    		# 		   linetype = 5, color = 'maroon', alpha = 0.5) +
    		# geom_hline(yintercept = c(grand_mean - mean(desc$sd), grand_mean + mean(desc$sd)),
    		# 		   linetype = 2, color = 'darkgreen', alpha = 0.5) +

    	if('unit_line' %in% input$plot_features) {
    		p <- p + geom_abline(slope = slope, intercept = intercept, color = 'grey70')
    	}

    	if('grand_mean' %in% input$plot_features) {
    		p <- p +
    			geom_hline(yintercept = mean(df$Value), alpha = 0.5, linetype = 2, size = 1) +
    			geom_vline(xintercept = 0, alpha = 0.5, linetype = 2, size = 1) +
    			geom_point(aes(x = 0, y = grand_mean), color = 'blue', size = 4)
    	}

    	xlim <- c(-1.1 * max(2 * sqrt(MS_between), diff(range(df$Value)) ) / 2,
    			  1.1 * max(2 * sqrt(MS_between), diff(range(df$Value))) / 2)
    	ylim <- c(1.1 * (grand_mean - max(2 * sqrt(MS_between), diff(range(df$Value))) / 2),
    			  1.1 * (grand_mean + max(2 * sqrt(MS_between), diff(range(df$Value))) / 2))

    	p <- p +
    		geom_point(data = df, aes(x = contrast, y = Value, group = Group, color = Group),
    				   alpha = 0.75, shape = 1, size = 2) +
    		geom_point(data = desc, aes(x = contrast, y = mean, color = Group), size = 3) +
    		geom_text(data = desc, aes(label = Group, x = contrast, y = min(df$Value)),
    				  angle = 90, hjust = 0, vjust = -0.8) +
    		ggtitle(title) +
    		xlim(xlim) + ylim(ylim) +
    		xlab('Contrast Coefficient') + ylab('Dependent Variable') + coord_equal() +
    		theme_minimal() + theme(panel.grid.major = element_line(color = 'grey90', size = 0.3),
    								panel.grid.minor = element_blank(),
    								legend.position = 'bottom')

    	p
    })
}

shinyApp(ui = ui, server = server)
