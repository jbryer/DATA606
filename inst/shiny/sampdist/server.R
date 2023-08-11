# BCD COMMENT: THis app was originally created by Kyle Hardman.  See the "about" tab for a link.

shinyServer(function(input, output, clientData, session) {
  userData <- reactiveValues(
    samples = matrix(nrow = 0, ncol = 0),
    distInfo = NULL
  )

  # not sure why validations need to appear multiple places
  resetSamples <- reactive({
  	# JMB: Not sure why you need to put these here and have the observeEvents.
  	input$clearSamples
  	input$normalmu
  	input$normalsigma
  	input$distribution
  	input$sampleSize
  	input$samplesToDraw

  	validate(
  		need(!is.null(input$sampleSize), 'Please specify a sample size'),
  		need(is.integer(input$sampleSize), 'Sample size must be an integer'),
  		need(input$sampleSize > 0, 'Sample size must be greater than 0'),
  		need(input$sampleSize <= 500, 'Sample size must be less than 500')

  	)



    userData$samples <- matrix(nrow = 0, ncol = input$sampleSize)
    userData$distInfo <- getDistInfo(normMu = input$normalmu,
                            normSigma = input$normalsigma,
                            expRate = 1 / 10,
                            unifLower = 1,
                            unifUpper = 6)
  })

  # Changing any parameter should reset the samples
  observeEvent(input$clearSamples, { resetSamples() })
  observeEvent(input$normalmu, { resetSamples() })
  observeEvent(input$normalsigma, { resetSamples() })
  observeEvent(input$distribution, { resetSamples() })
  observeEvent(input$sampleSize, { resetSamples() })
  # Uncomment this to have the figure reset when number of samples to draw is changes
  # observeEvent(input$samplesToDraw, { resetSamples() })

  observeEvent(input$sample, {
  	validate(
  		need(!is.null(input$sampleSize), 'Please specify a sample size!'),
  		need(is.integer(input$sampleSize), 'Please enter a whole number for sample size'),
  		need(input$sampleSize > 0, 'Please enter a sample size greater than 0'),
  		need(input$sampleSize <= 500, 'Please enter a sample size  less than 501'),
  		need(!is.null(input$samplesToDraw), 'Please specify number of samples to draw'),
  		need(is.integer(input$samplesToDraw), 'Number of simulated samples must be a whole number'),
  		need(input$samplesToDraw > 0, 'Number of simulated samples to draw must be greater than 0'),
  		need(input$samplesToDraw <= 1000, 'Number of simulated samples to draw must be less than 1,000 for this app'),
  		if(input$distribution =="normal"){
  			need(!is.na(input$normalmu), 'Please specify a mu')},
  		if(input$distribution =="normal"){
  			need(!is.na(input$normalsigma), 'Please specify a sigma')}
  	)

    currentDist <- userData$distInfo[[input$distribution]]
    for(i in 1:input$samplesToDraw) {
      thisSample <- currentDist$realizationFun(input$sampleSize)
      userData$samples <- rbind(userData$samples, thisSample)
    }
  })

  output$distPlot <- renderPlot({
  	validate(
  		need(!is.null(input$sampleSize), 'Please specify a sample size!'),
  		need(is.integer(input$sampleSize), 'Please enter a whole number for sample size'),
  		need(input$sampleSize > 0, 'Please enter a sample size greater than 0'),
  		need(input$sampleSize <= 500, 'Please enter a sample size  less than 501'),
  		need(!is.null(input$samplesToDraw), 'Please specify number of samples to draw'),
  		need(is.integer(input$samplesToDraw), 'Number of simulated samples must be a whole number'),
  		need(input$samplesToDraw > 0, 'Number of simulated samples to draw must be greater than 0'),
  		need(input$samplesToDraw <= 1000, 'Number of simulated samples to draw must be less than 1,000 for this app'),
  		if(input$distribution =="normal"){
  		    need(!is.na(input$normalmu), 'Please specify a mu')},
  		if(input$distribution =="normal"){
  			need(!is.na(input$normalsigma), 'Please specify a sigma')}
  	)

  	currentDist <- userData$distInfo[[input$distribution]]
    statFun <- getStatFun(input$statFun)
    samples <- userData$samples
    distInfo <- userData$distInfo

    if(nrow(samples) > 0) {
      statCex <- 1.2

      screenCells <- getScreenCells()
      #  populationBarColor <- "cadetblue3"
      populationBarColor <- "lightblue"
      #  samplingDistributionBarColor <- "darkgoldenrod2"
      #samplingDistributionBarColor <- "lemonchiffon"
      samplingDistributionBarColor <- "gray95"
      #  firstPreviousSampleBarColor <- "indianred2"
      firstPreviousSampleBarColor <- "lightblue4"
      previousSamplesBarColor <- populationBarColor

      close.screen(all.screens = TRUE)
      split.screen(screenCells)

      xlim <- range(samples) + c(-1, 1) * 0.25 * (max(samples) - min(samples))
      breaks <- longBreaks(samples, n = 20)
      breakD <- breaks[2] - breaks[1]

      setScreenMar(1)
      hist(
        samples,
        xlim = xlim,
        main = "All sampled values",
        xlab = "",
        breaks = breaks,
        prob = input$showPopulation,
        col = populationBarColor
      )

      lastSample <- samples[nrow(samples),]
      for(i in 1:(length(breaks) - 1)) {
        lower <- breaks[i]
        upper <- breaks[i + 1]
        height <- sum(lastSample >= lower & lastSample < upper)
        if(input$showPopulation) {
          height <- height / (length(samples) * breakD)
        }
        polygon(c(lower, lower, upper, upper),
                y = c(0, height, height, 0),
                col = firstPreviousSampleBarColor)
      }


      if(input$showStats) {
        ltext <- c(
          paste(
            "True Pop.",
            input$statFun,
            "=",
            round(currentDist$stats[[input$statFun]](), 2)
          ),
          paste("Observed Pop.", input$statFun, "=", round(statFun(samples), 2))
        )
        mtext(ltext[1], 1, 3, cex = statCex)
        mtext(ltext[2], 1, 4, cex = statCex)
      }

      if(input$showPopulation) {
        xv <- seq(xlim[1], xlim[2], 0.01)
        lines(xv, currentDist$densFun(xv), lwd = 2)
      }

      sampDist <- apply(samples, 1, statFun)

      matchScales <- input$matchScales
      if(input$statFun %in% c("variance", "standard deviation", "range")) {
        matchScales <- FALSE
      }

      sdXlim <- xlim
      sdBreaks <- breaks
      if(!matchScales) {
        sdXlim <- range(sampDist) + c(-1, 1) * 0.25 * (max(sampDist) - min(sampDist))
        sdBreaks <- longBreaks(sampDist, n = 20)
      }

      #####
      # Sampling distribution
      setScreenMar(2)
      hist(
        sampDist,
        xlim = sdXlim,
        main = paste(
          "Simulated Sampling distribution of the\nsample ",
          input$statFun,
          "s",
          sep = ""
        ),
        xlab = "",
        breaks = sdBreaks,
        prob = input$showPopulation,
        col = samplingDistributionBarColor
      )

      lastStat <- sampDist[length(sampDist)]
      lower <- max(sdBreaks[sdBreaks <= lastStat])
      upper <- min(sdBreaks[sdBreaks > lastStat])
      height <- 1
      if(input$showPopulation) {
        height <- height / (length(sampDist) * breakD)
      }
      polygon(c(lower, lower, upper, upper),
              y = c(0, height, height, 0),
              col = firstPreviousSampleBarColor)

      if(input$showStats) {
        ltext <- c(paste("Samp. dist. mean =", round(mean(sampDist), 2)),
                   paste("Samp. dist. SD =", round(sd(sampDist), 2)))
        #legend("topright", legend=ltext, bty='n' )
        mtext(ltext[1], 1, 3, cex = statCex)
        mtext(ltext[2], 1, 4, cex = statCex)
      }

      #Plot previous samples
      previousSampleNames <- c("Last sample", "2nd to last", "3rd to last", "4th to last")
      previousSamples <- samples[nrow(samples):max((nrow(samples) - 3), 1), ]

      if(is.vector(previousSamples)) {
        previousSamples <- matrix(previousSamples, nrow = 1)
      }

      maxheight <- max(apply(previousSamples, 1, function(x) {
        countsInBreaks(x, breaks)
      }))

      for(i in 1:nrow(previousSamples)) {
        screen(i + 2)
        par(mar = c(2, 2, 2, 0))

        value <- statFun(previousSamples[i, ])
        title <- paste(previousSampleNames[i],
                       "\n",
                       input$statFun,
                       " = ",
                       round(value, 2),
                       sep = "")

        col <- ifelse(i == 1,
                      firstPreviousSampleBarColor,
                      previousSamplesBarColor)

        hist(
          previousSamples[i, ],
          xlim = xlim,
          ylim = c(0, maxheight),
          main = title,
          xlab = "",
          breaks = breaks,
          prob = FALSE,
          col = col
        )
      }
    }
  })
})

