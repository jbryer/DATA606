library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Point Estimates"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    	numericInput("populationMean",
    				 label = "Population Mean",
    				 value = 1),
    	numericInput("populationSD",
    				 label = "Population Standard Deviation",
    				 value = 1.25),
    	sliderInput("sampleSize",
    				label = "Sample Size",
    				min = 20, max = 100, value = 30, step = 1),
    	checkboxInput("showPopulation",
    				  label = "Show Population Distribution",
    				  value = FALSE),
    	checkboxInput("showFrequentist",
    				  label = "Show Likelihood",
    				  value = TRUE),
    	checkboxInput("showBootstrap",
    				  label = "Show Bootstrap",
    				  value = FALSE),
    	checkboxInput("showBayesianPrior",
    				  label = "Show Bayesian Prior",
    				  value = FALSE),
    	checkboxInput("showBayesianPosterior",
    				  label = "Show Bayesian Posterior",
    				  value = FALSE),
    	checkboxInput("showLegend",
    				  label = "Show Legend",
    				  value = TRUE),
    	actionButton("resample",
    				 label = "Resample",
    				 icon = icon("refresh"))
    ),

    mainPanel(
       plotOutput("plot", height = "600px")
    )
  )
))
