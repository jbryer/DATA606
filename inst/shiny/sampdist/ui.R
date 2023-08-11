library(shiny)
# BCD COMMENT: THis app was originally created by Kyle Hardman.  See the "about" tab for a link.
shinyUI(fluidPage(
  theme="superhero.bcd2.css",
  titlePanel("Simulated Sampling Distributions"),

  sidebarLayout(
    sidebarPanel(
      div(class="row",
          div(class="col-xs-6",
              selectInput("distribution",
                          label = "Population Distribution",
                          choices = list("Normal"= "normal",
                                         "Continuous Uniform" = "uniform",
                                         "Negative Exponential" = "exponential"),
                          selected = "normal")),
          div(class="col-xs-6",
              selectInput("statFun",
                          label = "Statistic",
                          choices = list("mean", "median",
                                         "variance", "standard deviation", "range"),
                          selected = "mean"))
      ),

      conditionalPanel(condition="input.distribution =='normal'",
                       div(class="row",
                           div(class="col-xs-6",
                               numericInput("normalmu",
                                            label = "Mu for normal", value=100)),
                           div(class="col-xs-6",
                               numericInput("normalsigma",
                                            label = "Sigma for normal", value=15)) )
      ),

      div(class="row",
          div(class="col-xs-6",
              numericInput("sampleSize", label="Sample size",
                           min=1, max=500, value=10)),
          div(class="col-xs-6",
              numericInput("samplesToDraw", label="Samples to draw at a time",
                           min=1, max=1000, value=1))
      ),

      actionButton("sample", label="Draw Sample(s)", class = "btn-primary"),
      actionButton("clearSamples", label="Clear All Samples", class = "btn-info"),
      checkboxInput("matchScales", label="Match scales", value=TRUE),
      checkboxInput("showStats", label="Show stats", value=FALSE),
      checkboxInput("showPopulation", label="Show parent distribution (population)", value=FALSE),

      conditionalPanel(condition="input.distribution == 'normal'",
                       wellPanel(
                         HTML("For the Normal Distribution Simulation, Mu is initially set at 100
                              and Sigma is initially set at 15, but the user can change these values.")
                       )),

      conditionalPanel(condition="input.distribution == 'uniform'",
                       wellPanel(
                         HTML("For the Uniform Distribution Simulation,
                              values range from 1 to 6, with a Mu of 3.5  and Sigma is
                              1.44")
                       )),

      conditionalPanel(condition="input.distribution == 'exponential'",
                       wellPanel(
                         HTML("For the Exponential Distribution Simulation,
                              the rate parameter is set to 1/10")
                       ))
    ),

    mainPanel(
      # BCD comment: This tag$head inserted by BCD to handle validation output and change tab colors
      tags$head(
        tags$style(HTML("
		                    .shiny-output-error-validation {
		                    font-size: 20px; color: skyblue;
		                    }
		                    ")),
        tags$style(HTML("
                        .tabbable > .nav > li > a                  {background-color: lightgray;  color:navy}
        				.tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
        				.tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
        				.tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
        				.tabbable > .nav > li[class=active]    > a {background-color: lemonchiffon; color:navy}
        				"))
      ),
      tabsetPanel(
        tabPanel("Plots/Summary",
                 plotOutput("distPlot", height=600)
        ),
        tabPanel("About",
                 includeMarkdown('about.md')
        )
      )
    )
  )
))

