shinyUI(fluidPage(
    titlePanel("Loess Regression"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("center", "Center:",
                        min = min(df$x), max = max(df$x),
                        value = min(df$x), step = 0.01),
            numericInput("span", "Span:",
                         min = 0, max = 5,
                         value = 0.5, step = 0.1)
        ),

        mainPanel(
            plotOutput("loessPlot")
        )
    )
))
