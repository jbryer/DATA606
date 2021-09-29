library(shiny)
library(DATA606)

distributions <- list(
    'Normal' = list(fun = normal_plot),
    'Chi-Squared' = list(fun = chisquare_plot),
    'F distribution' = list(fun = F_plot)
)

ui <- fluidPage(
    titlePanel("Distributions"),

    sidebarLayout(

        sidebarPanel(
            selectInput('distribution',
                        label = 'Distribution',
                        choices = names(distributions)),
            uiOutput('fun_params_ui'),
            uiOutput('tails_ui'),
            uiOutput('limits_lower_ui'),
            uiOutput('limits_upper_ui'),
            uiOutput('bounds_ui')
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    output$fun_params_ui <- renderUI({
        params <- formals(distributions[[input$distribution]]$fun)
        params_default <- formals(distribution_plot)
        fun_params <- names(params)[!names(params) %in% names(params_default)]

        result <- tagList()
        for(i in fun_params) {
            result[[i]] <- numericInput(i, label = i,
                                        value = eval(params[[i]], params),
                                        step = 1)
        }

        return(result)
    })

    output$bounds_ui <- renderUI({
        req(input$tails)

        params_default <- formals(distribution_plot)
        params <- formals(distributions[[input$distribution]]$fun)
        bounds <- eval(params$cv, envir = params)
        limits <- eval(params$limits, envir = params)
        if(input[['tails']] == 'no') {
            sliderInput('bounds',
                        label = 'Bounds',
                        value = bounds,
                        min = limits[1],
                        max = limits[2],
                        step = 0.1
            )
        } else {
            sliderInput('bounds',
                        label = 'Critical value',
                        value = bounds[1],
                        min = limits[1],
                        max = limits[2],
                        step = 0.1
            )
        }
    })

    output$tails_ui <- renderUI({
        params_default <- formals(distribution_plot)
        params <- formals(distributions[[input$distribution]]$fun)
        selectInput('tails',
                    'Tails',
                    choices = eval(params_default$tails, params),
                    selected = eval(params$tails, params))
    })

    output$limits_lower_ui <- renderUI({
        params <- formals(distributions[[input$distribution]]$fun)
        limits <- eval(params$limits, envir = params)
        numericInput('limits_lower',
                     'Lower plot limits',
                     value = limits[1],
                     step = 1)
    })

    output$limits_upper_ui <- renderUI({
        params <- formals(distributions[[input$distribution]]$fun)
        limits <- eval(params$limits, envir = params)
        numericInput('limits_upper',
                     'Upper plot limits',
                     value = limits[2],
                     step = 1)
    })

    output$distPlot <- renderPlot({
        params <- formals(distributions[[input$distribution]]$fun)
        params_default <- formals(distribution_plot)
        fun_params <- names(params)[!names(params) %in% names(params_default)]
        call_params <- list()
        call_params[['tails']] <- input[['tails']]
        call_params[['limits']] <- c(input[['limits_lower']], input[['limits_upper']])
        call_params[['cv']] <- input[['bounds']]
        for(i in fun_params) {
            call_params[[i]] <- input[[paste0(i)]]
        }
        do.call(distributions[[input$distribution]]$fun, call_params)
    })
}

shinyApp(ui = ui, server = server)
