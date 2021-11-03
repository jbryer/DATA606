shinyServer(function(input, output) {
    getData <- reactive({
        loess.out <- loess(y ~ x, data = df, degree = 1, span = input$span)
        df %>% mutate(fitted = fitted(loess.out))
    })

    getWeightedData <- reactive({
        getData() %>%
            mutate(dist = abs(x - input$center)) %>%
            filter(rank(dist) / n() <= input$span) %>%
            mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)
    })

    output$loessPlot <- renderPlot({
        df <- getData()
        df.points <- getWeightedData()

        ggplot(df.points, aes(x = x, y = y)) +
            geom_vline(xintercept = input$center, linetype = 2) +
            geom_point(data = df, alpha = 0.5, shape = 1) +
            geom_point(aes(color = weight)) +
            geom_smooth(method = 'lm', formula = y ~ x, aes(weight = weight),
                        se = FALSE, color = 'blue', size = 0.5) +
            scale_color_gradient2(low = '#ece7f2', high = '#2b8cbe') +
            geom_line(data = df, aes(y = fitted), color = 'black', size = 0.5) +
            xlim(-1, 1) + ylim(-0.5, 0.5)
    })

})
