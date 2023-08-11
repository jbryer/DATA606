shinyServer(function(input, output) {
	pond <- reactiveValues(
	)

	output$about <- renderRmd('about.Rmd', input)
})
