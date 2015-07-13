library(devtools)

document()
install(build_vignettes=TRUE)
build()

library(IS606)

vignette(package='IS606') # Documentation (including the book)
demo(package='IS606')     # Demos (including shiny apps)
getLabs()                 # The labs
data(package='IS606')     # The data

# Demos
IS606::demo('CLT_mean')
