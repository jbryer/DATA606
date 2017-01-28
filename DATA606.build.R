# Build and Test Script for IS606 Package
# Package for CUNY IS606 course - Statistics and Probability for Data Analysis

install.packages(c('devtools','roxygen2',
				   'ggplot2','reshape2','psych',
				   'openintro','OIdata'))

##### Building #################################################################
library(devtools)

document()
install(build_vignettes=TRUE)
build()

##### Testing ##################################################################
library(IS606)

vignette(package='DATA606') # Documentation (including the book)
vignette('os3')
demo(package='DATA606')     # List the available demos (including shiny apps)
getLabs()                 # List the available labs
data(package='DATA606')     # List the available data

# Demos
IS606::demo('CLT_mean')

shiny_demo(package='IS606')

shiny_demo('BayesBilliards')
shiny_demo('lottery')
shiny_demo('gambler')
shiny_demo('DualScales')

# View labs
viewLab('Lab0')
viewLab('Lab1')
viewLab('Lab2')
viewLab('Lab3')
viewLab('Lab4a')
viewLab('Lab4b')
viewLab('Lab5')
viewLab('Lab6')
viewLab('Lab7')
viewLab('Lab8')

# Start labs
startLab('Lab0')
