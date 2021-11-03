library(shiny)
library(tidyverse)

df <- tibble(
	x = seq(-1, 1, by = 0.01),
	y = x^3 + rnorm(length(x), mean = 0, sd = 0.05) - x
)
