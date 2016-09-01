library(ggplot2)
library(grid)
library(png)

source('renderRmd.R')

init.pond.size <- 250

tufte.white <- '#fffff8'
tufte.black <- '#111111'
theme_update(panel.background = element_rect(fill='white', color='grey70'),
			 panel.grid.major = element_line(color='grey90'),
			 panel.grid.minor = element_line(color='grey96'),
			 strip.background = element_rect(color=tufte.white),
			 strip.text = element_text(face='bold', color=tufte.black),
			 panel.border = element_blank(),
			 axis.line = element_line()
)
