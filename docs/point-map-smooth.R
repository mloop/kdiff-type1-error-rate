# Purpose: Plot example dataset

# Preliminaries

## Packages
library(ggplot2)
library(spatstat)

## User-functions
source(file = '~/Documents/r-scripts/multiplot.R')

# Import example dataset
data <- read.table(file = "../data/datasets/c-5/1.txt", sep = "\t", header = T)

# Plot dataset
p1 <- ggplot(data, aes(x, y)) +
    geom_point(aes(color = marks), size = 0.3) +
    ggtitle("Example event locations") +
    xlab("Miles") + 
    ylab("Miles") + 
    guides(colour = guide_legend(override.aes = list(size = 2))) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank()) +  # https://felixfan.github.io/page8/
    scale_color_brewer(palette = 'Set1')
pp <- ppp(x = data$x, y = data$y, window = as.owin(c(0, sqrt(3000000), 0, sqrt(3000000))), marks = data$marks)
rr <- relrisk(pp)
rr_df <- as.data.frame(rr)

p2 <- ggplot(rr_df, aes(x = x, y = y)) +
    geom_raster(aes(fill = 1 - value)) +
    ggtitle('Example smoothed risk') +
    scale_fill_continuous(low = 'blue', high = 'red', name = 'Risk') +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank()) +
    xlab("Miles") + 
    ylab("Miles")

png(file = "point-map-smooth.png", res = 300, width = 6.6, height = 3, 
units = 'in') 
multiplot(p1, p2, cols = 2)
dev.off()
