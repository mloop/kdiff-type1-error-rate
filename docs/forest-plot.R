# Purpose: Create figure 1 for manuscript. It's the presentation of the results from the Cox processes

# Packages
library(plyr)
library(dplyr)
library(ggplot2)

# Load data
alpha_hat <- tbl_df(read.table(file = "../analysis/alpha-hat.txt", sep = "\t", header = TRUE))
alpha_hat_cox <- filter(alpha_hat, process == "cox")

# Generate plot
p <- ggplot(alpha_hat_cox, aes(x = tests, y = alpha_hat)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 0.2) +
    ggtitle("Family wise error rates (FWE), with 95% confidence intervals") +
    xlab("Number of ranges tested") +
    ylab("FWE") + 
    theme_classic() +
    theme(plot.title = element_text(size = 8, family = ""), axis.title = element_text(size = 6, family = ""), axis.text = element_text(size = 6, family = ""))
png(file = "forest-plot.png", res = 300, width = 3.3, height = 2, units 
= 'in')
p
dev.off()
