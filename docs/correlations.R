# Purpose: Create figure 2 for manuscript. It's the correlation matrix of the $\hat{D}(h_i)$ values, for different values of $h_i$

# Packages
library(dplyr)
library(ggplot2)

# Load data
results <- tbl_df(read.table(file = "../analysis/results.txt", sep = "\t", header = TRUE))
results_s <- arrange(
        filter(results, condition > 4),
    condition, iteration, range)
results_s <- data.frame(results_s)

# Function to calculate the correlation matrices
range_corr <- function(x, n.tests, var){
  small <- filter(x, tests == n.tests)
  u1 <- small[small$iteration == 1, "h"]
  u2 <- u1
  range_combos <- expand.grid(h1 = u1, h2 = u2)
  correlation <- vector()
  for(i in 1:nrow(range_combos)){
      x <- select(
          filter(small, h == range_combos[i, 1]),
          reject)
      y <- select(
          filter(small, h == range_combos[i, 2]),
          reject)
      stopifnot(nrow(x) <= 2000, nrow(x) >= 1997, nrow(y) <= 2000, nrow(y) >= 1997)
      correlation[i] <- cor(x, y)
  }
  corr_table <- tbl_df(data.frame(range_combos, correlation))
  return(corr_table)
}

# Calculate correlations
hundred <- range_corr(results_s, 100, reject)

## Plot correlation matrix
p100 <- ggplot(hundred, aes(x = h1, y = h2, fill = correlation)) +
    geom_tile() +
    ggtitle("Correlations among tests when testing 100 ranges") +
    xlab("Range (miles)") +
    ylab("Range (miles)") +
    xlim(min(hundred$h1), max(hundred$h1)) +
    ylim(min(hundred$h2), max(hundred$h2)) +
    scale_fill_continuous(name = "Correlation") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_blank())  # https://felixfan.github.io/page8/
# Combine into 1 plot
png(file = "correlations.png", res = 300, width = 7, height = 7, units = 
'in')
p100
dev.off()
