# Purpose: To estimate type 1 error rate of K test

# Preliminaries
## Set up code to run as array job on Cheaha
if(TRUE){
    args = (commandArgs(TRUE))
if(length(args) == 0){
   print('no args')
    i = 1; full = TRUE
 } else{
     print('eval agrs')
     for(i in 1:length(args)){
         eval(parse(text = args[[i]]))
    }
}}

## Packages
library(spatstat)
library(plyr)

## Simulation parameters
nsim <- 199
nrank <- 5
window <- as.owin(c(0, sqrt(3000000), 0, sqrt(3000000)))


## User-define functions
# Function, from 2013 edition of Applied Spatial Data Analysis with R
kdiff <- function(Xppp, r, cr = cr){
    k1 <- Kest(Xppp[marks(Xppp) == "case"], r = r, correction = cr)
    k2 <- Kest(Xppp[marks(Xppp) == "control"], r = r, correction = cr)
    res <- data.frame(r = r, D = k1[[cr]] - k2[[cr]])
    return(fv(res, valu = "D", fname = "D"))
}

## Import conditions
conditions <- read.table("../data/conditions.txt", sep = "\t", header = TRUE)

# Perform simulation
for(j in 1:nrow(conditions)){

        u <- 54321 + i*j*30000
        set.seed(u, kind = "Mersenne-Twister", normal.kind = "Inversion")
        
        # Import data and format for spatstat
        data <- read.table(file = paste("../data/datasets/c-", conditions$condition[j], "/", i, ".txt", sep = ""), sep = "\t", header = T)
        data <- ppp(x = data$x, y = data$y, window = window, marks = as.factor(data$marks))
        
        # Calculate the distances at which to evaluate the test
        max <- 0.25 * sqrt(conditions$A[j])
        int <- max/conditions$tests[j]
        r <- seq(0, max, by = int)
        
        # Perform the random labeling nsim times determine the high and low ranks based upon crit
        envkdiff <- envelope(data, kdiff, r = r, cr = "border", nsim = nsim, nrank = nrank, savefuns = TRUE, simulate = expression(rlabel(data)))
        reject <- rep(0, times = (length(r) - 1))
        condition <- rep(conditions$condition[j], times = (length(r) - 1))
        tests <- rep(conditions$tests[j], times = (length(r) - 1))
        iteration <- rep(i, times = (length(r) - 1))
        range <- seq(1, (length(r) - 1), 1)
        obs <- envkdiff[["obs"]][-1]
        lo <- envkdiff[["lo"]][-1]
        hi <- envkdiff[["hi"]][-1]
        h <- r[-1]
        data <- data.frame(condition, iteration, tests, range, h, obs, lo, hi, reject)
        data$reject[which(data$obs < data$lo | data$obs > data$hi)] <- 1
        write.table(data, paste("results-", conditions$condition[j], "-", i, ".txt", sep = ""), sep = "\t", row.names = FALSE)

}

# Reproducibility
sessionInfo()