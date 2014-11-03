# Author: Matthew Shane Loop

# Purpose: Generate datasets

# Load arguments and set variables
if(TRUE){
    args=(commandArgs(TRUE))
    if(length(args)==0){
        print('no args')
        i=1; full=TRUE
    }else{
        print('eval agrs')
        for(i in 1:length(args)){
            eval(parse(text=args[[i]]))
        }
}}

# Read in data and split into different processes
simulation <- read.table("conditions.txt", sep = "\t", header = TRUE)

# Preliminaries

## Packages
library(spatstat)

### Parameters needed for spatstat
window <- as.owin(c(0, sqrt(3000000), 0, sqrt(3000000)))
status <- as.factor(c("case", "control"))
lambda <- 0.005
kappa <- 0.0001
rmax <- 15
radius <- rmax
mu <- 100

### User-defined functions for spatstat
nclust2 <- function(radius, n, types = status) {
    X <- runifdisc(n, radius, centre = c(0, 0))
    M <- sample(types, n, replace=TRUE)
    marks(X) <- M
    return(X)
}

# Generate datasets
for(j in 1:nrow(simulation)){
    dir.create(c(paste("datasets/c-",simulation$condition[j], sep = "")), recursive = T)
    # Don't need loop for i, because it's an array job
        u <- 12345 + i*j*30000
        set.seed(u, kind = "Mersenne-Twister", normal.kind = "Inversion")
        if(simulation$process[j] == 'homogeneous') {data <- rmpoispp(lambda, win = window, type = status)} else {data <- rNeymanScott(kappa = kappa, rmax = rmax, rcluster = list(mu = mu, f = nclust2), radius = radius, win = window)}
        data <- data.frame(data)
        write.table(data, paste("datasets/c-", simulation$condition[j], "/", i, ".txt", sep = ""), sep = "\t", row.names = F)
}

# Reproducibility
sessionInfo()