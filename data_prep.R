## code to prepare `DATASET` dataset goes here
# View(getwd())
# setwd("riparian.webapp")
file <- 'RiparianModelSelection.csv'
RiparianModels <- read.csv(file,header=TRUE,check.names=FALSE)
save(RiparianModels,file='RiparianModels.rda')

