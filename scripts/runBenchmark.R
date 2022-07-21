## Load libraries
library(nullranges)
library(microbenchmark)

## Helper functions
source("scripts/makeExampleData.R")

## Run benchmarks --------------------------------------------------------------

## Set the points for sample size
points <- 10^(seq(4, 6, by = 0.25))

## Set seed for reproducibility
set.seed(123)
results <- 
  lapply(points, \(n) {
    
    message(n)
    ## Generate dataset
    df <- makeExampleData(total = n, ff = 0.05)
    
    ## Run microbenchmark
    microbenchmark(
      nearest = matchRanges(focal = df[df$feature1,],
                            pool = df[!df$feature1,],
                            covar = ~feature2 + feature3,
                            method = "nearest",
                            replace = TRUE),
      stratified_with_replacement = matchRanges(focal = df[df$feature1,],
                                                pool = df[!df$feature1,],
                                                covar = ~feature2 + feature3,
                                                method = "stratified",
                                                replace = TRUE),
      stratified_without_replacement = matchRanges(focal = df[df$feature1,],
                                                   pool = df[!df$feature1,],
                                                   covar = ~feature2 + feature3,
                                                   method = "stratified",
                                                   replace = FALSE),
      rejection_with_replacement = matchRanges(focal = df[df$feature1,],
                                               pool = df[!df$feature1,],
                                               covar = ~feature2 + feature3,
                                               method = "rejection",
                                               replace = TRUE),
      rejection_without_replacement = matchRanges(focal = df[df$feature1,],
                                                  pool = df[!df$feature1,],
                                                  covar = ~feature2 + feature3,
                                                  method = "rejection",
                                                  replace = FALSE),
      times = 1
    )
    
  })

## Set names for each benchmark run
results <- setNames(results, points)

## Save benchmark results
saveRDS(object = results, file = "data/benchmarkResults.rds")

