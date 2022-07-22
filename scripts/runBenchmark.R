## Load libraries
library(nullranges)
library(microbenchmark)

## Helper functions
source("scripts/makeExampleData.R")

## Run benchmarks --------------------------------------------------------------

## Set the points for sample size
points <- 10^(seq(4, 6, by = 0.25))

## Apply benchmarking for each sample size in points
results <- 
  lapply(points, \(n) {
    
    message(n)
    
    ## Run microbenchmark
    microbenchmark(
      setup = {
        ## Generate dataset
        set.seed(20220722)
        df <- makeExampleData(total = n, ff = 0.05)
      },
      nearest = {
        message("nearest")
        set.seed(20220722)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2 + feature3,
                    method = "nearest",
                    replace = TRUE)
      },
      stratified_with_replacement = {
        message("stratified_with_replacement")
        set.seed(20220722)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "stratified",
                    replace = TRUE)
      },
      stratified_without_replacement = {
        message("stratified_without_replacement")
        set.seed(20220722)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "stratified",
                    replace = FALSE)
      },
      rejection_with_replacement = {
        message("rejection_with_replacement")
        set.seed(20220722)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = TRUE)
      },
      rejection_without_replacement = {
        message("rejection_without_replacement")
        set.seed(20220722)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = FALSE)
      },
      times = 20L
    )
    
  })

## Set names for each benchmark run
results <- setNames(results, points)

## Save benchmark results
saveRDS(object = results, file = "data/benchmarkResults.rds")
