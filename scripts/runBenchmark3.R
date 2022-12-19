## Load libraries
library(nullranges)
library(MatchIt)
library(microbenchmark)

## Helper functions
source("scripts/makeExampleData.R")

## Run benchmarks --------------------------------------------------------------

## Set the points for sample size at
## 3 different levels
lowPoints <- 10^(seq(3, 5.50, by = 0.25))
mediumPoints <- 10^(seq(5.75, 6, by = 0.25))
highPoints <- 10^(seq(6.25, 7, by = 0.25))

lowResults <-
  lapply(lowPoints, \(n) {
    message(n)
    microbenchmark(
      setup = {
        set.seed(20220725)
        df <- makeExampleData(total = n, ff = 0.05)
      },
      nearest = {
        message("nearest")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "nearest",
                    replace = TRUE)
      },
      stratified_with_replacement = {
        message("stratified_with_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "stratified",
                    replace = TRUE)
      },
      stratified_without_replacement = {
        message("stratified_without_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "stratified",
                    replace = FALSE)
      },
      rejection_with_replacement = {
        message("rejection_with_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = TRUE)
      },
      rejection_without_replacement = {
        message("rejection_without_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = FALSE)
      },
      matchit_with_replacement = {
        message("matchit_with_replacement")
        set.seed(20220725)
        matchit(formula = feature1 ~ feature2,
                data = df,
                method = 'nearest',
                replace = TRUE)
      },
      matchit_without_replacement = {
        message("matchit_without_replacement")
        set.seed(20220725)
        matchit(formula = feature1 ~ feature2,
                data = df,
                method = 'nearest',
                replace = FALSE)
      },
      times = 1L
    )
  })

mediumResults <-
  lapply(mediumPoints, \(n) {
    message(n)
    microbenchmark(
      setup = {
        set.seed(20220725)
        df <- makeExampleData(total = n, ff = 0.05)
      },
      nearest = {
        message("nearest")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "nearest",
                    replace = TRUE)
      },
      stratified_with_replacement = {
        message("stratified_with_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "stratified",
                    replace = TRUE)
      },
      stratified_without_replacement = {
        message("stratified_without_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "stratified",
                    replace = FALSE)
      },
      rejection_with_replacement = {
        message("rejection_with_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = TRUE)
      },
      rejection_without_replacement = {
        message("rejection_without_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = FALSE)
      },
      times = 1L
    )
  })

highResults <-
  lapply(highPoints, \(n) {
    message(n)
    microbenchmark(
      setup = {
        set.seed(20220725)
        df <- makeExampleData(total = n, ff = 0.05)
      },
      nearest = {
        message("nearest")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "nearest",
                    replace = TRUE)
      },
      rejection_with_replacement = {
        message("rejection_with_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = TRUE)
      },
      rejection_without_replacement = {
        message("rejection_without_replacement")
        set.seed(20220725)
        matchRanges(focal = df[df$feature1,],
                    pool = df[!df$feature1,],
                    covar = ~feature2,
                    method = "rejection",
                    replace = FALSE)
      },
      times = 1L
    )
  })

## Set names for each benchmark run
lowResults <- setNames(lowResults, lowPoints)
mediumResults <- setNames(mediumResults, mediumPoints)
highResults <- setNames(highResults, highPoints)

## Save individual results
# saveRDS(object = lowResults, file = "data/lowResults.rds")
# saveRDS(object = mediumResults, file = "data/mediumResults.rds")
# saveRDS(object = highResults, file = "data/highResults.rds")

## Combine
results <- c(lowResults, mediumResults, highResults)

## Save benchmark results
saveRDS(object = results, file = "data/benchmarkResults3.rds")
