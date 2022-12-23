## Latest development version of MatchIt
# remotes::install_github("ngreifer/MatchIt", force=TRUE, dependencies=TRUE)
# install.packages("MatchIt")

## Load libraries
library(nullranges)
library(MatchIt)
library(microbenchmark)

## Helper functions
source("scripts/makeExampleData2.R")

## Define functions to run -----------------------------------------------------

## Wrapper for matchRanges
mr <- function(df, covar, method, replace) {
  message(paste(method, replace, paste(covar, collapse=""), sep=", "))
  set.seed(20220725)
  matchRanges(focal = df[df$f1,],
              pool = df[!df$f1,],
              covar = covar,
              method = method,
              replace = replace)
}

## Wrapper for MatchIt
mi <- function(df, covar, replace) {
  message(paste("MatchIt", replace, paste(covar, collapse=""), sep=", "))
  set.seed(20220725)
  matchit(formula = covar,
          data = df,
          method = "nearest",
          replace = replace)
}

## Run benchmarks --------------------------------------------------------------

## Define the size of each dataset
points <- 10^(seq(3, 7, by=0.25))

res <- lapply(points, \(n) {
  message(n)
  microbenchmark(
    setup = {
      set.seed(20220725)
      df <- makeExampleData(total=n, ff=0.05)
      colnames(df) <- c("f1", "f2", "f3") # shorten colnames
    },
    nearest_one_TRUE = mr(df, ~f2, 'nearest', TRUE),
    nearest_two_TRUE = mr(df, ~f2+f3, 'nearest', TRUE),
    rejection_one_TRUE = mr(df, ~f2, 'rejection', TRUE),
    rejection_two_TRUE = mr(df, ~f2+f3, 'rejection', TRUE),
    rejection_one_FALSE = mr(df, ~f2, 'rejection', FALSE),
    rejection_two_FALSE = mr(df, ~f2+f3, 'rejection', FALSE),
    stratified_one_TRUE = mr(df, ~f2, 'stratified', TRUE),
    stratified_two_TRUE = mr(df, ~f2+f3, 'stratified', TRUE),
    stratified_one_FALSE = mr(df, ~f2, 'stratified', FALSE),
    stratified_two_FALSE = mr(df, ~f2+f3, 'stratified', FALSE),
    matchit_one_TRUE = mi(df, f1~f2, TRUE),
    matchit_two_TRUE = mi(df, f1~f2+f3, TRUE),
    matchit_one_FALSE = mi(df, f1~f2,  FALSE),
    matchit_two_FALSE = mi(df, f1~f2+f3, FALSE),
    times=1L
  )
})

## Save benchmark results
saveRDS(object = results, file = "data/benchmarkResultsUpdated.rds")
