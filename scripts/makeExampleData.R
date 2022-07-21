#' Function to make example covariate data
#' @param total Numeric - total number of values in the set
#' @param ff Numeric - fraction of total reserved for the
#'  focal set.
#' @returns A data.frame
#' @noRd
makeExampleData <- function(total = 1e05, ff = 0.05) {
  sizeFocal <- total*ff
  sizePool <- total - sizeFocal
  data.frame(
    feature1 = c(rep(TRUE, sizeFocal),
                 rep(FALSE, sizePool)),
    feature2 = c(abs(rnorm(sizeFocal, mean = 4, sd = 2)),
                 runif(sizePool, min = 0, max = 12)),
    feature3 = c(sample(letters[seq_len(5)],
                        size = sizeFocal,
                        replace = TRUE,
                        prob = c(0.1, 0.3, 0.4, 0.1, 0.05)),
                 sample(letters[seq_len(5)],
                        size = sizePool,
                        replace = TRUE,
                        prob = c(0.4, 0.3, 0.1, 0.1, 0.05)))
  )
  
}