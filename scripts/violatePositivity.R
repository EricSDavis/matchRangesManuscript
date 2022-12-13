## Example violating positivity ------------------------------------------------
library(nullranges)

## Create example dataset
set.seed(123)
d <- makeExampleMatchedDataSet(type="data.frame")

## Define focal & pool sets
## Remove "a" from feature3 in pool set
focal <- d[d$feature1,]
pool <- d[!d$feature1 & d$feature3 != "a",]

par(mfrow=c(1,2))
barplot(table(focal$feature3), main="Focal")
barplot(table(pool$feature3), main="Pool")
par(mfrow=c(1,1))

## Matched set by covariate "feature3"
set.seed(123)
m <- matchRanges(focal=focal,
                 pool=pool,
                 covar=~feature3,
                 method='stratified',
                 replace=TRUE)

plotCovariate(x=m, covar="feature3")