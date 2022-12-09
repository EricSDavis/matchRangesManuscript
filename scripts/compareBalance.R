## Compare "quality" of matches between MatchIt and
## matchRanges

library(nullranges)
library(MatchIt)
library(cobalt)
library(data.table)
source("scripts/makeExampleData.R")

## Create example dataset
set.seed(123)
df <- as.data.table(makeExampleData(total=1e4, ff=0.05))
df2 <- df

## MatchIt
set.seed(123)
mi <- matchit(formula=feature1 ~ feature2 + feature3,
              data=df,
              method='nearest',
              replace=TRUE)

## MatchRanges
set.seed(123)
mr <- matchRanges(focal=df[df$feature1,],
                  pool=df[!df$feature1,],
                  covar=~feature2 + feature3,
                  method="nearest",
                  replace=TRUE)

## Extract & combine matched data for visualization
mrd <- matchedData(mr)
mid <- as.data.table(match.data(mi))

## Format and combine
## Add program name
mrd$program <- "matchRanges"
mid$program <- "MatchIt"

## Drop weights, distance, & ps columns
mid[,weights := NULL]
mid[,distance := NULL]
mrd[,ps := NULL]

## Rename columns
colnames(mid)[1] <- "id"
colnames(df2)[1] <- "id"

## Replace TRUE/FALSEf w/ 1/0
mid$id <- ifelse(mid$id, 1, 0)
df2$id <- ifelse(df2$id, 1, 0)

## Assemble formatted data for MatchIt
formatted <- rbind(
  mid[id == 1, c(.SD, set='focal')],
  mid[id == 0, c(.SD, set="matched")],
  df2[id == 0, c(.SD, program="MatchIt", set="pool")]
)

## Remove unmatched set
mrd <- mrd[set != "unmatched"]

## Combine mrd and formatted
data <- rbind(mrd, formatted)

## Visualize covariate distribution balance
library(ggplot2)
ggplot(data, aes())


mid[!mid$feature1,]

plotCovariate()

plot(density(mid[!mid$feature1,]$feature2))
lines(density(df[df$feature1,]$feature2), col="blue")

## Love plots to assess balance with cobalt
bal.tab(mi) |>
  plot() +
  ggplot2::xlim(c(-1.5,1.5))

bal.tab(set ~ feature2 + feature3,
        data=mrd[set %in% c("focal", "pool", "matched")],
        distance="ps",
        focal="focal",
        which.treat="focal",
        s.d.denom="all") |>
  plot() +
  ggplot2::xlim(c(-1.5,1.5))



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
