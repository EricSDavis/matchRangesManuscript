## Load libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)

## Load datasets
# results <- readRDS(file = "data/benchmarkResults3.rds")
# results <- readRDS(file = "data/benchmarkResultsTwoCov.rds")
# results <- readRDS("data/benchmarkResultsUpdated.rds")
results <- readRDS("data/benchmarkResultsUpdated2.rds")

## Common plot theme elements
plotTheme <- 
  theme(aspect.ratio = 0.75,
        strip.text=element_text(colour="black", size=14, face="bold"),
        text=element_text(color = "black", size=12, face="bold"),
        axis.text=element_text(color="black", size=12, face="bold"))

## Run time plot ---------------------------------------------------------------

## Format and summarize results
summarizedResults <-
  lapply(results, \(x) tibble(expr = x$expr, time = x$time)) |>
  {\(x) Map(cbind, x, timePoint = as.numeric(names(x)))}() |>
  do.call(rbind, args = _) |>
  separate(col = expr,
           into = c("method", "covariates", "replace"),
           sep = "_",
           extra = "drop",
           fill = "right") |>
  mutate(time = time*1e-09) |> # convert nanosec to sec
  mutate(covariates=factor(x=covariates,
                           labels=c("One covariate", "Two covariates"))) |>
  group_by(method, covariates, replace, timePoint) |>
  summarize(median = median(time))

## Visualize with ggplot
p1 <- ggplot(data = summarizedResults,
       mapping = aes(x = timePoint,
                     y = median,
                     color = method,
                     shape = replace)) +
  facet_wrap(~covariates) +
  geom_line(show.legend=FALSE) +
  geom_point(size = 3, show.legend=FALSE) +
  annotate(geom="point", x=10^3, y=500, shape=17, size=3) +
  annotate(geom="point", x=10^3, y=425, shape=16, size=3) +
  annotate(geom="text", x=10^3.2, y=500, hjust=0, label="With replacement") +
  annotate(geom="text", x=10^3.2, y=425, hjust=0, label="Without replacement") +
  labs(x = "Sample size", y = "Median run time (seconds)") +
  scale_x_log10(breaks = trans_breaks("log10", \(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  expand_limits(x = 10^8) +
  geom_text_repel(data=summarizedResults |>
                    group_by(method, covariates) |>
                    filter(median == max(median)),
                  aes(x=timePoint, label=method),
                  show.legend=FALSE,
                  seed=123,
                  nudge_x=0.2,
                  hjust=0,
                  min.segment.length = Inf) +
  theme_bw()+
  plotTheme

## Save plot
ggsave(plot=p1,
       filename = "figures/supplementaryFigure1RunTime.pdf",
       device = "pdf")


## Fold change plot ------------------------------------------------------------

## Calculate fold-change in runtime over matchit
fcData <- 
  summarizedResults |>
  filter(timePoint <= 10^6) |>
  group_by(covariates, replace, timePoint) |>
  summarise(rejection = median[method == "rejection"] /
              median[method == "matchit"],
            stratified = median[method == "stratified"] /
              median[method == "matchit"]) |>
  pivot_longer(cols=c("rejection", "stratified"),
               names_to="method",
               values_to="fc")

## Need to do nearest separately since
## with replacement is only option.
nearestFcData <-
  summarizedResults |>
  filter(timePoint <= 10^6) |>
  group_by(covariates, replace, timePoint) |>
  summarise(nearest = median[method == "nearest"] /
              median[method == "matchit"]) |>
  pivot_longer(cols="nearest",
               names_to="method",
               values_to="fc")

## Combine data
fcData <- rbind(nearestFcData, fcData)

## Visualize
p2 <- ggplot(data = fcData,
       mapping = aes(x = timePoint,
                     y = log10(fc),
                     color = method,
                     shape = replace)) +
  facet_wrap(~covariates) +
  geom_hline(yintercept=0, lty=2) +
  geom_line(show.legend=FALSE) +
  geom_point(size = 3, show.legend=FALSE) +
  annotate(geom="point", x=10^5.50, y=1.1, shape=17, size=3) +
  annotate(geom="point", x=10^5.50, y=0.9, shape=16, size=3) +
  annotate(geom="text", x=10^5.65, y=1.1, hjust=0, label="With replacement") +
  annotate(geom="text", x=10^5.65, y=0.9, hjust=0, label="Without replacement") +
  labs(x = "Sample size",
       y = "log10 Fold-change in run time over MatchIt") +
  scale_x_log10(breaks = trans_breaks("log10", \(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  expand_limits(x = 10^7) +
  ylim(c(-2, 2)) +
  geom_text_repel(data=fcData |>
                    group_by(method, covariates) |>
                    filter(fc == min(fc)),
                  aes(x=timePoint, y=log10(fc), label=method),
                  show.legend=FALSE,
                  seed=123,
                  nudge_x=0.2,
                  hjust=0,
                  min.segment.length = Inf) +
  theme_bw()+
  plotTheme
  
## Save plot
ggsave(plot=p2,
       filename = "figures/supplementaryFigure1FoldChange.pdf",
       device = "pdf",
       width=10,
       height=6)


## Visualize together ----------------------------------------------------------

library(patchwork)

## Arrange with patchwork
figure <- 
  (p1 / p2) +
  plot_annotation(tag_levels='A')

ggsave(plot=figure,
       filename="figures/supplementalFigure1Updated.png",
       device="png",
       width=12,
       height=10)

ggsave(plot=figure,
       filename="figures/supplementalFigure1Updated.pdf",
       device="pdf",
       width=12,
       height=10)

## Remove fold-change plot -----------------------------------------------------

figure <- p1

## Used for paper
ggsave(plot=figure,
       filename="figures/supplementalFigure1Updated_v2_1200dpi.png",
       device="png",
       width=10,
       height=4.75,
       dpi=1200)

ggsave(plot=figure,
       filename="figures/supplementalFigure1Updated_v2.pdf",
       device="pdf",
       width=10,
       height=4.75)
