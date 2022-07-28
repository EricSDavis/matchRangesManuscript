## Load libraries
library(tidyverse)
library(ggplot2)

## Load datasets
results <- readRDS(file = "data/benchmarkResults2.rds")

## Format and summarize results
summarizedResults <- 
  lapply(results, \(x) tibble(expr = x$expr, time = x$time)) |>
  {\(x) Map(cbind, x, timePoint = as.numeric(names(x)))}() |>
  do.call(rbind, args = _) |>
  separate(col = expr,
           into = c("method", "replace"),
           sep = "_",
           extra = "drop",
           fill = "right") |>
  replace_na(list(replace = "with")) |>
  mutate(replace = case_when(replace == "with" ~ "TRUE",
                             replace == "without" ~ "FALSE")) |>
  mutate(time = time*1e-09) |> # convert nanosec to sec
  group_by(method, replace, timePoint) |>
  summarize(median = median(time))

## Save table
write_delim(x = summarizedResults,
            file = "tables/benchmarkResults2.txt",
            delim = '\t')

## Visualize with ggplot
ggplot(data = summarizedResults,
       mapping = aes(x = timePoint,
                     y = median,
                     color = method,
                     shape = replace)) +
  scale_x_continuous(trans="log10") +
  geom_line() +
  geom_point(size = 3) +
  guides(color = guide_legend(override.aes = list(shape=NA))) +
  labs(x = "Sample size",
       y = "Median run time (seconds)",
       color = "Matching method",
       shape = "With replacement") +
  theme_bw()+
  theme(aspect.ratio = 0.75)

## Save plot
ggsave(filename = "figures/supplementaryFigure1_v2.pdf",
       device = "pdf")
