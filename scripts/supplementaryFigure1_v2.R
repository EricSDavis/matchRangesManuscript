## Load libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)

## Load datasets
results <- readRDS(file = "data/benchmarkResults2.rds")

## With MatchIt ----------------------------------------------------------------

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
  geom_line() +
  geom_point(size = 3) +
  guides(color = guide_legend(override.aes = list(shape=NA))) +
  labs(x = "Sample size",
       y = "Median run time (seconds)",
       color = "Matching method",
       shape = "With replacement") +
  scale_x_log10(breaks = trans_breaks("log10", \(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  expand_limits(x = 10^8) +
  geom_text_repel(data = 
                    summarizedResults |>
                    group_by(method) |>
                    filter(median == max(median)) |>
                    filter(method %in% c("matchit", "stratified")),
                  mapping = aes(x = timePoint, label = method),
                  show.legend = FALSE,
                  seed = 123,
                  hjust = 0) +
  geom_text_repel(data = 
                    summarizedResults |>
                    group_by(method) |>
                    filter(median == max(median)) |>
                    filter(!method %in% c("matchit", "stratified")),
                  mapping = aes(x = timePoint, label = method),
                  show.legend = FALSE,
                  seed = 123,
                  nudge_x = 0.1,
                  nudge_y = 0.1,
                  hjust = 0) +
  theme_bw()+
  theme(aspect.ratio = 0.75)


## Save plot
ggsave(filename = "figures/supplementaryFigure1_v2.pdf",
       device = "pdf")

## Without MatchIt -------------------------------------------------------------

## Remove MatchIt
summarizedResults2 <- summarizedResults |> filter(method != "matchit")

## Visualize with ggplot (without matchit)

ggplot(data = summarizedResults2,
       mapping = aes(x = timePoint,
                     y = median,
                     color = method,
                     shape = replace)) +
  geom_line() +
  geom_point(size = 3) +
  guides(color = guide_legend(override.aes = list(shape=NA))) +
  labs(x = "Sample size",
       y = "Median run time (seconds)",
       color = "Matching method",
       shape = "With replacement") +
  scale_x_log10(breaks = trans_breaks("log10", \(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  expand_limits(x = 10^8) +
  geom_text_repel(data = 
                    summarizedResults2 |>
                    group_by(method) |>
                    filter(median == max(median)) |>
                    filter(method %in% c("matchit", "stratified")),
                  mapping = aes(x = timePoint, label = method),
                  show.legend = FALSE,
                  seed = 123,
                  hjust = 0) +
  geom_text_repel(data = 
                    summarizedResults2 |>
                    group_by(method) |>
                    filter(median == max(median)) |>
                    filter(!method %in% c("matchit", "stratified")),
                  mapping = aes(x = timePoint, label = method),
                  show.legend = FALSE,
                  seed = 123,
                  nudge_x = 0.1,
                  nudge_y = 0.1,
                  hjust = 0) +
  theme_bw()+
  theme(aspect.ratio = 0.75)


## Save plot
ggsave(filename = "figures/supplementaryFigure1_v3.pdf",
       device = "pdf")
