## Compare "quality" of matches between MatchIt and
## matchRanges

## Load required packages
library(nullranges)
library(MatchIt)
library(cobalt)
library(data.table)
library(ggplot2)
library(patchwork)
source("scripts/makeExampleData.R")

## Create example dataset
set.seed(123)
df <- as.data.table(makeExampleData(total=1e4, ff=0.05))

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
## Drop weights column
mid[,weights := NULL]

## Rename columns
colnames(mid)[c(1,4)] <- c("id", "ps")

## Replace TRUE/FALSEf w/ 1/0
mid$id <- ifelse(mid$id, 1, 0)

## Add set labels
mid <- mid[id == 0, c(.SD, set="MatchIt")]

## Remove unmatched set & change set labels
mrd <- mrd[set != "unmatched"]
mrd[set == "matched", set := "matchRanges"]

## Combine matchRanges and MatchIt data
data <- rbind(mrd, mid)

## Visualize covariate distribution balance ------------------------------------

## Plot theme
plotTheme <- theme_bw() +
  theme(text=element_text(color="black"),
        axis.text=element_text(color="black"),
        axis.line=element_line(color="black"),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank())

## Place text in NPC coordinates
annotate_npc <- \(label, x, y, just='left', ...) {
  ggplot2::annotation_custom(grid::textGrob(
    x = unit(x, "npc"),
    y = unit(y, "npc"), 
    just=just,
    label = label, ...))
}

## Generate ggplot color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(4)

## Continuous covariate example w/density plots
p1 <- ggplot(data, aes(x=feature2, color=set)) +
  stat_density(geom='line', position='identity', na.rm=TRUE) +
  annotate_npc(label="focal", 0.40, 0.95,
               gp=grid::gpar(col=cols[1])) +
  annotate_npc(label="matchRanges", 0.45, 0.85,
               gp=grid::gpar(col=cols[3])) +
  annotate_npc(label="MatchIt", 0.475, 0.75,
               gp=grid::gpar(col=cols[2])) +
  annotate_npc(label="pool", 0.60, 0.475,
               gp=grid::gpar(col=cols[4])) +
  plotTheme +
  theme(legend.position='none')

## Categorical transformation for stacked barplots
data2 <- data[, .N, by=.(set, feature3)]
data2 <- data2[, .(feature3, N, "pct"=(N/sum(N)*100)), by=set]

p2 <- ggplot(data2, aes(x=set, y=pct, fill=feature3)) +
  geom_col(position='stack') +
  geom_text(aes(label=N),
            position=position_stack(vjust=0.5),
            color="white",
            size=3) + 
  labs(y='percentage') +
  scale_fill_hue(l=70, c=50) +
  plotTheme
  

## Love plots to assess balance with cobalt ------------------------------------

## Default pairwise cobalt plot (MatchIt)
bal_mi <- 
  bal.tab(set ~ feature2 + feature3,
          data=rbind(mid, mrd[set %in% c("focal", "pool")]),
          distance="ps",
          focal="focal",
          which.treat="focal",
          s.d.denom="all")
 
# plot(bal_mi) +
#   xlim(c(-1.5,1.5))

## Default pairwise cobat plot (matchRanges)
bal_mr <- 
  bal.tab(set ~ feature2 + feature3,
          data=mrd,
          distance="ps",
          focal="focal",
          which.treat="focal",
          s.d.denom="all")

# plot(bal_mr) +
#   xlim(c(-1.5,1.5))

## Custom love plot to put comparisons on same plot
## Gather covariate balance data from cobalt
bal <- bal_mr$Pair.Balance$`focal vs. pool`$Balance
bal <- subset(bal, select=-c(Diff.Adj, Type))
bal$strata <- rownames(bal)
bal$Diff.Adj.Mr <- 
  bal_mr$Pair.Balance$`focal vs. matchRanges`$Balance$Diff.Un
bal$Diff.Adj.Mi <- 
  bal_mi$Pair.Balance$`focal vs. MatchIt`$Balance$Diff.Un
dt <- melt(as.data.table(bal), id="strata")
colnames(dt)[2:3] <- c("Sample", "Mean Differences")

## Reorder factor levels
dt$strata <-
  factor(dt$strata, 
         levels=rev(c("ps", "feature2", "feature3_a",
                      "feature3_b", "feature3_c", "feature3_d",
                      "feature3_e")))

levels(dt$Sample) <- c("focal vs. pool",
                       "focal vs. matchRanges",
                       "focal vs. MatchIt")

## Custom cobalt plot
p3 <- ggplot(data=dt, aes(x=`Mean Differences`, y=strata, color=Sample)) +
  geom_point(size=2.5) +
  annotate('segment', x=0, xend=0, y=0, yend=Inf) +
  annotate('segment', x=-Inf, xend=Inf, y=6.5, yend=6.5) +
  geom_point(size=2.5) +
  xlim(c(-1.5, 1.5)) +
  labs(title="Covariate Balance", color="Comparison") +
  theme_bw() +
  theme(text=element_text(color="black"),
        axis.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5))

## Arrange with patchwork
figure <- 
  (p1 / p2 | p3) +
  plot_annotation(tag_levels='A')

## Save plot
ggsave(filename="figures/supplementaryFigureX.pdf",
       plot=figure,
       device="pdf",
       width=10.5, height=6)

ggsave(filename="figures/supplementaryFigureX.png",
       plot=figure,
       device="png",
       width=10.5, height=6)