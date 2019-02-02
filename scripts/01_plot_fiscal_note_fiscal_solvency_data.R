##  Rob Wiederstein
##  rob@robwiederstein.org
##  explore correlation between 
##  fiscal notes and fiscal soundness
###############################################################################

#read in cbpp better cost estimates data
file <- "./data/tabula-2015-11-24_cbpp_better_cost_estimates_table1.csv"
df <- read.csv(file = file, header = T, colClasses = "character", strip.white = F)
df <- data.frame(apply(df, 2, stringr::str_trim), stringsAsFactors = F)

#create total column for states that have adopted best practices
df$total <- apply(df, 1, function(x) length(which(x != "")))
df$total <- df$total - 1
df.1 <- dplyr::arrange(df, -total)
colnames(df.1)[1] <- "state"

#read in mercatus state financial rankings
file <-  "./data/mercatus_state_rankings.csv"
df.2 <- read.csv(file = file, header = T, stringsAsFactors = F)
df.2$state <- stringr::str_trim(df.2$state)

#merge the data sets
df.3 <- merge(df.1, df.2)
df.3 <- dplyr::arrange(df.3, -mercatus.fiscal.idx)
df.3$fiscal.rank <- 1:nrow(df.3)

#add state abbreviations for plotting
df.state <- data.frame(state = state.name, state.abb = state.abb)
df.4 <- merge(df.3, df.state)

#linear model
fit.lm.1 <- lm(mercatus.fiscal.idx ~ total, data = df.4)
summary(fit.lm.1) #statistically significant, but 5% explanation value

#generalized linear model
fit.lm.2 <- glm(mercatus.fiscal.idx ~ total, data = df.4)
summary(fit.lm.2)

#plot scatter plot
library(ggplot2)
p <- ggplot(df.4, aes(total, mercatus.fiscal.idx))
p <- p + geom_text(label = df.4$state.abb, size = 2)
p <- p + geom_smooth(method = "loess")
p <- p + scale_x_continuous(name = "fiscal.note.totals")
p <- p + ggtitle("50 States Comparison")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p
filename <- "./plots/50_state_comparison_fiscal_health_versus_fiscal_note_use.jpg"
ggsave(filename = filename, height = 4, width = 6, unit = "in")

#plot boxplot
df.4$total <- as.factor(df.4$total)
p <- ggplot(df.4, aes(total, mercatus.fiscal.idx, group = total, colour = total))
p <- p + geom_boxplot()
p <- p + geom_jitter(width = 0)
#p <- p + geom_point(color = df.4$total)
p <- p + scale_x_discrete(name = "fiscal.note.total")
p <- p + ggtitle("50 States Comparison")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(legend.position="none")
p
filename <- "./plots/50_state_comparison_boxplot.jpg"
ggsave(filename = filename, height = 4, width = 6, unit = "in")

#rearrange and write data out to be inserted in table
df.5 <- dplyr::select(df.4,
                      fiscal.rank,
                      state,
                      state.abb,
                      Prepared.for.all.most.bills:mercatus.fiscal.idx)
df.5$total <- as.integer(levels(df.5$total))[df.5$total]
df.5 <- dplyr::arrange(df.5, fiscal.rank)
file <- "./data/mercatus_cbpp_combined_table.csv"
write.csv(df.5, file = file, row.names = F)
