rm(list = ls())
library(data.table)
library(ggplot2)

# either choose "LogResults" or "Results"

d1 <- fread("LogResults/results_1.csv", header = T)
d2 <- fread("LogResults/results_2.csv", header = T)
d <- rbind(d1, d2)
setnames(d, old = c("variable","satt"), new = c("Group","ATT"))

p <- ggplot(d, aes(ATT)) + geom_density(aes(color = Group)) +
  ylab("Density") +
  theme_light()

ggsave("group_effects.pdf", height = 4, width = 10)
