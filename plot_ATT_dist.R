rm(list = ls())
library(data.table)
library(ggplot2)

# either choose "LogResults" or "Results"

d1 <- fread("LogResults/results_1.csv", header = T)
d2 <- fread("LogResults/results_2.csv", header = T)
d <- rbind(d1, d2)

satt_3 <- d[variable == "Overall" & year == "3",]$satt
satt_4 <- d[variable == "Overall" & year == "4",]$satt

d <- rbind(data.table(ATT = satt_3, year = "Year 3"), 
           data.table(ATT = satt_4, year =  "Year 4"))

# labels to plot

att_mean_3 <- round(d[year == "Year 3", mean(ATT)], 1)
att_mean_4 <- round(d[year == "Year 4", mean(ATT)], 1)

l <- list(mean_3 = att_mean_3, mean_4 = att_mean_4)

dat_text <- data.frame(
  label = c(as.character(as.expression(substitute(mu == mean_3, l))), 
            as.character(as.expression(substitute(mu == mean_4, l)))),
  year   = c("Year 3", "Year 4"),
  x     = c(80, 80), # position in plot (50, 50) for non-log results
  y     = c(0.025, 0.025),
  stringsAsFactors = F
)

(p <- ggplot(d, aes(ATT)) + geom_density() + 
        geom_vline(data = d, aes(xintercept = mean(ATT), group = year), colour = "blue") +
        facet_grid(.~ year) + ylab("Density") +
        geom_vline(data = d[year == "Year 3"], aes(xintercept = -0.2), colour = "red") +
        geom_vline(data = d[year == "Year 4"], aes(xintercept = 0.8), colour = "red") +
        geom_text(data    = dat_text, parse = T,
                  mapping = aes(x = x, y = y, label = label),
                  size = 4) +
        theme_light())

ggsave("logresults_causal_challange.pdf", height = 4, width = 10)

## Learner Weights

fread("LogResults/Qweights_1.csv")
fread("LogResults/Qweights_2.csv")

fread("LogResults/gweights_1.csv")
fread("LogResults/gweights_2.csv")
