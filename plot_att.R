rm(list = ls())
library(data.table)
library(ggplot2)

# substitute "LogResults" with "Results"

d1 <- fread("LogResults/results_1.csv", header = T)
#d2 <- fread("LogResults/results_2.csv", header = T)
d <- rbind(d1)

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
  x     = c(-20, 0), # position in plot (50, 50) for non-log results
  y     = c(0.02, 0.02),
  stringsAsFactors = F
)

(p <- ggplot(d, aes(ATT)) + geom_density() + 
        geom_vline(data = d, aes(xintercept = mean(ATT), group = year), colour = "blue") +
        facet_grid(.~ year) + ylab("Density") +
        geom_text(data    = dat_text, parse = T,
                  mapping = aes(x = x, y = y, label = label),
                  size = 4) +
        theme_light())

ggsave("logresults_causal_challange.pdf", height = 4, width = 10)

# Old plot
#par(mfrow = c(1,2))
# d <- fread("results1_1700.csv")
# satt_3 <- d[variable == "Overall" & year == "3",]$satt
# hist(satt_3, main = "Year 3", ylim = c(0,1200), xlab = "")
# abline(v = round(mean(satt_3),2), col = "red")
# avg <- format(round(mean(satt_3),2), digits=4)
# legend(x = "topright", legend = bquote(bar(x)*" = " ~ .(avg)))
# 
# satt_4 <- d[variable == "Overall" & year == "4",]$satt
# hist(satt_4, main = "Year 4", ylim = c(0,1200), xlab = "")
# abline(v = round(mean(satt_4),2), col = "red")
# avg <- format(round(mean(satt_4),2), digits=4)
# legend(x = "topright", legend = bquote(bar(x)*" = " ~ .(avg)))


## Learner Weights

fread("LogResults/Qweights_1.csv")
fread("LogResults/Qweights_2.csv")

fread("LogResults/gweights_1.csv")
fread("LogResults/gweights_2.csv")
