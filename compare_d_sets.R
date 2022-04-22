rm(list = ls())
library(data.table)
library(dplyr)
library(purrr)
p <- paste0(getwd(),"/data/track2/practice_year")
d <-  paste0(p, "/", list.files(path = p, pattern = "*.csv")) %>%  map_df(~fread(.))

p <- paste0(getwd(),"/data_Feb2022/track2/practice_year")
d1 <-  paste0(p, "/", list.files(path = p, pattern = "*.csv")) %>%  map_df(~fread(.))

dd <- data.table(diff_er = abs(d$Y - d1$Y), pat = d$n.patients)
dd <- dd[order(diff_er, decreasing = T),]
dd <- dd[1:10000,]
plot(log(dd$diff_er), log(dd$pat))

sapply(names(d), function(i) summary(abs(d[[i]] - d1[[i]])))

p <- paste0(getwd(),"/data/track2/practice")
d <-  paste0(p, "/", list.files(path = p, pattern = "*.csv")) %>%  map_df(~fread(.))

p <- paste0(getwd(),"/data_Feb2022/track2/practice")
d1 <-  paste0(p, "/", list.files(path = p, pattern = "*.csv")) %>%  map_df(~fread(.))

sapply(names(d), function(i) {
  if(is.numeric(d[[i]])) return(summary(abs(d[[i]] - d1[[i]])))
  return(summary(d[[i]] == d1[[i]]))
  }
)
