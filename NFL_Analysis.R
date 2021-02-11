library(tidyverse)

NFL <- read.csv("Data/NFL Play by Play 2009-2016 (v3).csv", fileEncoding = "UTF-8-BOM")

NFL2009 <- filter(NFL, Season == 2009)

#save(NFL2009, file = "Data/NFL2009.RData")

#load("Data/NFL2009.RData")

missing_values <- sapply(NFL, function(x) sum(is.na(x)))

missing_values

count(NFL2009)/count(NFL)
