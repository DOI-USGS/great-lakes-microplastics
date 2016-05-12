library(data.table)
library(dplyr)

source("scripts/0_download/cacheFiles.R")
files <- cacheFiles()

raw.data <- fread(files[1])
raw.data <- setDF(raw.data)

SI1 <- read_excel(files[2], skip = 2)
land.per.cols <- which(is.na(names(SI1)))
land.per.cols <- c(land.per.cols[1]-1, land.per.cols)
names(SI1)[land.per.cols] <- SI1[1,land.per.cols]
SI1 <- SI1[-1,]
SI1[,land.per.cols] <- sapply(SI1[,land.per.cols], function(x) as.numeric(x))

rm(files, land.per.cols)

