library(sbtools)
library(data.table)
library(readxl)

source("auth.R")

item.id <- "570eaf6be4b0ef3b7ca2565e"

files <- item_list_files(item.id)
tempFolder <- tempdir()
item_file_download(item.id, dest_dir = tempFolder)


raw.data <- fread(file.path(tempFolder, files$fname[1]))
raw.data <- setDF(raw.data)

SI1 <- read_excel(file.path(tempFolder, files$fname[2]), skip = 2)
land.per.cols <- which(is.na(names(SI1)))
land.per.cols <- c(land.per.cols[1]-1, land.per.cols)
names(SI1)[land.per.cols] <- SI1[1,land.per.cols]
SI1 <- SI1[-1,]
SI1[,land.per.cols] <- sapply(SI1[,land.per.cols], function(x) as.numeric(x))

SI2 <- read_excel(file.path(tempFolder, files$fname[3]), skip = 2)
SI2.2 <- read_excel(file.path(tempFolder, files$fname[3]), skip = 1)
top.names <- names(SI2.2)
top.names[is.na(top.names)] <- top.names[which(is.na(top.names))-1]

names(SI2) <- paste(top.names, names(SI2))
rm(SI2.2)

SI3 <- read_excel(file.path(tempFolder, files$fname[4]), skip = 1)
SI3 <- na.omit(SI3)
