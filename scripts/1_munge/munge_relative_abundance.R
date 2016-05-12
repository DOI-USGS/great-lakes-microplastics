# reading in the data for chapter 1

library(dplyr)

source('scripts/0_download/cacheFiles.R')
cacheFiles('All_data_for_data_release.csv')

data <- read.csv('data/cache/All_data_for_data_release.csv')


