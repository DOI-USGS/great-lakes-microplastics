# function to cache files in data/cache

cacheFiles <- function(names = NULL){
  library(sbtools)
  library(data.table)
  library(readxl)
  
  source("scripts/0_download/auth.R")
  
  item.id <- "5734d92ce4b0dae0d5de62ee"
  
  files <- item_list_files(item.id)
  cache_folder <- 'data/cache'
  
  if(is.null(names)){
    item_file_download(item.id, dest_dir = cache_folder)
  } else {
    item_file_download(item.id, names = names,
                       destinations = file.path(cache_folder, names))
  }
  
}
