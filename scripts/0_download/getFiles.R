# function to cache files in data/cache

#' @import sbtools

getFiles <- function(names = NULL, lookup.table){
  
  source("scripts/0_download/auth.R")
  item.id <- "570eaf6be4b0ef3b7ca2565e"
  cache_folder <- 'cache'
  
  # look up table
  #data.relative.abundance matches with All_data_for_data_release.csv
  
  if(!dir.exists(cache_folder)){
    dir.create(cache_folder)
  }
  
  if(is.null(names)){
    files <- item_list_files(item.id)
    fpath <- file.path(cache_folder, files$fname)
    fexists <- file.exists(fpath)
    item_file_download(item.id, names = files$fname[!fexists],
                       destinations = fpath)
  } else {
    fpath <- file.path(cache_folder, names)
    if(!file.exists(fpath)){
      item_file_download(item.id, names = names,
                         destinations = fpath)
    } else {
      return(fpath)
    }
    
  }
  
}
