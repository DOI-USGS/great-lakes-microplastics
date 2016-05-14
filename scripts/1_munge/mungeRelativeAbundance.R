# reading in the data for chapter 1

#' @import data.table
#' @import dplyr
#' @import tidyr
#' @import jsonlite

mungeRelativeAbundance <- function(raw.data, target_name){

  
  clean.data <- raw.data %>% 
    select(starts_with('count')) %>% 
    gather() %>% 
    rename(Microplastic.Type = key, Count = value) %>% 
    group_by(Microplastic.Type) %>% 
    summarize(Count = sum(Count, na.rm = TRUE)) 
  
  total.count <- clean.data$Count[which(clean.data$Microplastic.Type == "countTotal")]
  
  percent.data <- clean.data %>% 
    mutate(Percent.Type = (Count/total.count)*100) %>% 
    filter(!Microplastic.Type %in% c("countOther", "countTotal")) %>% 
    rowwise() %>% 
    mutate(Figure.Name = switch(Microplastic.Type,
                                countFrag = "Fragment",
                                countPellet = "Pellet/Bead",
                                countLine = "Fiber/Line",
                                countFilm = "Film",
                                countFoam = "Foam"))
  
  write.table(percent.data, target_name, sep="\t")
  return(target_name)
}
