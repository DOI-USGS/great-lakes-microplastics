#' @import data.table
#' @import dplyr
#' @import reshape2

mungeLandUse <- function(raw.data, target_name){

  # ignore size classes for now, just look at totals
  allSizes <- filter(raw.data, sizeCategory == "total_all")
  
  # remove leftover blanks
  allSizes <- filter(allSizes, towLength_m.y != "DI BLANK")
  
  allSizesSub <- subset(allSizes, select=c("shortName", "UrbanPct", "populationDensity",
                                           "AgTotalPct","ForestPct","Water_WetlandPct","OtherLandUsePct",
                                           "sampleDate","flowCondition","flowConditionAKB",
                                           "conc_per_m3_frag","conc_per_m3_pellet","conc_per_m3_line",
                                           "conc_per_m3_film","conc_per_m3_foam"))

  siteAvg <- mutate(allSizesSub, OtherPct = ForestPct + Water_WetlandPct + OtherLandUsePct) %>%
    group_by(shortName, UrbanPct, OtherPct, AgTotalPct) %>%
    summarise(meanFrag = mean(conc_per_m3_frag, na.rm=TRUE),
              meanPellet = mean(conc_per_m3_pellet, na.rm=TRUE),
              meanFiber = mean(conc_per_m3_line, na.rm=TRUE),
              meanFilm = mean(conc_per_m3_film, na.rm=TRUE),
              meanFoam = mean(conc_per_m3_foam, na.rm=TRUE))
  
  # convert to long
  conc.summary <- melt(siteAvg, id.vars=c("shortName", "UrbanPct", "OtherPct", "AgTotalPct"), variable.name="type", value.name="conc_per_m3")

  write.table(conc.summary,file=target_name, sep="\t")
  return(f.path)
}

  
  
  