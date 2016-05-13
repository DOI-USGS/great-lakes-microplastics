#' @import data.table
#' @import dplyr
#' @import reshape2
#' @import readxl

mungeLandUse <- function(fname){
  all.data <- fname[grep("All_data", fname)]
  SI.1.data <- fname[grep("SI_Table 1", fname)]
  
  raw.data <- fread(all.data)
  raw.data <- setDF(raw.data)
  
  SI.1 <- read_excel(SI.1.data, skip = 2)
  land.per.cols <- which(is.na(names(SI.1)))
  land.per.cols <- c(land.per.cols[1]-1, land.per.cols)
  names(SI.1)[land.per.cols] <- SI.1[1,land.per.cols]
  SI.1 <- SI.1[-1,]
  SI.1[,land.per.cols] <- sapply(SI.1[,land.per.cols], function(x) as.numeric(x))

  allSizesSub <- subset(raw.data, select=c("shortName", "UrbanPct", "populationDensity","sampleDate","flowCondition","flowConditionAKB",
                                           "conc_per_m3_frag","conc_per_m3_pellet","conc_per_m3_line",
                                           "conc_per_m3_film","conc_per_m3_foam"))
  # convert to long:
  siteMeanMax <- melt(allSizesSub, id.vars=c("shortName", "UrbanPct", "populationDensity","sampleDate","flowCondition","flowConditionAKB"), 
                          variable.name="type", value.name="concentration") %>%
                     mutate(siteDate = paste(shortName, sampleDate))%>%
                    group_by(shortName, UrbanPct, populationDensity, type) %>%
                    summarise(n=n(),
                              mean = mean(concentration, na.rm=TRUE),
                              max = max(concentration, na.rm=TRUE)) %>%
                    mutate(type = gsub("conc_per_m3_","", type))

  siteMeanMax$type <- ifelse(siteMeanMax$type == "frag", "fragments", siteMeanMax$type)
  siteMeanMax$type <- ifelse(siteMeanMax$type == "pellet", "pellets/beads", siteMeanMax$type)
  siteMeanMax$type <- ifelse(siteMeanMax$type == "line", "fibers/lines", siteMeanMax$type)
  siteMeanMax$type <- ifelse(siteMeanMax$type == "film", "films", siteMeanMax$type)
  siteMeanMax$type <- ifelse(siteMeanMax$type == "foam", "foams", siteMeanMax$type)
  
  siteMeanMax <- siteMeanMax[siteMeanMax$type != "all types" ,]
  
  conc.summary <- siteMeanMax
  f.path <- "cache/mungeLandUse.tsv"
  write.table(conc.summary,file=f.path, sep="\t")
  return(f.path)
}

  
  
  