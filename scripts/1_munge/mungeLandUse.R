#' @import data.table
#' @import dplyr
#' @import reshape2
#' @import readxl

mungeLandUse <- function(fname){

  raw.data <- fread(fname[1])
  raw.data <- setDF(raw.data)
  
  SI.1 <- read_excel(fname[2], skip = 2)
  land.per.cols <- which(is.na(names(SI.1)))
  land.per.cols <- c(land.per.cols[1]-1, land.per.cols)
  names(SI.1)[land.per.cols] <- SI.1[1,land.per.cols]
  SI.1 <- SI.1[-1,]
  SI.1[,land.per.cols] <- sapply(SI.1[,land.per.cols], function(x) as.numeric(x))

  # ignore size classes for now, just look at totals
  allSizes <- filter(raw.data, sizeCategory == "total_all")
  
  # remove leftover blanks
  allSizes <- filter(allSizes, towLength_m.y != "DI BLANK")
  
  allSizesSub <- subset(allSizes, select=c("shortName", "UrbanPct", "populationDensity","sampleDate","flowCondition","flowConditionAKB",
                                           "conc_per_m3_frag","conc_per_m3_pellet","conc_per_m3_line",
                                           "conc_per_m3_film","conc_per_m3_foam"))

  siteAvg <- group_by(allSizesSub, shortName, UrbanPct) %>%
    summarise(meanFrag = mean(conc_per_m3_frag, na.rm=TRUE),
              meanPellet = mean(conc_per_m3_pellet, na.rm=TRUE),
              meanFiber = mean(conc_per_m3_line, na.rm=TRUE),
              meanFilm = mean(conc_per_m3_film, na.rm=TRUE),
              meanFoam = mean(conc_per_m3_foam, na.rm=TRUE))
  
  # convert to long
  conc.summary <- melt(siteAvg, id.vars=c("shortName", "UrbanPct"), variable.name="type", value.name="conc_per_m3")
  
  f.path <- "cache/mungeLandUse.tsv"
  write.table(conc.summary,file=f.path, sep="\t")
  return(f.path)
}

  
  
  