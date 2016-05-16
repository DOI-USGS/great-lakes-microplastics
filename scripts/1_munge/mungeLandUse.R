#' @import data.table
#' @import dplyr
#' @import reshape2

mungeLandUse <- function(raw.data){

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

  # find position for site bars on the x-axis
  sites <- unique(conc.summary$shortName)
  num.sites <- length(sites)
  rect.seq <- seq(0, 100, length.out = num.sites+1)
  site.geom.df <- data.frame(site.name = sites,
                                 x.left = head(rect.seq, -1),
                                 x.right = tail(rect.seq, -1),
                                 stringsAsFactors = FALSE) %>% 
    rowwise() %>% 
    mutate(x.middle = mean(c(x.left, x.right)))
  
  return(site.geom.df)
}

mungeLandUseConc <- function(data.in, fname.output){
  
  data.in.landuse <- data.in %>% 
    rename(site.name = shortName) %>% 
    select(-c(type, conc_per_m3)) %>% 
    unique() %>% 
    gather(key = 'landuse.type', value = 'landuse.pct', -site.name)
  
  geom.df.landuse.urban <- data.in.landuse %>%
    filter(landuse.type == "UrbanPct") %>% 
    mutate(y.bottom = 0,
           y.top = landuse.pct) 
  geom.df.landuse.ag <- data.in.landuse %>%
    filter(landuse.type == "AgTotalPct") %>% 
    inner_join(geom.df.landuse.urban[c('site.name','y.top')], by='site.name') %>% 
    mutate(y.bottom = y.top, y.top = y.top+landuse.pct)
  geom.df.landuse.other <- data.in.landuse %>%
    filter(landuse.type == "OtherPct") %>% 
    inner_join(geom.df.landuse.ag[c('site.name','y.top')], by='site.name') %>% 
    mutate(y.bottom = y.top, y.top = 100) # verified that these sum to either 100, 99.9 or 100.1
  
  geom.df.landuse <- bind_rows(geom.df.landuse.urban, 
                               geom.df.landuse.ag,
                               geom.df.landuse.other)
  geom.df.landuse <- left_join(geom.df, geom.df.landuse) %>% 
    mutate(rect.col = switch(landuse.type,
                             UrbanPct = "salmon",
                             AgTotalPct = "yellow",
                             OtherPct = "lightgreen"))
  
  write.table(geom.df.landuse, file=fname.output, sep="\t")
  return(fname.output)
}

mungeLandUsePct <- function(data.in, fname.output){
  
  data.in.conc <- data.in %>% 
    select(-c(UrbanPct, OtherPct, AgTotalPct)) %>% 
    rename(site.name = shortName) %>% 
    mutate(type = factor(type, levels = c("meanFrag", "meanPellet", "meanFiber", 
                                          "meanFilm", "meanFoam")), ordered = TRUE)
  
  geom.df.conc.frag <- data.in.conc %>%
    filter(type == "meanFrag") %>% 
    mutate(y.bottom = 0,
           y.top = conc_per_m3) 
  geom.df.conc.pellet <- data.in.conc %>%
    filter(type == "meanPellet") %>% 
    inner_join(geom.df.conc.frag[c('site.name','y.top')], by='site.name') %>% 
    mutate(y.bottom = y.top, y.top = y.top+conc_per_m3)
  geom.df.conc.fiber <- data.in.conc %>%
    filter(type == "meanFiber") %>% 
    inner_join(geom.df.conc.pellet[c('site.name','y.top')], by='site.name') %>% 
    mutate(y.bottom = y.top, y.top = y.top+conc_per_m3)
  geom.df.conc.film <- data.in.conc %>%
    filter(type == "meanFilm") %>% 
    inner_join(geom.df.conc.fiber[c('site.name','y.top')], by='site.name') %>% 
    mutate(y.bottom = y.top, y.top = y.top+conc_per_m3)
  geom.df.conc.foam <- data.in.conc %>%
    filter(type == "meanFoam") %>% 
    inner_join(geom.df.conc.film[c('site.name','y.top')], by='site.name') %>% 
    mutate(y.bottom = y.top, y.top = y.top+conc_per_m3)
  
  geom.df.conc <- bind_rows(geom.df.conc.frag, geom.df.conc.pellet,
                            geom.df.conc.fiber, geom.df.conc.film, 
                            geom.df.conc.foam)
  geom.df.conc <- left_join(geom.df, geom.df.conc) %>% 
    mutate(rect.col = switch(type,
                             meanFrag = "green",
                             meanPellet = "purple",
                             meanFiber = "orange",
                             meanFilm = "yellow",
                             meanFoam = "blue"))
  
  write.table(geom.df.conc, file=fname.output, sep="\t")
  return(fname.output)
}
  