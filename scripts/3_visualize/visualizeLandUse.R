
#' @import gsplot
#' 

# these will actually save the figures and change the sizes
# visualizeRelativeAbundance_mobile <- function(data.in) {
#   visualizeRelativeAbundance('mobile', data.in)
# }
# visualizeRelativeAbundance_desktop <- function(data.in) {
#   visualizeRelativeAbundance('desktop', data.in)
# }
# visualizeRelativeAbundance_ie <- function(data.in) {
#   visualizeRelativeAbundance('ie', data.in)
# }

visualizeLandUse <- function(fname.data, fname.fig){
  
  data.in <- read.table(fname.data, sep = "\t", 
                        stringsAsFactors = FALSE)
  
  sites <- unique(data.in$shortName)
  num.sites <- length(sites)
  rect.width <- 100/num.sites
  rect.seq <- seq(0, 100, length.out = num.sites+1)
  position.df <- data.frame(site.name = sites,
                            x.left = head(rect.seq, -1),
                            x.right = tail(rect.seq, -1),
                            stringsAsFactors = FALSE) %>% 
    rowwise() %>% 
    mutate(x.middle = mean(c(x.left, x.right))) 
 
## ----------- ## concentration barplot ## ----------- ##
  data.in.conc <- data.in %>% 
    select(-c(UrbanPct, OtherPct, AgTotalPct)) %>% 
    rename(site.name = shortName)
  
  position.df.conc.frag <- data.in.conc %>%
    filter(type == "meanFrag") %>% 
    mutate(y.bottom = 0,
           y.top = conc_per_m3) 
  position.df.conc.pellet <- data.in.conc %>%
    filter(type == "meanPellet") %>% 
    mutate(y.bottom = cbind(position.df.conc.frag$y.top),
           y.top = conc_per_m3)
  position.df.conc.fiber <- data.in.conc %>%
    filter(type == "meanFiber") %>% 
    mutate(y.bottom = cbind(position.df.conc.pellet$y.top),
           y.top = conc_per_m3)
  position.df.conc.film <- data.in.conc %>%
    filter(type == "meanFilm") %>% 
    mutate(y.bottom = cbind(position.df.conc.fiber$y.top),
           y.top = conc_per_m3)
  position.df.conc.foam <- data.in.conc %>%
    filter(type == "meanFoam") %>% 
    mutate(y.bottom = cbind(position.df.conc.film$y.top),
           y.top = conc_per_m3)
    
  position.df.conc <- rbind(position.df.conc.frag, position.df.conc.pellet,
                            position.df.conc.fiber, position.df.conc.film, 
                            position.df.conc.foam)
  position.df.conc <- left_join(position.df, position.df.conc) %>% 
    mutate(rect.col = switch(type,
                             meanFrag = "green",
                             meanPellet = "purple",
                             meanFiber = "orange",
                             meanFilm = "yellow",
                             meanFoam = "blue"))
  
  gs.conc <- gsplot() %>% 
    rect(position.df.conc$x.left, position.df.conc$y.bottom, 
         position.df.conc$x.right, position.df.conc$y.top,
         lwd=0.5, col = position.df.conc$rect.col) %>% 
    axis(side = 1, at = position.df.conc$x.middle, 
         labels = position.df.conc$site.name, 
         tick = FALSE, las = 2, cex.axis = 0.1) %>% 
    axis(side = 2, at = seq(0, 10, by=5))
  
## ----------- ## landuse barplot ## ----------- ##
  
  data.in.landuse <- data.in %>% 
    rename(site.name = shortName) %>% 
    select(-c(type, conc_per_m3)) %>% 
    unique() %>% 
    gather(key = 'landuse.type', value = 'landuse.pct', -site.name)
  
  position.df.landuse.urban <- data.in.landuse %>%
    filter(landuse.type == "UrbanPct") %>% 
    mutate(y.bottom = 0,
           y.top = landuse.pct) 
  position.df.landuse.ag <- data.in.landuse %>%
    filter(landuse.type == "AgTotalPct") %>% 
    mutate(y.bottom = cbind(position.df.landuse.urban$y.top),
           y.top = landuse.pct)
  position.df.landuse.other <- data.in.landuse %>%
    filter(landuse.type == "OtherPct") %>% 
    mutate(y.bottom = cbind(position.df.landuse.ag$y.top),
           y.top = landuse.pct)
  
  
  position.df.landuse <- rbind(position.df.landuse.urban, 
                               position.df.landuse.ag,
                               position.df.landuse.other)
  position.df.landuse <- left_join(position.df, position.df.landuse) %>% 
    mutate(rect.col = switch(landuse.type,
                             UrbanPct = "salmon",
                             AgTotalPct = "yellow",
                             OtherPct = "lightgreen"))
  
  gs_landuse <- gsplot() %>% 
    rect(position.df.landuse$x.left, position.df.landuse$y.bottom, 
         position.df.landuse$x.right, position.df.landuse$y.top,
         lwd=0.5, col = position.df.landuse$rect.col) %>% 
    axis(side = 1, at = position.df.landuse$x.middle, 
         labels = position.df.landuse$site.name, 
         tick = FALSE, las = 2, cex.axis = 0.1) %>% 
    axis(side = 2, at = seq(0, 100, by=25))
  
## ----------- ## Return the two gsplot objects ## ----------- ##
  
  saveRDS(gs_conc, file = "cache/visualize_conc.RDS")
  saveRDS(gs_landuse, file = "cache/visualize_land_use.RDS")
}

