#' @import gsplot
#' @import dinosvg

# Functions directly called by remake:make('figures_R.yaml')
visualizeRelativeAbundance_mobile <- function(...) {
  visualizeRelativeAbundance('mobile', ...)
}
visualizeRelativeAbundance_desktop <- function(...) {
  visualizeRelativeAbundance('desktop', ...)
}
visualizeRelativeAbundance_ie <- function(...) {
  visualizeRelativeAbundance('ie', ...)
}

# The workhorse function
visualizeLandUse <- function(tag, fname.geom.conc, fname.geom.pct, fname.fig){

  gs.conc <- gsplotLandUseConc(fname.geom.conc)
  gs.landuse <- gsplotLandUsePct(fname.geom.pct)
  
  ############## SVG MAGIC HAPPENS HERE ############## 

}

# Returns gsplot object for the top part of the figure
gsplotLandUseConc <- function(fname.data){
  
  geom.df <-  read.table(fname.data, sep = "\t", 
                         stringsAsFactors = FALSE)
  
  gs.conc <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col, 
         legend.name=levels(geom.df$type)) %>% 
    axis(side = 1, at = geom.df$x.middle, 
         labels = geom.df$site.name, 
         tick = FALSE, las = 2, cex.axis = 0.1) %>% 
    axis(side = 2, at = seq(0, 10, by=5))
  
  return(gs.conc)
}

# Returns gsplot object for the bottom part of the figure
gsplotLandUsePct <- function(fname.data){
  
  geom.df <-  read.table(fname.data, sep = "\t", 
                         stringsAsFactors = FALSE)
  
  gs_landuse <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col) %>% 
    axis(side = 1, at = geom.df$x.middle, 
         labels = geom.df$site.name, 
         tick = FALSE, las = 2, cex.axis = 0.1) %>% 
    axis(side = 2, at = seq(0, 100, by=25))
  
  return(gs_landuse)
}

