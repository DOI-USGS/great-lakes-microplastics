#' @import gsplot
#' @import dinosvg

# Functions directly called by remake:make('figures_R.yaml')

visualizeLandUse_mobile <- function(...) {
  visualizeLandUse('mobile', ...)
}
visualizeLandUse_desktop <- function(...) {
  visualizeLandUse('desktop', ...)
}
visualizeLandUse_ie <- function(...) {
  visualizeLandUse('ie', ...)
}

# The workhorse function
visualizeLandUse <- function(tag, fname.geom.conc, fname.geom.pct, fname.fig){

  gs.conc <- gsplotLandUseConc(fname.geom.conc)
  gs.landuse <- gsplotLandUsePct(fname.geom.pct)
  
  createBarFig(gs.conc, gs.landuse, fname.fig)

}

# Returns gsplot object for the top part of the figure
gsplotLandUseConc <- function(fname.data){
  
  geom.df <-  read.table(fname.data, sep = "\t", stringsAsFactors = FALSE)
  sites <- unique(geom.df$site.name)
  site.ids <- data.frame('site.name'=sites, num=1:length(sites), stringsAsFactors = FALSE)
  geom.df <- left_join(geom.df, site.ids) %>% 
    mutate(id = paste0(num,'-',type))
  gs.conc <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col) %>% 
    axis(side = 2, at = seq(0, 10, by=5)) %>% 
    axis(1, labels=FALSE)
  # hack because we need to support gs extensions
  gs.conc$view.1.2$rect$id=geom.df$id
  return(gs.conc)
}

# Returns gsplot object for the bottom part of the figure
gsplotLandUsePct <- function(fname.data){
  
  geom.df <-  read.table(fname.data, sep = "\t")
  
  gs_landuse <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col) %>% 
    axis(side = 1, at = geom.df$x.middle, 
         labels = geom.df$site.label, 
         tick = FALSE, las = 2, cex.axis = 0.1) %>% 
    axis(side = 2, at = seq(0, 100, by=25))
  
  return(gs_landuse)
}


createBarFig <- function(gs.conc, gs.landuse, target_name){
  gs.landuse$global$par$mar <- c(9.1, 4.1, 13.5, 2.1)
  svg <- dinosvg::svg(gs.landuse, width = 6, height = 6.3, as.xml=TRUE)
  view.1 <- dinosvg:::g_view(svg, side=c(1,2))
  attrs <- XML:::xmlAttrs(view.1)
  
  XML:::removeAttributes(view.1)
  XML:::addAttributes(view.1, .attrs = c(id='view-1-2a')) # renaming the view as a hack...
  
  gs.conc$global$par$mar <- c(19.1, 4.1, 2.1, 2.1)
  dinosvg::svg(svg, gs.conc, file=target_name)
}
