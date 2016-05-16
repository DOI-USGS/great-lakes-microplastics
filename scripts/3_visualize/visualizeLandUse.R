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
    axis(1, labels=FALSE) %>% 
    title(ylab = "Average concentration,\n in particles per cubic meter")
  
  # hack because we need to support gs extensions
  gs.conc$view.1.2$rect$id=geom.df$id
  
  return(gs.conc)
}

# Returns gsplot object for the bottom part of the figure
gsplotLandUsePct <- function(fname.data){
  
  geom.df <-  read.table(fname.data, sep = "\t")
  sites <- unique(geom.df$site.name)
  site.ids <- data.frame('site.name'=sites, num=1:length(sites), stringsAsFactors = FALSE)
  geom.df <- left_join(geom.df, site.ids) %>% 
    mutate(id = paste0(num,'-',landuse.type))
  
  gs_landuse <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col) %>% 
    axis(side = 1, at = unique(geom.df$x.middle), 
         labels = unique(geom.df$site.name), 
         tick = FALSE, las = 2, cex.axis = 0.1) %>% 
    axis(side = 2, at = seq(0, 100, by=25)) %>% 
    title(ylab = "Basin land use,\nin percent",
          xlab = "Sampling locations")
  
  gs_landuse$view.1.2$rect$id=geom.df$id
  q.sorted <- quickSortIterative(filter(geom.df, landuse.type == 'UrbanPct') %>% .$landuse.pct)
  gs_landuse$json <- q.sorted$swaps_ids
  return(gs_landuse)
}


createBarFig <- function(gs.conc, gs.landuse, target_name){
  gs.landuse$global$par$mar <- c(9.1, 4.1, 13.5, 2.1)
  svg <- dinosvg::svg(gs.landuse, width = 6, height = 6.3, as.xml=TRUE)
  view.1 <- dinosvg:::g_view(svg, side=c(1,2))
  attrs <- XML:::xmlAttrs(view.1)
  
  XML:::removeAttributes(view.1)
  XML:::addAttributes(view.1, .attrs = c(id='view-1-2a')) # renaming the view as a hack...
  un.conc.types <- unique(unlist(lapply(gs.conc$view.1.2$rect$id,function(x) strsplit(x, '[-]')[[1]][2])))
  un.lu.types <- unique(unlist(lapply(gs.landuse$view.1.2$rect$id,function(x) strsplit(x, '[-]')[[1]][2])))
  all.types = c(un.lu.types, un.conc.types) #swaps.length
  js.function <- c('function swapNums(){
\tvar i =0;
 \twindow.myInterval = setInterval(function () {   
 if (i < swaps.length){
  \t var x0 = document.getElementById(swaps[i][0] + "-meanFiber").getAttribute("x");
  \t var x1 = document.getElementById(swaps[i][1] + "-meanFiber").getAttribute("x");',
  sprintf('\t document.getElementById(swaps[i][0] + "-%s").setAttribute("x", x1);',all.types),
  sprintf('\t document.getElementById(swaps[i][1] + "-%s").setAttribute("x", x0);',all.types),
  'i++
  } else {
     clearInterval(window.myInterval);
}}, 100)',
  '}')
  dinosvg:::add_ecmascript(svg, sprintf('var swaps = %s\n%s', jsonlite::toJSON(gs.landuse$json), paste(js.function, collapse='\n')))
  
  gs.conc$global$par$mar <- c(19.1, 4.1, 2.1, 2.1)
  dinosvg::svg(svg, gs.conc, file=target_name)
}
