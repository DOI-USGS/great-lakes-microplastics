#' @import gsplot
#' @import dinosvg
#' @examples 
#' fname.geom.conc <- 'cache/munged_LandUse_geomConc.tsv'
#' fname.geom.pct <- 'cache/munged_LandUse_geomPct.tsv'
#' fname.site <- 'cache/munged_LandUse_site.tsv'
#' gap <- 0.15
#' gs.conc <- gsplotLandUseConc(fname.geom.conc, fname.site, gap)
#' gs.landuse <- gsplotLandUsePct(fname.geom.pct, fname.site, gap)

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
visualizeLandUse <- function(tag, fname.geom.conc, fname.geom.pct, fname.site,
                             fname.fig, gap = 0.15){

  gs.conc <- gsplotLandUseConc(fname.geom.conc, fname.site, gap)
  gs.landuse <- gsplotLandUsePct(fname.geom.pct, fname.site, gap)
  
  createBarFig(gs.conc, gs.landuse, fname.fig)

}

# Returns gsplot object for the top part of the figure
gsplotLandUseConc <- function(fname.data, fname.site, gap){
  
  geom.df <-  read.table(fname.data, sep = "\t", stringsAsFactors = FALSE)
  
  geom.df$site.name[geom.df$site.name == "StLouis, MN"] <- "St Louis, MN"
  geom.df$site.name[geom.df$site.name == "StJoseph, MI"] <- "St Joseph, MI"
  
  site.df <-  read.table(fname.site, sep = "\t", stringsAsFactors = FALSE)
  
  sites <- site.df$Sampling.location.short.name
  site.ids <- data.frame('site.name'=sites, num=1:length(sites), stringsAsFactors = FALSE)
  geom.df <- left_join(geom.df, site.ids) %>% 
    mutate(id = paste0(num,'-',type), hovertext=sprintf('%1.1f (ppcm)',conc_per_m3)) %>% 
    arrange(num) %>%
    #use gap specification for spacing bars
    mutate(x.right = x.left*gap + x.right,
           x.left = x.left*(1+gap), #xright calc before xleft calc bc it needs orig xleft vals
           x.middle = rowMeans(cbind(x.left, x.right))) 
  
  gs.conc <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col, 
         border = NA,
         ylab = "Average concentration,\n in particles per cubic meter",
         ylim=c(0,13.5)) %>% 
    axis(side = 2, at = seq(0, 10, by=5)) %>% 
    axis(1, labels=FALSE, lwd.tick = 0)
  
  # hack because we need to support gs extensions
  gs.conc$view.1.2$rect$id=geom.df$id
  gs.conc$view.1.2$rect$hovertext = geom.df$hovertext
  
  return(gs.conc)
}

# Returns gsplot object for the bottom part of the figure
gsplotLandUsePct <- function(fname.data, fname.site, gap){

  geom.df <-  read.table(fname.data, sep = "\t", stringsAsFactors = FALSE)
  geom.df$site.name[geom.df$site.name == "StLouis, MN"] <- "St Louis, MN"
  geom.df$site.name[geom.df$site.name == "StJoseph, MI"] <- "St Joseph, MI"
  
  site.df <-  read.table(fname.site, sep = "\t", stringsAsFactors = FALSE)
  
  sites <- site.df$Sampling.location.short.name
  site.ids <- data.frame('site.name'=sites, num=1:length(sites), stringsAsFactors = FALSE)

  geom.df <- left_join(geom.df, site.ids) %>% 
    mutate(id = paste0(num,'-',landuse.type), hovertext=sprintf('%1.1f (pct)',landuse.pct)) %>% 
    arrange(num) %>%
    #use gap specification for spacing bars
    mutate(x.right = x.left*gap + x.right,
           x.left = x.left*(1+gap), #xright calc before xleft calc bc it needs orig xleft vals
           x.middle = rowMeans(cbind(x.left, x.right))) 
           
  gs_landuse <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col,
         border = NA,
         ylab = "Basin land use,\nin percent",
         xlab = "Sampling locations") %>% 
    axis(side = 1, at = unique(geom.df$x.middle), 
         labels = unique(geom.df$site.name), 
         tick = FALSE, las = 2, cex.axis = 0.1) %>% 
    axis(side = 2, at = seq(0, 100, by=25))
  
  gs_landuse$view.1.2$rect$id=geom.df$id
  gs_landuse$view.1.2$rect$hovertext = geom.df$hovertext
  gs_landuse$side.1$axis$id=paste0('site-',1:length(sites))
  
  q.sorted <- quickSortIterative(filter(geom.df, landuse.type == 'UrbanPct') %>% .$landuse.pct)
  gs_landuse$json <- q.sorted$swaps_ids
  return(gs_landuse)
}

renameViewSides <- function(svg, side){
  attRename <- function(g, attr='id'){
    attrs <- XML:::xmlAttrs(g)
    attrs[[attr]] <- paste0(attrs[[attr]],'a')
    XML:::removeAttributes(g)
    XML:::addAttributes(g, .attrs = attrs) # renaming the ids as a hack because we are adding new views with the same names
    invisible(NULL)
  }
  
  
  attRename(dinosvg:::g_mask(svg, side=side))
  attRename(dinosvg:::g_view(svg, side=side))
  attRename(dinosvg:::g_side(svg, side=side[1]))
  attRename(dinosvg:::g_side(svg, side=side[2]))
  
  xpath = sprintf("//*[local-name()='g'][@clip-path='url(#mask-%s-%s)']", side[1], side[2])
  masked.nodes <- xpathApply(dinosvg:::g_view(svg, side=c(side[1],paste0(side[2],'a'))), xpath)
  sapply(masked.nodes, function(x) attRename(x, attr='clip-path'))
  invisible(svg)
}

modifyAttr <- function(g, value){
  attrs <- XML:::xmlAttrs(g)
  attrs[[names(value)]] <- as.character(value)
  XML:::removeAttributes(g)
  XML:::addAttributes(g, .attrs = attrs)
  invisible(g)
}
injectLabelTextBreaks <- function(svg.side){
  
  g.lab <- dinosvg:::xpath_one(svg.side, "//*[local-name()='g'][@id='axis-label']")
  lab <- dinosvg:::xpath_one(g.lab, "//*[local-name()='text']")
  text <- strsplit(xmlValue(lab),'\n')[[1]]
  xmlValue(lab) <- text[1]
  attrs <- XML:::xmlAttrs(lab)
  attrs[['dy']] = "-2.5em"
  newXMLNode('text', parent = g.lab, attrs = c(attrs,'class'='sub-label'), newXMLTextNode(text[2]))
  attrs[['dy']] = "-3.0em"
  XML:::removeAttributes(lab)
  XML:::addAttributes(lab, .attrs = attrs)
  
}

JS_defineInitFunction <- function(){
  c('function init(evt){
    if ( window.svgDocument == null ) {
    svgDocument = evt.target.ownerDocument;
    svgDocument.sortLU = this.sortLU;}
}')
}

CSS_defineCSS <- function(){
  'text {
  cursor: default;
  font-family: Tahoma, Geneva, sans-serif;
}
.sub-label, .x-tick-label, .y-tick-label, #tooltip {
font-size: 10px;
}

text{
font-size: 12px;

}'
}
JS_defineSwapLuFunction <- function(types, swap.length, duration=2){
  
  
  frame.interval <- round(duration/swap.length*1000)
  js.function <- c('function sortLU(){
                   \tvar i =0;
                   \twindow.myInterval = setInterval(function () {   
                   if (i < swaps.length){
                   \t var x0 = document.getElementById(swaps[i][0] + "-meanFiber").getAttribute("x");
                   \t var x1 = document.getElementById(swaps[i][1] + "-meanFiber").getAttribute("x");',
    '\t var tr0vals = document.getElementById("site-" + swaps[i][0]).getAttribute("transform").split(/[,()]+/);
    \t var tr1vals = document.getElementById("site-" + swaps[i][1]).getAttribute("transform").split(/[,()]+/);
    \t var tr0new = tr0vals[0]+"("+tr1vals[1]+","+tr0vals[2]+") "+tr0vals[3]+"("+tr0vals[4]+")"
    \t var tr1new = tr1vals[0]+"("+tr0vals[1]+","+tr1vals[2]+") "+tr1vals[3]+"("+tr1vals[4]+")"',
    '\t document.getElementById("site-" + swaps[i][0]).setAttribute("transform", tr0new);',
    '\t document.getElementById("site-" + swaps[i][1]).setAttribute("transform", tr1new);',
    sprintf('\t document.getElementById(swaps[i][0] + "-%s").setAttribute("x", x1);',types),
    sprintf('\t document.getElementById(swaps[i][1] + "-%s").setAttribute("x", x0);',types),
    'i++
                   } else {
    clearInterval(window.myInterval);',
                   sprintf('}}, %s)',frame.interval),
    '}')
  return(paste(js.function, collapse='\n'))
}


createBarFig <- function(gs.conc, gs.landuse, target_name){
  gs.landuse$global$par$mar <- c(9.1, 4.1, 13.5, 2.1)
  gs.landuse$css <- CSS_defineCSS()
  
  svg <- dinosvg::svg(gs.landuse, width = 6, height = 6.3, as.xml=TRUE)
  renameViewSides(svg, gsplot:::as.side(names(gsplot:::sides(gs.landuse))))
  xlab <- dinosvg:::xpath_one(dinosvg:::g_side(svg,"1a"), "//*[local-name()='g'][@id='axis-label']//*[local-name()='text']")
  modifyAttr(xlab, c('dy' = "7.5em"))

  un.conc.types <- unique(unlist(lapply(gs.conc$view.1.2$rect$id,function(x) strsplit(x, '[-]')[[1]][2])))
  un.lu.types <- unique(unlist(lapply(gs.landuse$view.1.2$rect$id,function(x) strsplit(x, '[-]')[[1]][2])))
  all.types = c(un.lu.types, un.conc.types)
  
  LU.swaps <- jsonlite::toJSON(gs.landuse$json)
  swap.length <- nrow(gs.landuse$json)
  dinosvg:::add_ecmascript(svg, sprintf('%s\nvar swaps = %s\n%s', 
                                        JS_defineInitFunction(), 
                                        LU.swaps , 
                                        JS_defineSwapLuFunction(all.types, swap.length, duration=1.5)))
  
  gs.conc$global$par$mar <- c(19.1, 4.1, 2.1, 2.1)
  svg <- dinosvg::svg(svg, gs.conc, as.xml=TRUE)
  
  injectLabelTextBreaks(dinosvg:::g_side(svg,"2a"))
  injectLabelTextBreaks(dinosvg:::g_side(svg,"2"))
  
  tick.labs <- xpathApply(dinosvg:::g_side(svg,"1a"), "//*[local-name()='g'][@id='axis-side-1a']//*[local-name()='g'][@id='tick-labels']//*[local-name()='text']")
  lapply(tick.labs, modifyAttr, c('class'='x-tick-label'))
  tick.labs <- xpathApply(dinosvg:::g_side(svg,"2a"), "//*[local-name()='g'][@id='axis-side-2a']//*[local-name()='g'][@id='tick-labels']//*[local-name()='text']")
  lapply(tick.labs, modifyAttr, c('class'='y-tick-label'))
  tick.labs <- xpathApply(dinosvg:::g_side(svg,"2"), "//*[local-name()='g'][@id='axis-side-2']//*[local-name()='g'][@id='tick-labels']//*[local-name()='text']")
  lapply(tick.labs, modifyAttr, c('class'='y-tick-label'))
  
  dinosvg:::add_tooltip(svg, dx="1.0em")
  dinosvg:::write_svg(svg, target_name)
}
