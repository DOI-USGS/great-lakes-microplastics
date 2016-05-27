#' @import dinosvg
#' @import dplyr
#' @import yaml
#' @import XML
#' @examples 
#' visualizeRelativeAbundance("desktop","cache/mungedRelativeAbundance.tsv","data/siteText.yaml","test.svg")


# Functions directly called by remake::make('figures_R.yaml')
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
visualizeRelativeAbundance <- function(tag='desktop', file.in, file.text, target_name){

  data.in <- read.table(file.in, header = TRUE, sep = '\t', stringsAsFactors = FALSE)
  
  text.in <- yaml.load_file(file.text)
  
  svg <- dinosvg:::init_svg(width = 12, height = 6, onload="init(evt)")
  dinosvg:::add_css(svg, CSS_relAbun())
  
  groups <- list(list(col='#4ebec2', cx='85', cy='75', name='Pellet/Bead', id="beads"),
                 list(col='#0b516b', cx='400', cy='146.5', name='Film', id="films"),
                 list(col='#01b29F', cx='85', cy='218', name='Foam', id="foams"),
                 list(col='#aadedc', cx='400', cy='300', name='Fragment', id="fragments"),
                 list(col='#26b9da', cx='85', cy='360', name='Fiber/Line', id="fiberlines"))

  start.y <- 25
  bar.height <- 400
  for (group in groups){
    name <- group$name
    perc <- filter(data.in, Figure.Name==name) %>% .$Percent.Type
    height <- perc*bar.height/100
    fill <- group$col
    cx <- as.numeric(group$cx)
    cy <- as.numeric(group$cy)
    cr <- 70
    offset.x <- -50
    offset.y <- -30

    on.right <- cx > 300 # on right side of bars
    
    g <- dinosvg:::svg_node("g", svg, c(id=group$name))
    
    if (on.right){
      # top of circle to top of rect, to bottom of rect, to bottom of circle
      d = sprintf("M%s %s a%s,%s 0 0,0 -%s,%s L 300 %s L 300 %sz", cx, cy-cr, cr,cr,cr,cr, start.y+height, start.y)
    } else {
      d = sprintf("M%s %s a%s,%s 0 0,1 %s,%s L 200 %s L 200 %sz", cx, cy-cr, cr,cr,cr,cr, start.y+height, start.y)
    }

    dinosvg:::svg_node("path", g, c(d=d, fill=fill, stroke="none",class='hidden', opacity="0.8",
                                    id = paste0(group$name,"-path"),
                                    onmouseover="MakeOpaque(evt)",
                                    onmouseout="MakeTransparent(evt)"))
    dinosvg:::svg_node("rect", g, c(x="200", y=start.y, width="100", height=height, 
                                    fill=fill,opacity="0.8",
                                    id = paste0(group$name,"-rect"),
                                    onmouseover="MakePathOpaque(evt)",
                                    onmouseout="MakePathTransparent(evt)",
                                    'clip-path'="url(#mask-water)"))
    dinosvg:::svg_node("circle", g, c(cx=cx, cy=cy,r=cr, fill=fill,stroke=fill, opacity="0.8", 'stroke-opacity'="0.4", 
                                      id=paste0(group$name,"-viz-circle")))
    
    dinosvg:::svg_node("text", g, c(x=cx+offset.x, y=cy+offset.y,fill="#FFFFFF",
                                    'text-anchor'='left',dy='0.33em'),
                       newXMLTextNode(text.in[paste0("relAbundance-",group$id,"-label")]))
    dinosvg:::svg_node("text", g, c(x=cx, y=cy,fill="#FFFFFF",
                                    'text-anchor'='middle',dy='0.23em',
                                    id=paste0(group$id,".details"),
                                    'font-size'='2em'),
                       newXMLTextNode(paste(sprintf(fmt = "%1.1f",perc),"%")))
    # this is a dummy that is invisible, but controls the mouseover
    dinosvg:::svg_node("circle", g, c(cx=cx, cy=cy,r=cr, opacity="0.0",
                                      id = paste0(group$name,"-circle"),
                                      onmouseover="MakePathOpaque(evt)",
                                      onmouseout="MakePathTransparent(evt)"))
    start.y <- start.y+height
  }
  defs = dinosvg:::svg_node("defs", svg)
  clip = dinosvg:::svg_node("clipPath", svg, c(id="mask-water"))
  dinosvg:::svg_node("rect", clip, c(y="425", height="0", width="400", id="water-dim"))
  
  dinosvg:::add_ecmascript(g, paste(JS_MakeTransparent(),JS_MakeOpaque(),JS_moveMask(), 
                                    JS_MakePathTransparent(),JS_MakePathOpaque(), sep=
                             '\n'))
  dinosvg:::write_svg(svg, file=target_name)
  return(target_name)
}

JS_MakeOpaque <- function(){
  c('function MakeOpaque(evt) {
    evt.target.setAttributeNS(null,"class","shown");
}')
}

JS_MakeTransparent <- function(){
  c('function MakeTransparent(evt) {
    evt.target.setAttributeNS(null,"class","hidden");
}')
}

JS_MakePathTransparent <- function(){
  c('function MakePathTransparent(evt) {
      var id=evt.target.getAttribute("id");
	    var node = document.getElementById(id.split("-")[0] + "-path");
      node.setAttributeNS(null, "class","hidden");
}')
}
JS_MakePathOpaque <- function(){
  c('function MakePathOpaque(evt) {
    var id=evt.target.getAttribute("id");
    var node = document.getElementById(id.split("-")[0] + "-path");
    node.setAttributeNS(null, "class","shown");
    
}')
}
JS_moveMask <- function(){
  'function init(evt){
			moveMask()
  }

function moveMask(){
  var yo = 425;
  var y = 25;
  var i = 0;
  var numSteps = 100;
  var mask = document.getElementById("water-dim");
  var sweepCount = setInterval(function () {   
  if (i < numSteps){
  var ynew = (y*(i+1)+yo*(numSteps-1-i))/numSteps;
  var height = 400*(i+1)/numSteps;
  mask.setAttribute("y", ynew);
  mask.setAttribute("height", height);
  i++
  } else {
  clearInterval(sweepCount);
  }}, 30);};'
}

CSS_relAbun <- function(){
  '.shown, .hidden {
    -webkit-transition: opacity 0.2s ease-in-out;
    -moz-transition: opacity 0.2s ease-in-out;
    -o-transition: opacity 0.2s ease-in-out;
    transition: opacity 0.2s ease-in-out;
  }
  
  .hidden {
    opacity:0;
  }
'
}
