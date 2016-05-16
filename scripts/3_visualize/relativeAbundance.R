#' @import dinosvg
#' @import dplyr
#' @import yaml

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

visualizeRelativeAbundance <- function(tag='desktop', file.in, file.text, target_name){

# The workhorse function

    data.in <- read.table(file.in, header = TRUE, sep = '\t', stringsAsFactors = FALSE)
  
  text.in <- yaml.load_file(file.text)
  
  svg <- dinosvg:::init_svg(width = 12, height = 6)
  
  groups <- list(list(col='#7fc97f', cx='50', cy='50', name='Foam'),
                 list(col='#beaed4', cx='150', cy='50', name='Film'),
                 list(col='#fdc086', cx='250', cy='50', name='Pellet/Bead'),
                 list(col='#ffff99', cx='350', cy='50', name='Fragment'),
                 '5'=list(col='#386cb0', cx='450', cy='50', name='Fiber/Line'))
  
  start.y <- 150
  bar.height <- 250
  for (group in groups){
    name <- group$name
    perc <- filter(data.in, Figure.Name==name) %>% .$Percent.Type
    height <- perc*bar.height/100
    fill <- group$col
    cx <- group$cx
    cy <- group$cy
    
    g <- dinosvg:::svg_node("g", svg, c(id=group$name))
    dinosvg:::svg_node("rect", g, c(x="200", y=start.y, width="100", height=height, fill=fill))
    dinosvg:::svg_node("circle", g, c(cx=cx, cy=cy,r='40', fill=fill))
    dinosvg:::svg_node("text", g, c(x=cx, y=cy,'text-anchor'='middle',dy='0.33em'),
                       newXMLTextNode("Bead"))
    dinosvg:::svg_node("text", g, c(x=cx, y=cy,'text-anchor'='middle',dy='0.33em', opacity='0.2',id="something"),
                       newXMLTextNode("Detail 1"))
    
    start.y <- start.y+height
  }
  
  dinosvg:::write_svg(svg, file=target_name)
  return(target_name)
}


