
#' @import dinosvg
#' @import dplyr
#' 
visualizeRelativeAbundance <- function(tag='desktop', data.in){
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
    start.y <- start.y+height
  }
  
  file.out <- sprintf('images/relativeAbundance-%s.svg',tag)
  dinosvg:::write_svg(svg, file=file.out)
  return(file.out)
}


