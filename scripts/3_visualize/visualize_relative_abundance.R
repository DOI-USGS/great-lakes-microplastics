
#' @import dinosvg
#' 
visualizeRelativeAbundance <- function(){
  svg <- dinosvg:::init_svg(width = 12, height = 6)
  
  groups <- list(list(col='#7fc97f', perc=21.2, cx='50', cy='50', name='FOAM'),
                 list(col='#beaed4', perc=11.9, cx='150', cy='50', name='FILM'),
                 list(col='#fdc086', perc=30.7, cx='250', cy='50', name='PELLET/BEAD'),
                 list(col='#ffff99', perc=4.3, cx='350', cy='50', name='FRAGMENT'),
                 '5'=list(col='#386cb0', perc=31.9, cx='450', cy='50', name='FIBER/LINE'))
  
  start.y <- 150
  bar.height <- 250
  for (group in groups){
    height <- group$perc*bar.height/100
    fill <- group$col
    cx <- group$cx
    cy <- group$cy
    g <- dinosvg:::svg_node("g", svg, c(id=group$name))
    dinosvg:::svg_node("rect", g, c(x="200", y=start.y, width="100", height=height, fill=fill))
    dinosvg:::svg_node("circle", g, c(cx=cx, cy=cy,r='40', fill=fill))
    start.y <- start.y+height
  }
  
  file.out <- 'images/relative_abundance.svg'
  dinosvg:::write_svg(svg, file=file.out)
  return(file.out)
}


