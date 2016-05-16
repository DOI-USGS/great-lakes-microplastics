#' @import dinosvg
#' @import dplyr
#' @import yaml
#' @import XML

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
  
  svg <- dinosvg:::init_svg(width = 12, height = 6)
  
  groups <- list(list(col='#fdc086', cx='85', cy='75', name='Pellet/Bead', id="beads"),
                 list(col='#beaed4', cx='400', cy='146.5', name='Film', id="films"),
                 list(col='#7fc97f', cx='85', cy='218', name='Foam', id="foams"),
                 list(col='#ffff99', cx='400', cy='300', name='Fragment', id="fragments"),
                 list(col='#386cb0', cx='85', cy='360', name='Fiber/Line', id="fiberlines"))
  # groups <- list(list(col='#aadedc', cx='50', cy='50', name='Foam', id="foams"),
  #                list(col='#26b9da', cx='150', cy='50', name='Film', id="films"),
  #                list(col='#036c83', cx='250', cy='50', name='Pellet/Bead', id="beads"),
  #                list(col='#4ebec2', cx='350', cy='50', name='Fragment', id="fragments"),
  #                list(col='#0b516b', cx='450', cy='50', name='Fiber/Line', id="fiberlines"))
  
  start.y <- 25
  bar.height <- 400
  for (group in groups){
    name <- group$name
    perc <- filter(data.in, Figure.Name==name) %>% .$Percent.Type
    height <- perc*bar.height/100
    fill <- group$col
    cx <- group$cx
    cy <- group$cy
    
    g <- dinosvg:::svg_node("g", svg, c(id=group$name))
    dinosvg:::svg_node("rect", g, c(x="200", y=start.y, width="100", height=height, fill=fill))
    dinosvg:::svg_node("circle", g, c(cx=cx, cy=cy,r='70', fill=fill))
    dinosvg:::svg_node("text", g, c(x=cx, y=cy,'text-anchor'='middle',dy='0.33em'),
                       newXMLTextNode(text.in[paste0("relAbundance-",group$id,"-label")]))
    dinosvg:::svg_node("text", g, c(x=cx, y=cy,'text-anchor'='middle',dy='0.33em', opacity='0.2',id=paste0(group$id,".details")),
                       newXMLTextNode(text.in[paste0("relAbundance-",group$id)]))

    start.y <- start.y+height
  }
  
  dinosvg:::write_svg(svg, file=target_name)
  return(target_name)
}


#visualizeRelativeAbundance("desktop","cache/mungedRelativeAbundance.tsv","data/siteText.yaml","test.svg")

