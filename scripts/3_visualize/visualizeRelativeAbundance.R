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

  groups <- list(list(col='#aadedc', cx='85', cy='75', name='Pellet/Bead', id="beads"),
                 list(col='#26b9da', cx='400', cy='146.5', name='Film', id="films"),
                 list(col='#036c83', cx='85', cy='218', name='Foam', id="foams"),
                 list(col='#4ebec2', cx='400', cy='300', name='Fragment', id="fragments"),
                 list(col='#0b516b', cx='85', cy='360', name='Fiber/Line', id="fiberlines"))

  start.y <- 25
  bar.height <- 400
  for (group in groups){
    name <- group$name
    perc <- filter(data.in, Figure.Name==name) %>% .$Percent.Type
    height <- perc*bar.height/100
    fill <- group$col
    cx <- as.numeric(group$cx)
    cy <- as.numeric(group$cy)
    
    offset.x <- -50
    offset.y <- -30

    opacity <- '0.2'
    
    g <- dinosvg:::svg_node("g", svg, c(id=group$name))
    dinosvg:::svg_node("rect", g, c(x="200", y=start.y, width="100", height=height, fill=fill, opacity = opacity))
    dinosvg:::svg_node("circle", g, c(cx=cx, cy=cy,r='70', fill=fill, opacity = opacity))
    dinosvg:::svg_node("text", g, c(x=cx+offset.x, y=cy+offset.y,fill="#FFFFFF",
                                    'text-anchor'='left',dy='0.33em'),
                       newXMLTextNode(text.in[paste0("relAbundance-",group$id,"-label")]))
    dinosvg:::svg_node("text", g, c(x=cx, y=cy,'text-anchor'='middle',dy='0.23em',
                                                opacity='0.2',id=paste0(group$id,".details"),
                                                'font-size'='2em'),
                                   newXMLTextNode(paste(sprintf(fmt = "%1.1f",perc),"%")))

    start.y <- start.y+height
  }
  
  dinosvg:::write_svg(svg, file=target_name)
  return(target_name)
}


# visualizeRelativeAbundance("desktop","cache/mungedRelativeAbundance.tsv","data/siteText.yaml","test.svg")

