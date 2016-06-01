#' @import whisker
buildPages <- function(viz.yaml) {
  pages <- viz.yaml[["pages"]]
  
  info <- getTemplateInfo()
  for (page in pages) {
    template <- readLines(page$template)
    data <- readData(page[["context"]])
    # TODO these should be moved to a higher level at some point
    data[["ga-tracking-id"]] <- googleAnalyticsId()
    data[["vizlab-page-path"]] <- viz.yaml$info$path
    for (section in page$sections) {
      data <- append(data, info$data[[section]])
      
      # TODO make sure we get figures in there too
      # TODO also maybe pull subset of partials
    }
    figureInfo <- getFigureInfo()
    for (figure in figureInfo) {
      output <- list(figure$outputs$desktop)
      names(output) <- figure$id
      data <- append(data, output)
    }
    cat(whisker.render(template = template, partials = info$partials, data = data),
        file=paste0("target/", page$id, ".html"))
  }
}