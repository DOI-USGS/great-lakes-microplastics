getData <- function(x) UseMethod("getData")

getData.default <- function(identifier) {
  data <- NULL
  data.info <- getDataInfo()
  data.item <- data.info[[identifier]]
  if (!is.null(data.item)) {
    class(data.item) <- ifelse(exists("handler", data.item), data.item$handler, "file")
    data <- getData(data.item)
  }
  return(data)
}

getData.file <- function(data) {
  fileItem <- list()
  fileItem$location <- data$location
  fileItem$mimeType <- data$mimeType
  class(fileItem) <- "fileItem"
  return(fileItem)
}

getData.sciencebase <- function(data) {
  fileItem <- list()
  file <- getFiles(data$id)
  fileItem$location <- file
  fileItem$mimeType <- data$mimeType
  class(fileItem) <- "fileItem"
  return(fileItem)
}