readData <- function(x) UseMethod("readData")

readData.default <- function(identifier) {
  data <- getData(identifier)
  return(readData(data))
}

readData.fileItem <- function(file.item) {
  mimeType <- file.item[["mimeType"]]
  class <- switch(mimeType,
                  "text/csv" = "csv",
                  "text/yaml" = "yaml",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "excel",
                  "none")
  class(file.item) <- class
  return(readData(file.item))
}

readData.csv <- function(data) {
  read.csv(data$filename)
}

readData.yaml <- function(data) {
  yaml.load_file(data$filename)
}

readData.excel <- function(data) {
  read_excel(data$filename)
}

readData.none <- function(data) {
  throw("Could not read file of this type")
}