#' @import yaml
runAll <- function() {
  viz.yaml <- yaml::yaml.load_file("viz.yaml")
  
  dir.create("target")
  buildPages()
  # TODO describe this stuff in yaml
  file.copy("images", "target", recursive = TRUE)
  file.copy("layout/js", "target", recursive = TRUE)
  file.copy("layout/css", "target", recursive = TRUE)
}