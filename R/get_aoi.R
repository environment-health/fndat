#' Script to get the area of interest for the assessment
#'
#' @export
get_aoi <- function() {
  # Load federal bioregions
  dat <- pipedat:::basemap$can

  # Export data
  if (!file.exists("data/data-basemap/")) dir.create("data/data-basemap/")
  sf::st_write(
    obj = dat,
    dsn = "./data/data-basemap/aoi.geojson",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  # Export for lazy load
  save(dat, file = './data/aoi.RData')
}