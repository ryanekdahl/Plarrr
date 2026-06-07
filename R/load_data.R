#' Load plastic waste data
#'
#' Reads the merged plastic waste data from the package's `inst/extdata/`
#' folder.
#'
#' @return A data frame of plastic-waste records.
#'
#' @export
#'
#' @importFrom arrow read_parquet
#'
#' @examples
#' df <- load_data()
#' head(df)
load_data <- function() {
  parquet <- system.file("extdata", "population_plastics.parquet", package = "Plarrr")
  arrow::read_parquet(parquet)
}
