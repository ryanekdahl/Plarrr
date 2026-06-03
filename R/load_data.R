#' Load plastic waste data
#'
#' @return A data frame with columns that include country, year, parent_company,
#'   plastic type totals (hdpe, ldpe, pet, pp, ps, pvc), grand_total,
#'   num_events, volunteer, and other country level metrics.
#'
#' @export
#'
#' @importFrom here here
#' @importFrom arrow read_parquet write_parquet
#' @importFrom readr read_csv
#' @importFrom dplyr mutate case_when
#'
#' @examples
#' df <- load_data()
#' head(df)
load_data <- function() {
  # reading existing parquet file
  dir <- here::here("inst", "pokemon.parquet")

  if (file.exists(dir)) {
    return(arrow::read_parquet(dir))
  }

  # creates a parquet file from csv data set
  path <- here::here("inst", "population_plastics.csv")
  plastic_data <- readr::read_csv(path) |>
    # standardizing countries
    dplyr::mutate(country = dplyr::case_when(
      country == "United States Of America" ~ "United States of America",
      country == "Taiwan_ Republic Of China (Roc)" ~ "Taiwan",
      TRUE ~ country
    ))
  arrow::write_parquet(plastic_data, dir)
  return(plastic_data)

}
