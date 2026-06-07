#' Load plastic waste data
#'
#' Loads the plastic waste dataset directly
#' from TidyTuesday
#'
#' @return A data frame with columns that include country, year, parent_company,
#'   plastic type totals (hdpe, ldpe, pet, pp, ps, pvc), grand_total,
#'   num_events, volunteer, and other country level metrics.
#' @export
#' @importFrom readr read_csv
#' @examples
#' df <- load_data()
#' head(df)
load_data <- function() {
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv'
  )
}
