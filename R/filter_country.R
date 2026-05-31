#' Filter plastics data to one country's Grand Total rows
#'
#' Helper used by [plot_plastics()] and [summarize_country()] to subset the
#' TidyTuesday plastics data to the per-country aggregate rows
#' (`parent_company == "Grand Total"`) for a single country. Optionally drops
#' rows whose `grand_total` is zero, which is useful when downstream code
#' divides by `grand_total`.
#'
#' @param data A data frame from [load_data()] or with the same structure.
#'   Must contain `country`, `parent_company`, and `grand_total` columns.
#' @param country Character. A single country name to keep. Matched against the
#'   `country` column.
#' @param positive_only Logical. If `TRUE`, also drop rows where
#'   `grand_total == 0`. Defaults to `FALSE`.
#'
#' @return A data frame with the same columns as `data`, containing only the
#'   Grand Total rows for `country` (and, if `positive_only = TRUE`, only those
#'   with `grand_total > 0`).
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data .env
#'
#' @examples
#' \dontrun{
#' df <- load_data()
#' filter_country(df, "India")
#' filter_country(df, "India", positive_only = TRUE)
#' }
filter_country <- function(data, country, positive_only = FALSE) {
  if (!is.character(country) || length(country) != 1 || is.na(country)) {
    stop("`country` must be a single non-NA character string.")
  }
  if (!is.logical(positive_only) || length(positive_only) != 1 ||
      is.na(positive_only)) {
    stop("`positive_only` must be TRUE or FALSE.")
  }

  out <- dplyr::filter(
    data,
    .data$parent_company == "Grand Total",
    .data$country == .env$country
  )

  if (positive_only) {
    out <- dplyr::filter(out, .data$grand_total > 0)
  }

  out
}
