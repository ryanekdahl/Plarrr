#' Summarize plastic composition for a single country
#'
#' Computes the average proportion of each plastic type collected in a given
#' country and identifies the most common plastic type. Proportions
#' are calculated as plastic-type count divided by `grand_total`, averaged
#' across all years available for that country.
#'
#' Only rows where `parent_company == "Grand Total"` and `grand_total > 0` are
#' used, matching the per-country totals reported in the TidyTuesday plastics
#' dataset.
#'
#' @param data A data frame from [load_data()] or with the same structure.
#'   Must contain the columns `country`, `parent_company`, `grand_total`,
#'   `pet`, `hdpe`, `ldpe`, `pp`, `ps`, `pvc`, and `o`.
#' @param country_name Character. Name of the country to summarize. Must match
#'   a value present in `data$country`.
#'
#' @return A one-row tibble with columns:
#' \describe{
#'   \item{country}{The country name.}
#'   \item{avg_pet, avg_hdpe, avg_ldpe, avg_pp, avg_ps, avg_pvc, avg_o}{
#'     Average proportion of each plastic type across the country's records.}
#'   \item{leading_plastic}{The plastic type with the highest average
#'     proportion, formatted in upper case ("PET", "HDPE", …) or "Other"
#'     for the `o` category.}
#' }
#'
#' @export
#' @importFrom dplyr summarize mutate pull if_else slice_max
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove str_to_upper
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' df <- load_data()
#' summarize_country(df, "India")
#' }
summarize_country <- function(data, country_name) {
  if (!(country_name %in% data$country)) {
    stop(paste("Country not found:", country_name))
  }

  country_stats <- data |>
    filter_country(country_name, positive_only = TRUE) |>
    dplyr::summarize(
      country  = country_name,
      avg_pet  = mean(.data$pet  / .data$grand_total, na.rm = TRUE),
      avg_hdpe = mean(.data$hdpe / .data$grand_total, na.rm = TRUE),
      avg_ldpe = mean(.data$ldpe / .data$grand_total, na.rm = TRUE),
      avg_pp   = mean(.data$pp   / .data$grand_total, na.rm = TRUE),
      avg_ps   = mean(.data$ps   / .data$grand_total, na.rm = TRUE),
      avg_pvc  = mean(.data$pvc  / .data$grand_total, na.rm = TRUE),
      avg_o    = mean(.data$o    / .data$grand_total, na.rm = TRUE)
    )

  leading <- country_stats |>
    tidyr::pivot_longer(
      cols = !c("country"),
      names_to = "plastic_type",
      values_to = "avg_value"
    ) |>
    dplyr::slice_max(order_by = .data$avg_value, n = 1, with_ties = FALSE) |>
    dplyr::mutate(
      plastic_type = stringr::str_remove(.data$plastic_type, "avg_"),
      plastic_type = stringr::str_to_upper(.data$plastic_type),
      plastic_type = dplyr::if_else(.data$plastic_type == "O",
                                    "Other", .data$plastic_type)
    ) |>
    dplyr::pull(.data$plastic_type)

  country_stats |> dplyr::mutate(leading_plastic = leading)
}
