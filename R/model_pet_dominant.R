#' Logistic regression of PET dominance on ecological indicators
#'
#' Fits a logistic regression predicting whether a country's leading plastic
#' type is PET from forested_area and threatened_species. Uses the output of
#' [summarize_country()] mapped across all valid countries.
#'
#' @param data A data frame from [load_data()].
#' @return A tibble of model coefficients from [broom::tidy()], including
#'   estimates, standard errors, and p-values.
#' @export
#' @importFrom dplyr filter distinct pull left_join mutate
#' @importFrom purrr map_dfr
#' @importFrom broom tidy
#' @importFrom stats glm binomial
model_pet_dominant <- function(data) {
  countries <- data |>
    dplyr::filter(
      parent_company == "Grand Total",
      grand_total > 0,
      !is.na(forested_area),
      !is.na(threatened_species)
    ) |>
    dplyr::distinct(country) |>
    dplyr::pull(country)

  ecology <- data |>
    dplyr::filter(parent_company == "Grand Total") |>
    dplyr::group_by(country) |>
    dplyr::summarize(
      forested_area      = mean(forested_area,      na.rm = TRUE),
      threatened_species = mean(threatened_species, na.rm = TRUE),
      .groups = "drop"
    )

  complete <- purrr::map_dfr(countries, ~ summarize_country(data, .x)) |>
    dplyr::left_join(ecology, by = "country") |>
    dplyr::mutate(pet_dominant = as.integer(leading_plastic == "PET"))

  fit <- stats::glm(
    pet_dominant ~ forested_area + threatened_species,
    data   = complete,
    family = binomial
  )
  broom::tidy(fit)
}
