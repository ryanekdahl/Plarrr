#' Summarize plastic metrics by environmental-variable tier
#'
#' Bins each country-year `Grand Total` row by an environmental indicator
#' (`threatened_species` or `forested_area`) into three quantile tiers
#' (`"Low"`, `"Medium"`, `"High"`), then summarizes plastic-collection
#' metrics within each tier. Useful for comparing plastic composition
#' across countries that share an environmental profile (e.g. high vs. low
#' forest coverage).
#'
#' Only rows where `parent_company == "Grand Total"`, `grand_total > 0`, and
#' `env_var` is non-missing are used.
#'
#' @param data A data frame from [load_data()] or with the same structure.
#'   Must contain `country`, `parent_company`, `grand_total`, the plastic
#'   columns (`pet`, `hdpe`, `ldpe`, `pp`, `ps`, `pvc`, `o`), and the chosen
#'   `env_var`.
#' @param env_var Character. Environmental indicator to tier by. One of
#'   `"threatened_species"` or `"forested_area"`. Defaults to
#'   `"threatened_species"`.
#'
#' @return A tibble with one row per tier and columns:
#' \describe{
#'   \item{tier}{Tier label (factor): `"Low"`, `"Medium"`, or `"High"`.}
#'   \item{env_min, env_max}{Range of `env_var` covered by the tier.}
#'   \item{n_countries}{Number of distinct countries in the tier.}
#'   \item{avg_grand_total}{Mean total plastic collected per row in the tier.}
#'   \item{avg_pet, avg_hdpe, avg_ldpe, avg_pp, avg_ps, avg_pvc, avg_o}{
#'     Mean share (count / `grand_total`) of each plastic type in the tier.}
#'   \item{leading_plastic}{Plastic type with the highest average share in
#'     the tier (`"PET"`, `"HDPE"`, ..., or `"Other"`).}
#' }
#'
#' @export
#' @importFrom dplyr filter mutate group_by summarize n_distinct slice_max ungroup select left_join
#' @importFrom rlang .data
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' df <- load_data()
#' summary_category_metrics(df, env_var = "threatened_species")
#' summary_category_metrics(df, env_var = "forested_area")
#' }
summary_category_metrics <- function(data,
                                     env_var = c("threatened_species",
                                                 "forested_area")) {
  env_var <- match.arg(env_var)

  tier_labels <- c("Low", "Medium", "High")

  filtered <- data |>
    dplyr::filter(
      .data$parent_company == "Grand Total",
      .data$grand_total > 0,
      !is.na(.data[[env_var]])
    )

  breaks <- stats::quantile(
    filtered[[env_var]],
    probs = seq(0, 1, length.out = 4),
    na.rm = TRUE
  )

  filtered <- filtered |>
    dplyr::mutate(
      tier = cut(.data[[env_var]],
                 breaks         = unique(breaks),
                 labels         = tier_labels[seq_len(length(unique(breaks)) - 1)],
                 include.lowest = TRUE)
    )

  wide <- filtered |>
    dplyr::group_by(.data$tier) |>
    dplyr::summarize(
      env_min         = min(.data[[env_var]], na.rm = TRUE),
      env_max         = max(.data[[env_var]], na.rm = TRUE),
      n_countries     = dplyr::n_distinct(.data$country),
      avg_grand_total = mean(.data$grand_total, na.rm = TRUE),
      avg_pet         = mean(.data$pet  / .data$grand_total, na.rm = TRUE),
      avg_hdpe        = mean(.data$hdpe / .data$grand_total, na.rm = TRUE),
      avg_ldpe        = mean(.data$ldpe / .data$grand_total, na.rm = TRUE),
      avg_pp          = mean(.data$pp   / .data$grand_total, na.rm = TRUE),
      avg_ps          = mean(.data$ps   / .data$grand_total, na.rm = TRUE),
      avg_pvc         = mean(.data$pvc  / .data$grand_total, na.rm = TRUE),
      avg_o           = mean(.data$o    / .data$grand_total, na.rm = TRUE),
      .groups = "drop"
    )

  leading <- filtered |>
    plastics_long() |>
    dplyr::mutate(share = .data$count / .data$grand_total) |>
    dplyr::group_by(.data$tier, .data$plastic_label) |>
    dplyr::summarize(avg_share = mean(.data$share, na.rm = TRUE),
                     .groups = "drop_last") |>
    dplyr::slice_max(.data$avg_share, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(.data$tier, leading_plastic = .data$plastic_label)

  dplyr::left_join(wide, leading, by = "tier")
}
