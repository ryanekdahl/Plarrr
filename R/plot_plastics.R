#' Top plastic types for a country
#'
#' Horizontal bar chart of a country's `n` most-collected plastic types, with
#' each bar labelled by the share (percent) of that country's total plastic
#' collected. If a country has multiple rows (e.g. multiple years), counts are
#' summed before ranking.
#'
#' @param data A data frame from [load_data()] or with the same structure.
#'   Must contain `country`, `parent_company`, `grand_total`, and the plastic
#'   columns `pet`, `hdpe`, `ldpe`, `pp`, `ps`, `pvc`, `o`.
#' @param country Character. The country to plot. Matched against the
#'   `country` column on `parent_company == "Grand Total"` rows.
#' @param n Integer. Number of top plastic types to show. Defaults to `3`.
#'
#' @return A `ggplot` object.
#'
#' @export
#' @importFrom dplyr filter summarize across all_of select slice_max mutate
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_x_continuous expansion labs theme_minimal theme element_text
#' @importFrom rlang .data .env
#'
#' @examples
#' \dontrun{
#' df <- load_data()
#' plot_plastics(df, country = "Switzerland")
#' }
plot_plastics <- function(data, country, n = 3) {

  allowed <- c("pet", "hdpe", "ldpe", "pp", "ps", "pvc", "o")

  totals <- data |>
    dplyr::filter(
      .data$parent_company == "Grand Total",
      .data$country == .env$country
    ) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(c(allowed, "grand_total")),
        \(x) sum(x, na.rm = TRUE)
      )
    )

  if (nrow(totals) == 0 || totals$grand_total == 0) {
    stop("No 'Grand Total' rows found for country: ", country)
  }

  plastic_total <- totals$grand_total

  top <- totals |>
    dplyr::select(dplyr::all_of(allowed)) |>
    plastics_long(cols = allowed) |>
    dplyr::mutate(percent = .data$count / plastic_total * 100) |>
    dplyr::slice_max(.data$count, n = n, with_ties = FALSE) |>
    dplyr::mutate(
      plastic_label = factor(.data$plastic_label,
                             levels = .data$plastic_label[order(.data$count)])
    )

  ggplot2::ggplot(
    top,
    ggplot2::aes(x = .data$percent, y = .data$plastic_label)
  ) +
    ggplot2::geom_col(fill = "#8B1A1A") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", .data$percent)),
      hjust = -0.1, size = 4
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title    = paste0("Top ", n, " plastics in ", country),
      subtitle = "Share of total plastic collected",
      x        = "Percent of total plastic collected",
      y        = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11)
    )
}
