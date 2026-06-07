#' Weighted density ridges of an environmental indicator by plastic type
#'
#' For each selected plastic type, draws a weighted density ridge of an
#' environmental indicator (e.g. `threatened_species`) across countries. Each
#' country contributes a weight equal to the quantity of that plastic type it
#' collected, so high-volume collections drive the density.
#'
#' Only rows where `parent_company == "Grand Total"` and `grand_total > 0` are
#' used, mirroring [summarize_country()] and the TidyTuesday plastics data.
#' Rows missing the chosen `env_var` are dropped before plotting.
#'
#' @param data A data frame from [load_data()] or with the same structure.
#'   Must contain `country`, `parent_company`, `grand_total`, the plastic
#'   columns named in `plastics`, and the column named in `env_var`.
#' @param plastics Character vector. Plastic-type columns to plot, drawn from
#'   `c("pet", "hdpe", "ldpe", "pp", "ps", "pvc")`. Order here controls the
#'   factor order on the y axis. Defaults to `c("pet", "hdpe", "ldpe")`.
#' @param env_var Character. Name of the environmental-indicator column to put
#'   on the x axis. Defaults to `"threatened_species"`.
#' @param log_x Logical. If `TRUE` (default), the x axis is on a log10 scale.
#'
#' @return A `ggplot` object.
#'
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom ggridges geom_density_ridges
#' @importFrom ggplot2 ggplot aes scale_x_log10 scale_fill_manual labs theme_minimal theme element_text
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' df <- load_data()
#' plot_plastic_ridges(df, plastics = c("pet", "hdpe", "ldpe"))
#' }
plot_plastic_ridges <- function(data,
                                plastics = c("pet", "hdpe", "ldpe"),
                                env_var = "threatened_species",
                                log_x = TRUE) {

  allowed <- c("pet", "hdpe", "ldpe", "pp", "ps", "pvc")
  bad <- setdiff(plastics, allowed)
  if (length(bad) > 0) {
    stop("Unknown plastic type(s): ", paste(bad, collapse = ", "),
         ". Allowed: ", paste(allowed, collapse = ", "))
  }
  if (!env_var %in% names(data)) {
    stop("env_var column not found in data: ", env_var)
  }

  long <- data |>
    dplyr::filter(
      .data$parent_company == "Grand Total",
      .data$grand_total > 0,
      !is.na(.data[[env_var]])
    ) |>
    plastics_long(cols = allowed) |>
    dplyr::filter(
      .data$count > 0,
      .data$plastic_type %in% plastics
    ) |>
    dplyr::mutate(
      plastic_label = factor(.data$plastic_label, levels = toupper(plastics))
    )

  if (nrow(long) == 0) {
    stop("No data left after filtering to the selected plastics and ", env_var)
  }

  n_plastics   <- length(unique(long$plastic_label))
  ridge_colors <- grDevices::colorRampPalette(c("#E8A0A0", "#8B1A1A"))(n_plastics)

  x_label <- if (log_x) paste0(env_var, " (log scale)") else env_var

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x      = .data[[env_var]],
      y      = .data$plastic_label,
      fill   = .data$plastic_label,
      weight = .data$count
    )
  ) +
    ggridges::geom_density_ridges(scale = 0.8, alpha = 0.7) +
    ggplot2::scale_fill_manual(values = ridge_colors) +
    ggplot2::labs(
      title    = paste0(env_var, " across selected plastic types"),
      subtitle = "Each country weighted by quantity collected",
      x        = x_label,
      y        = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      legend.position = "none"
    )

  if (log_x) {
    p <- p + ggplot2::scale_x_log10()
  }
  p
}
