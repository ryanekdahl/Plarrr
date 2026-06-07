#' Pivot plastic-type columns to long form with display labels
#'
#' Pivots the wide plastic-type columns (`pet`, `hdpe`, `ldpe`, `pp`, `ps`,
#' `pvc`, `o`) into a long-format tibble and adds a `plastic_label` column
#' with display-ready labels (`"PET"`, `"HDPE"`, ..., `"Other"` for the `o`
#' category). Used as a shared helper by [plot_plastics()] and
#' [plot_plastic_ridges()].
#'
#' @param data A data frame containing the plastic-type columns in `cols`.
#' @param cols Character vector of plastic-type columns to pivot. Defaults to
#'   `c("pet", "hdpe", "ldpe", "pp", "ps", "pvc", "o")`.
#'
#' @return A long-format tibble with the non-pivoted columns of `data` plus
#'   `plastic_type` (raw column name), `count` (the value), and
#'   `plastic_label` (display-ready name).
#'
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate if_else all_of
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' df <- load_data()
#' plastics_long(df)
#' }
plastics_long <- function(data,
                          cols = c("pet", "hdpe", "ldpe", "pp", "ps", "pvc", "o")) {
  data |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(cols),
      names_to  = "plastic_type",
      values_to = "count"
    ) |>
    dplyr::mutate(
      plastic_label = dplyr::if_else(
        .data$plastic_type == "o", "Other", toupper(.data$plastic_type)
      )
    )
}
