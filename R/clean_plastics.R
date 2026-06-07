#' Cleans and recodes raw plastics data
#'
#' Standardizes country names, removes "bad" rows, and adds plastic proportion
#' columns. Uses data.table for better in-place mutation.
#'
#' @param data A data frame from [load_data()].
#' @return A data.table with corrected country names and added
#'   `_prop` columns for each plastic type.
#' @export
#' @importFrom data.table as.data.table :=

clean_plastics <- function(data) {
  prop_cols  <- c("pet", "hdpe", "ldpe", "pp", "ps", "pvc", "o")
  prop_names <- paste0(prop_cols, "_prop")

  dt <- data.table::as.data.table(data)

  dt[country == "Taiwan_ Republic of China (ROC)",
     country := "Taiwan"]
  dt[country == "United Kingdom of Great Britain & Northern Ireland",
     country := "United Kingdom"]
  dt[country == "Cote D_ivoire",
     country := "Cote d'Ivoire"]
  dt[country == "ECUADOR",
     country := "Ecuador"]
  dt[country == "NIGERIA",
     country := "Nigeria"]
  dt[country == "Korea",
     country := "South Korea"]
  dt[country %in% c("EMPTY", "Kalis Sparkling Water (P) Ltd\"", "crispy date\""),
     country := NA_character_]

  dt[, (prop_names) := lapply(.SD, function(x) x / grand_total),
     .SDcols = prop_cols]

  dt
}
