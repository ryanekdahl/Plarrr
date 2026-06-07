make_fake_plastics_env <- function() {
  tibble::tibble(
    country = rep(c("Atlantis", "Wakanda", "Genovia"), each = 2),
    parent_company = rep("Grand Total", 6),
    year = rep(c(2019, 2020), 3),
    pet         = c(50, 80, 10, 30,  5, 15),
    hdpe        = c(20, 10, 40, 20,  0,  0),
    ldpe        = c(10, 10,  0,  0,  5, 10),
    pp          = c(10,  0, 30, 30,  5,  5),
    ps          = c( 5,  0,  5,  5,  0,  0),
    pvc         = c( 0,  0,  5,  5,  0,  0),
    o           = c( 5,  0, 10, 10,  0,  0),
    grand_total = c(100,100,100,100, 15, 30),
    threatened_species = c(100, 110, 500, 520, 900, 950),
    forested_area      = c( 10,  12,  35,  38,  60,  62)
  )
}

test_that("plot_plastic_ridges returns a ggplot object", {
  df <- make_fake_plastics_env()
  p <- plot_plastic_ridges(df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_plastic_ridges uses a density-ridges geom", {
  df <- make_fake_plastics_env()
  p <- plot_plastic_ridges(df)
  expect_true(inherits(p$layers[[1]]$geom, "GeomDensityRidges"))
})

test_that("plot_plastic_ridges defaults to pet/hdpe/ldpe in that order", {
  df <- make_fake_plastics_env()
  p <- plot_plastic_ridges(df)
  expect_equal(levels(p$data$plastic_label), c("PET", "HDPE", "LDPE"))
})

test_that("plot_plastic_ridges respects a custom plastics vector and order", {
  df <- make_fake_plastics_env()
  p <- plot_plastic_ridges(df, plastics = c("pp", "pet"))
  expect_equal(levels(p$data$plastic_label), c("PP", "PET"))
  expect_setequal(as.character(unique(p$data$plastic_label)), c("PP", "PET"))
})

test_that("plot_plastic_ridges drops zero-count and non-Grand-Total rows", {
  df <- make_fake_plastics_env()
  df_extra <- dplyr::bind_rows(
    df,
    tibble::tibble(
      country = "Atlantis",
      parent_company = "Some Brand",
      year = 2019,
      pet = 9999, hdpe = 9999, ldpe = 9999,
      pp  = 9999, ps   = 9999, pvc  = 9999,
      o   = 9999, grand_total = 9999,
      threatened_species = 100, forested_area = 10
    )
  )
  p <- plot_plastic_ridges(df_extra)
  expect_true(all(p$data$count < 9999))
  expect_true(all(p$data$count > 0))
})

test_that("plot_plastic_ridges drops rows missing env_var", {
  df <- make_fake_plastics_env()
  df$threatened_species[1:2] <- NA
  p <- plot_plastic_ridges(df)
  expect_false(any(is.na(p$data$threatened_species)))
})

test_that("plot_plastic_ridges accepts an alternate env_var", {
  df <- make_fake_plastics_env()
  p <- plot_plastic_ridges(df, env_var = "forested_area")
  expect_true("forested_area" %in% names(p$data))
})

test_that("plot_plastic_ridges errors on unknown plastic types", {
  df <- make_fake_plastics_env()
  expect_error(
    plot_plastic_ridges(df, plastics = c("pet", "bogus")),
    "Unknown plastic type"
  )
})

test_that("plot_plastic_ridges errors when env_var column is missing", {
  df <- make_fake_plastics_env()
  expect_error(
    plot_plastic_ridges(df, env_var = "rainfall"),
    "env_var column not found"
  )
})

test_that("plot_plastic_ridges errors when filtering removes all rows", {
  df <- make_fake_plastics_env()
  df$grand_total <- 0
  expect_error(
    plot_plastic_ridges(df),
    "No data left after filtering"
  )
})

test_that("plot_plastic_ridges adds a log scale on x when log_x = TRUE", {
  df <- make_fake_plastics_env()
  p_log    <- plot_plastic_ridges(df, log_x = TRUE)
  p_linear <- plot_plastic_ridges(df, log_x = FALSE)

  has_log <- function(p) {
    any(vapply(p$scales$scales,
               function(s) "x" %in% s$aesthetics && identical(s$trans$name, "log-10"),
               logical(1)))
  }
  expect_true(has_log(p_log))
  expect_false(has_log(p_linear))
})
