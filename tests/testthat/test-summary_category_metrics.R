make_fake_plastics_env <- function() {
  tibble::tibble(
    country = rep(c("Atlantis", "Wakanda", "Genovia",
                    "Narnia",   "Oz",      "Hyrule"), each = 2),
    parent_company = rep("Grand Total", 12),
    year = rep(c(2019, 2020), 6),
    pet         = c(50, 80, 10, 30,  5, 15, 40, 60, 20, 25,  8, 12),
    hdpe        = c(20, 10, 40, 20,  0,  0, 15, 10, 30, 25,  2,  4),
    ldpe        = c(10, 10,  0,  0,  5, 10,  5,  5, 10, 10,  2,  2),
    pp          = c(10,  0, 30, 30,  5,  5, 20, 10, 20, 20,  2,  2),
    ps          = c( 5,  0,  5,  5,  0,  0,  5,  5,  5,  5,  0,  0),
    pvc         = c( 0,  0,  5,  5,  0,  0,  5,  5,  5,  5,  0,  0),
    o           = c( 5,  0, 10, 10,  0,  0, 10,  5, 10, 10,  1,  0),
    grand_total = c(100,100,100,100, 15, 30,100,100,100,100, 15, 20),
    threatened_species = c(100, 110, 300, 320, 500, 520,
                           700, 720, 900, 920, 1100, 1120),
    forested_area      = c( 10,  12,  25,  28,  40,  42,
                            55,  58,  70,  72,  85,  88)
  )
}

test_that("summary_category_metrics returns a tibble with one row per tier", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
})

test_that("summary_category_metrics labels tiers Low / Medium / High in order", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df)
  expect_equal(as.character(res$tier), c("Low", "Medium", "High"))
})

test_that("summary_category_metrics returns the expected columns", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df)
  expect_named(
    res,
    c("tier", "env_min", "env_max", "n_countries", "avg_grand_total",
      "avg_pet", "avg_hdpe", "avg_ldpe", "avg_pp", "avg_ps", "avg_pvc",
      "avg_o", "leading_plastic")
  )
})

test_that("summary_category_metrics defaults to threatened_species", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df)
  expect_gte(min(res$env_min), min(df$threatened_species))
  expect_lte(max(res$env_max), max(df$threatened_species))
})

test_that("summary_category_metrics accepts forested_area as env_var", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df, env_var = "forested_area")
  expect_gte(min(res$env_min), min(df$forested_area))
  expect_lte(max(res$env_max), max(df$forested_area))
})

test_that("summary_category_metrics errors on an unknown env_var", {
  df <- make_fake_plastics_env()
  expect_error(summary_category_metrics(df, env_var = "rainfall"))
})

test_that("summary_category_metrics no longer accepts n_tiers", {
  df <- make_fake_plastics_env()
  expect_error(summary_category_metrics(df, n_tiers = 4), "unused argument")
})

test_that("summary_category_metrics drops non-Grand-Total rows", {
  df <- make_fake_plastics_env()
  df_extra <- dplyr::bind_rows(
    df,
    tibble::tibble(
      country = "Atlantis", parent_company = "Some Brand", year = 2019,
      pet = 9999, hdpe = 9999, ldpe = 9999,
      pp  = 9999, ps   = 9999, pvc  = 9999, o = 9999,
      grand_total = 9999,
      threatened_species = 100, forested_area = 10
    )
  )
  res <- summary_category_metrics(df_extra)
  expect_true(all(res$avg_grand_total < 9999))
})

test_that("summary_category_metrics drops rows with grand_total == 0", {
  df <- make_fake_plastics_env()
  df$grand_total[1] <- 0
  res <- summary_category_metrics(df)
  expect_equal(nrow(res), 3)
})

test_that("summary_category_metrics drops rows missing env_var", {
  df <- make_fake_plastics_env()
  df$threatened_species[1:2] <- NA
  res <- summary_category_metrics(df)
  expect_false(any(is.na(res$env_min)))
})

test_that("summary_category_metrics avg shares are in [0, 1]", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df)
  share_cols <- c("avg_pet", "avg_hdpe", "avg_ldpe",
                  "avg_pp", "avg_ps", "avg_pvc", "avg_o")
  shares <- unlist(res[share_cols])
  expect_true(all(shares >= 0 & shares <= 1))
})

test_that("summary_category_metrics leading_plastic is one of the known labels", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df)
  expect_true(all(res$leading_plastic %in%
                    c("PET", "HDPE", "LDPE", "PP", "PS", "PVC", "Other")))
})

test_that("summary_category_metrics counts distinct countries per tier", {
  df <- make_fake_plastics_env()
  res <- summary_category_metrics(df)
  expect_equal(sum(res$n_countries), dplyr::n_distinct(df$country))
})
