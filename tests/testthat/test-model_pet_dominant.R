make_fake_plastics <- function() {
  tibble::tibble(
    country = rep(c("Atlantis", "Wakanda", "Genovia"), each = 2),
    parent_company = rep("Grand Total", 6),
    year           = rep(c(2019, 2020), 3),
    pet            = c(50, 80, 10, 30,  5, 15),
    hdpe           = c(20, 10, 40, 20,  0,  0),
    ldpe           = c(10, 10,  0,  0,  5, 10),
    pp             = c(10,  0, 30, 30,  5,  5),
    ps             = c( 5,  0,  5,  5,  0,  0),
    pvc            = c( 0,  0,  5,  5,  0,  0),
    o              = c( 5,  0, 10, 10,  0,  0),
    grand_total    = c(100,100,100,100, 15, 30),
    forested_area      = c(10, 10, 40, 40, 25, 25),
    threatened_species = c(100, 100, 500, 500, 300, 300)
  )
}

test_that("model_pet_dominant returns a tibble with 3 rows", {
  out <- model_pet_dominant(make_fake_plastics())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3)
})

test_that("model_pet_dominant returns correct column names", {
  out <- model_pet_dominant(make_fake_plastics())
  expect_named(out, c("term", "estimate", "std.error", "statistic", "p.value"))
})

test_that("model_pet_dominant returns correct predictor terms", {
  out <- model_pet_dominant(make_fake_plastics())
  expect_equal(out$term, c("(Intercept)", "forested_area", "threatened_species"))
})

test_that("model_pet_dominant errors when forested_area is missing", {
  df <- dplyr::select(make_fake_plastics(), -forested_area)
  expect_error(model_pet_dominant(df))
})

test_that("model_pet_dominant errors when threatened_species is missing", {
  df <- dplyr::select(make_fake_plastics(), -threatened_species)
  expect_error(model_pet_dominant(df))
})
