make_fake_plastics <- function() {
  tibble::tibble(
    country = c("Atlantis", "Atlantis", "Atlantis", "Atlantis",
                "Wakanda",  "Wakanda",
                "Genovia"),
    parent_company = c("Grand Total", "Grand Total",
                       "Some Brand",  "Grand Total",
                       "Grand Total", "Grand Total",
                       "Grand Total"),
    year = c(2019, 2020, 2019, 2020, 2019, 2020, 2019),
    pet         = c(50, 80, 999,  0, 10, 30,  0),
    hdpe        = c(20, 10, 999,  0, 40, 20,  0),
    ldpe        = c(10, 10, 999,  0,  0,  0,  0),
    pp          = c(10,  0, 999,  0, 30, 30,  0),
    ps          = c( 5,  0, 999,  0,  5,  5,  0),
    pvc         = c( 0,  0, 999,  0,  5,  5,  0),
    o           = c( 5,  0, 999,  0, 10, 10,  0),
    grand_total = c(100,100, 999,  0,100,100,  0)
  )
}

test_that("summarize_country returns a one-row tibble with expected cols", {
  df  <- make_fake_plastics()
  out <- summarize_country(df, "Atlantis")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_named(
    out,
    c("country",
      "avg_pet", "avg_hdpe", "avg_ldpe",
      "avg_pp",  "avg_ps",   "avg_pvc",  "avg_o",
      "leading_plastic")
  )
})

test_that("summarize_country averages proportions across years correctly", {
  df  <- make_fake_plastics()
  out <- summarize_country(df, "Atlantis")

  expect_equal(out$avg_pet,  mean(c(50, 80)  / 100))
  expect_equal(out$avg_hdpe, mean(c(20, 10)  / 100))
  expect_equal(out$avg_pp,   mean(c(10,  0)  / 100))
})

test_that("summarize_country only uses Grand Total rows with grand_total > 0", {
  df  <- make_fake_plastics()
  out <- summarize_country(df, "Atlantis")

  expect_lt(out$avg_pet, 1)
  expect_equal(out$avg_pet,  mean(c(50, 80)  / 100))
})

test_that("summarize_country picks the correct leading plastic", {
  df <- make_fake_plastics()

  expect_equal(summarize_country(df, "Atlantis")$leading_plastic, "PET")
  expect_equal(summarize_country(df, "Wakanda")$leading_plastic,  "HDPE")
})

test_that("summarize_country errors on a country not in the data", {
  df <- make_fake_plastics()
  expect_error(
    summarize_country(df, "Narnia"),
    "Country not found"
  )
})
