make_fake_plastics <- function() {
  tibble::tibble(
    country = c("Atlantis", "Atlantis", "Atlantis",
                "Wakanda",  "Wakanda",
                "Genovia"),
    parent_company = c("Grand Total", "Grand Total", "Some Brand",
                       "Grand Total", "Grand Total",
                       "Grand Total"),
    year        = c(2019, 2020, 2019, 2019, 2020, 2019),
    pet         = c(50, 80, 999, 10, 30, 0),
    hdpe        = c(20, 10, 999, 40, 20, 0),
    grand_total = c(100, 100, 999, 100, 100, 0)
  )
}

test_that("filter_country keeps only Grand Total rows for the country", {
  df  <- make_fake_plastics()
  out <- filter_country(df, "Atlantis")

  expect_equal(nrow(out), 2)
  expect_true(all(out$country == "Atlantis"))
  expect_true(all(out$parent_company == "Grand Total"))
})

test_that("filter_country drops other countries", {
  df  <- make_fake_plastics()
  out <- filter_country(df, "Wakanda")

  expect_setequal(out$country, "Wakanda")
  expect_equal(nrow(out), 2)
})

test_that("filter_country drops non-Grand-Total rows", {
  df  <- make_fake_plastics()
  out <- filter_country(df, "Atlantis")

  expect_false(any(out$parent_company == "Some Brand"))
  expect_false(any(out$pet == 999))
})

test_that("filter_country returns zero rows when country is absent", {
  df  <- make_fake_plastics()
  out <- filter_country(df, "Narnia")

  expect_equal(nrow(out), 0)
})

test_that("filter_country with positive_only drops zero-total rows", {
  df  <- make_fake_plastics()
  out <- filter_country(df, "Genovia", positive_only = TRUE)

  expect_equal(nrow(out), 0)
})

test_that("filter_country without positive_only keeps zero-total rows", {
  df  <- make_fake_plastics()
  out <- filter_country(df, "Genovia")

  expect_equal(nrow(out), 1)
  expect_equal(out$grand_total, 0)
})

test_that("filter_country errors on invalid country input", {
  df <- make_fake_plastics()

  expect_error(filter_country(df, c("Atlantis", "Wakanda")),
               "single non-NA character string")
  expect_error(filter_country(df, NA_character_),
               "single non-NA character string")
  expect_error(filter_country(df, 42),
               "single non-NA character string")
})

test_that("filter_country errors on invalid positive_only input", {
  df <- make_fake_plastics()

  expect_error(filter_country(df, "Atlantis", positive_only = "yes"),
               "TRUE or FALSE")
  expect_error(filter_country(df, "Atlantis", positive_only = NA),
               "TRUE or FALSE")
})
