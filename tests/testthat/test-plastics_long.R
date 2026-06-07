make_fake_plastics <- function() {
  tibble::tibble(
    country = c("Atlantis", "Wakanda"),
    parent_company = c("Grand Total", "Grand Total"),
    year = c(2019, 2020),
    pet  = c(50, 80),
    hdpe = c(20, 10),
    ldpe = c(10, 10),
    pp   = c(10,  0),
    ps   = c( 5,  0),
    pvc  = c( 0,  0),
    o    = c( 5,  0),
    grand_total = c(100, 100)
  )
}

test_that("plastics_long returns a tibble in long form", {
  df <- make_fake_plastics()
  res <- plastics_long(df)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), nrow(df) * 7)
})

test_that("plastics_long adds plastic_type, count, and plastic_label columns", {
  df <- make_fake_plastics()
  res <- plastics_long(df)
  expect_true(all(c("plastic_type", "count", "plastic_label") %in% names(res)))
})

test_that("plastics_long preserves non-pivoted columns", {
  df <- make_fake_plastics()
  res <- plastics_long(df)
  expect_true(all(c("country", "parent_company", "year", "grand_total")
                  %in% names(res)))
})

test_that("plastics_long drops the pivoted columns from the output", {
  df <- make_fake_plastics()
  res <- plastics_long(df)
  pivoted <- c("pet", "hdpe", "ldpe", "pp", "ps", "pvc", "o")
  expect_false(any(pivoted %in% names(res)))
})

test_that("plastics_long maps 'o' to 'Other' and uppercases the rest", {
  df <- make_fake_plastics()
  res <- plastics_long(df)
  expect_setequal(
    unique(res$plastic_label),
    c("PET", "HDPE", "LDPE", "PP", "PS", "PVC", "Other")
  )
  expect_true(all(res$plastic_label[res$plastic_type == "o"] == "Other"))
  expect_true(all(res$plastic_label[res$plastic_type == "pet"] == "PET"))
})

test_that("plastics_long count values match the original wide values", {
  df  <- make_fake_plastics()
  res <- plastics_long(df)
  row1_pet <- res$count[res$country == "Atlantis" & res$plastic_type == "pet"]
  expect_equal(row1_pet, df$pet[df$country == "Atlantis"])
})

test_that("plastics_long respects a custom cols vector", {
  df  <- make_fake_plastics()
  res <- plastics_long(df, cols = c("pet", "hdpe"))
  expect_equal(nrow(res), nrow(df) * 2)
  expect_setequal(unique(res$plastic_type), c("pet", "hdpe"))
  expect_true(all(c("ldpe", "pp", "ps", "pvc", "o") %in% names(res)))
})

test_that("plastics_long errors when a requested column is missing", {
  df <- make_fake_plastics()
  expect_error(plastics_long(df, cols = c("pet", "bogus")))
})
