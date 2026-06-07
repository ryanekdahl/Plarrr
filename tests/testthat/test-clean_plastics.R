make_fake_plastics <- function() {
  tibble::tibble(
    country     = c("Taiwan_ Republic of China (ROC)", "Taiwan_ Republic of China (ROC)",
                    "United Kingdom of Great Britain & Northern Ireland", "United Kingdom of Great Britain & Northern Ireland",
                    "Cote D_ivoire", "ECUADOR", "NIGERIA", "Korea",
                    "EMPTY", "Genovia", "Genovia"),
    parent_company = rep("Grand Total", 11),
    year           = c(2019, 2020, 2019, 2020, 2019, 2019, 2019, 2019, 2019, 2019, 2020),
    pet            = c(50, 80, 10, 30,  5, 15, 20, 10,  5, 40, 60),
    hdpe           = c(20, 10, 40, 20,  0,  0, 10,  5,  0, 10, 10),
    ldpe           = c(10, 10,  0,  0,  5, 10,  5,  5,  0,  5,  5),
    pp             = c(10,  0, 30, 30,  5,  5, 10, 10,  0, 20, 10),
    ps             = c( 5,  0,  5,  5,  0,  0,  5,  5,  0,  5,  5),
    pvc            = c( 0,  0,  5,  5,  0,  0,  0,  5,  0,  5,  5),
    o              = c( 5,  0, 10, 10,  0,  0,  0,  5,  0,  5,  5),
    grand_total    = c(100,100,100,100, 15, 30, 50, 45,  5,  90, 100)
  )
}

test_that("clean_plastics recodes Taiwan", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(all(out$country[1:2] == "Taiwan"))
  expect_false(any(out$country == "Taiwan_ Republic of China (ROC)", na.rm = TRUE))
})

test_that("clean_plastics recodes United Kingdom", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(all(out$country[3:4] == "United Kingdom"))
  expect_false(any(out$country == "United Kingdom of Great Britain & Northern Ireland", na.rm = TRUE))
})

test_that("clean_plastics recodes Cote D_ivoire", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(any(out$country == "Cote d'Ivoire", na.rm = TRUE))
  expect_false(any(out$country == "Cote D_ivoire", na.rm = TRUE))
})

test_that("clean_plastics recodes ECUADOR and NIGERIA", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(any(out$country == "Ecuador", na.rm = TRUE))
  expect_true(any(out$country == "Nigeria", na.rm = TRUE))
  expect_false(any(out$country == "ECUADOR", na.rm = TRUE))
  expect_false(any(out$country == "NIGERIA", na.rm = TRUE))
})

test_that("clean_plastics recodes Korea to South Korea", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(any(out$country == "South Korea", na.rm = TRUE))
  expect_false(any(out$country == "Korea", na.rm = TRUE))
})

test_that("clean_plastics sets junk country rows to NA", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(any(is.na(out$country)))
  expect_false(any(out$country == "EMPTY", na.rm = TRUE))
})

test_that("clean_plastics leaves unaffected country names unchanged", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(all(out$country[10:11] == "Genovia"))
})

test_that("clean_plastics adds proportion columns for all plastic types", {
  out <- clean_plastics(make_fake_plastics())
  expect_true(all(c("pet_prop", "hdpe_prop", "ldpe_prop",
                    "pp_prop", "ps_prop", "pvc_prop", "o_prop") %in% names(out)))
})

test_that("clean_plastics proportion columns are between 0 and 1", {
  out <- clean_plastics(make_fake_plastics())
  prop_cols <- c("pet_prop", "hdpe_prop", "ldpe_prop",
                 "pp_prop", "ps_prop", "pvc_prop", "o_prop")
  expect_true(all(dplyr::select(out, dplyr::all_of(prop_cols)) >= 0, na.rm = TRUE))
  expect_true(all(dplyr::select(out, dplyr::all_of(prop_cols)) <= 1, na.rm = TRUE))
})
