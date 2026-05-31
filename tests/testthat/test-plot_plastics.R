make_fake_plastics <- function() {
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
    grand_total = c(100,100,100,100, 15, 30)
  )
}

test_that("plot_plastics returns a ggplot object", {
  df <- make_fake_plastics()
  p  <- plot_plastics(df, country = "Atlantis")
  expect_s3_class(p, "ggplot")
})

test_that("plot_plastics returns the top n plastics (default 3)", {
  df <- make_fake_plastics()
  p  <- plot_plastics(df, country = "Atlantis")
  expect_equal(nrow(p$data), 3)
})

test_that("plot_plastics respects custom n", {
  df <- make_fake_plastics()
  p  <- plot_plastics(df, country = "Atlantis", n = 5)
  expect_equal(nrow(p$data), 5)
})

test_that("plot_plastics picks the correct top plastics for a country", {
  df <- make_fake_plastics()
  # Atlantis totals across years: pet=130, hdpe=30, ldpe=20, pp=10, ps=5, pvc=0
  p  <- plot_plastics(df, country = "Atlantis")
  expect_setequal(as.character(p$data$plastic_type), c("PET", "HDPE", "LDPE"))
})

test_that("plot_plastics computes percent of grand_total correctly", {
  df <- make_fake_plastics()
  # Atlantis grand_total summed = 200; pet summed = 130 -> 65%
  p  <- plot_plastics(df, country = "Atlantis")
  pet_pct <- p$data$percent[p$data$plastic_type == "PET"]
  expect_equal(pet_pct, 65)
})

test_that("plot_plastics sums across multiple rows for the same country", {
  df <- make_fake_plastics()
  # Wakanda hdpe: 40 + 20 = 60; grand_total: 200 -> 30%
  p  <- plot_plastics(df, country = "Wakanda")
  hdpe_pct <- p$data$percent[p$data$plastic_type == "HDPE"]
  expect_equal(hdpe_pct, 30)
})

test_that("plot_plastics ignores non-Grand-Total rows", {
  df <- make_fake_plastics()
  df_extra <- dplyr::bind_rows(
    df,
    tibble::tibble(
      country = "Atlantis",
      parent_company = "Some Brand",
      year = 2019,
      pet = 9999, hdpe = 9999, ldpe = 9999,
      pp  = 9999, ps   = 9999, pvc  = 9999,
      o   = 9999, grand_total = 9999
    )
  )
  p <- plot_plastics(df_extra, country = "Atlantis")
  expect_true(all(p$data$count < 9999))
})

test_that("plot_plastics errors when the country is not present", {
  df <- make_fake_plastics()
  expect_error(
    plot_plastics(df, country = "Narnia"),
    "No 'Grand Total' rows found"
  )
})
