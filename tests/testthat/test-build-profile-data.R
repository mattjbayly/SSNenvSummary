test_that("build_profile_data works with Bertrand Creek", {
  strm <- sf::st_read(
    system.file("extdata/Bertrand_Creek/strm.gpkg", package = "SSNenvSummary"),
    quiet = TRUE
  )
  idx <- read.csv(
    system.file("extdata/Bertrand_Creek/line_network_us_index.csv",
                 package = "SSNenvSummary")
  )
  outlet <- 701656068L

  prof <- build_profile_data(strm, idx, outlet)

  expect_s3_class(prof, "data.frame")
  expect_true(all(c("dist", "elev", "rid", "STREAM_ORDER", "GNIS_NAME") %in% names(prof)))
  expect_true(nrow(prof) > 0)
  expect_true(all(prof$dist >= 0))
  expect_true(all(is.finite(prof$elev)))

  # All rids should be from the upstream network
  us_rids <- idx$usrid[idx$rid == outlet]
  expect_true(all(prof$rid %in% us_rids))
})

test_that("build_profile_data errors on bad outlet", {
  strm <- sf::st_read(
    system.file("extdata/Bertrand_Creek/strm.gpkg", package = "SSNenvSummary"),
    quiet = TRUE
  )
  idx <- read.csv(
    system.file("extdata/Bertrand_Creek/line_network_us_index.csv",
                 package = "SSNenvSummary")
  )
  expect_error(build_profile_data(strm, idx, 999999999L), "No upstream reaches")
})

test_that("plot_profile returns a ggplot", {
  skip_if_not_installed("ggplot2")
  strm <- sf::st_read(
    system.file("extdata/Bertrand_Creek/strm.gpkg", package = "SSNenvSummary"),
    quiet = TRUE
  )
  idx <- read.csv(
    system.file("extdata/Bertrand_Creek/line_network_us_index.csv",
                 package = "SSNenvSummary")
  )
  prof <- build_profile_data(strm, idx, 701656068L)
  g <- plot_profile(prof, theme = "light", show_names = FALSE)
  expect_s3_class(g, "ggplot")
})
