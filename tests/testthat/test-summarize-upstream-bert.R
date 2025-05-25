test_that("test-summarize-upstream-bert", {

  # Load Bertrand Creek Data
  # Load package data
  filename <- system.file("extdata/Bertrand_Creek/strm.gpkg",
                          package = "SSNenvSummary")

  strm <- st_read(filename)
  filename <- system.file("extdata/Bertrand_Creek/rca.gpkg",
                          package = "SSNenvSummary")
  rca_polygons <- st_read(filename)
  filename <- system.file("extdata/Bertrand_Creek/poly_stream_network_us_index.csv",
                          package = "SSNenvSummary")
  poly_stream_network_us_index <- read.csv(filename)

  # Check UTM Zone
  utm_zone <- getUTMzone(rca_polygons[1:5,], epsg_code = TRUE)

  # ----------------------------------------------
  # Simple Drainage Area Summary
  # ----------------------------------------------
  us_da <- summarize_upstream(net = poly_stream_network_us_index,
                      summary_type = "total_upstream_area",
                      summary_field = NA,
                      rca_polygons = rca_polygons,
                      rca_env_data = rca_polygons,
                      utm_zone = utm_zone,
                      output_fieldname = "drainage_area_m2")

  strm <- merge(strm, us_da, by.x = "rid", by.y = "rid", all.x = TRUE)
  strm$log_da <- log(strm$drainage_area_m2 + 1)
  # plot(strm["log_da"])

  # ..........................
  # Perform some basic checks
  # ..........................


  # Check Main outlet
  check_us_rids <- poly_stream_network_us_index$rca_id[poly_stream_network_us_index$rid == 701656068]
  check_us_rids <- sort(check_us_rids)
  # Main outlet should include all RCAs
  expect_true(all(check_us_rids == sort(rca_polygons$rca_id)))
  check_da <- round(strm$drainage_area_m2[strm$rid == 701656068], 0)
  # Drainage area should be the sum of all RCAs
  check_area <- round(sum(as.numeric(st_area(st_transform(rca_polygons, utm_zone)))), 0)
  expect_true(check_area == check_da)

  check_us_rids <- poly_stream_network_us_index$rca_id[poly_stream_network_us_index$rid == 701655353]
  expect_true(check_us_rids == 4263)
  check_area <- round(as.numeric(st_area(rca_polygons[rca_polygons$rca_id == 4263, ])), 0)
  expect_true(check_area == 17865)
  strm$drainage_area_m2[strm$rid == 701655353]
  new_poly <- sf::st_transform(rca_polygons[rca_polygons$rca_id == 4263, ], utm_zone)
  check_area <- round(as.numeric(st_area(new_poly)), 0)
  # Check Drainage area
  check_da <- round(strm$drainage_area_m2[strm$rid == 701655353], 0)
  expect_true(check_area == check_da)


  check_us_rids <- poly_stream_network_us_index$rca_id[poly_stream_network_us_index$rid == 701655301]
  expect_true(all(check_us_rids == c(4263, 13219)))
  check_area <- round(as.numeric(st_area(rca_polygons[rca_polygons$rca_id %in% c(4263, 13219), ])), 0)
  check_area <- sum(check_area)
  expect_true(check_area == 454393)
  strm$drainage_area_m2[strm$rid == 701655301]
  new_poly <- sf::st_transform(rca_polygons[rca_polygons$rca_id %in% c(4263, 13219), ], utm_zone)
  check_area <- sum(round(as.numeric(st_area(new_poly)), 0))
  # Check Drainage area
  check_da <- round(strm$drainage_area_m2[strm$rid == 701655301], 0)
  expect_true(check_area == check_da)





  # ----------------------------------------------
  # linear_density with roads
  # ----------------------------------------------

  filename <- system.file("extdata/Bertrand_Creek/env/roads.gpkg",
                          package = "SSNenvSummary")
  rca_env_data <- st_read(filename)

  us_da <- summarize_upstream(net = poly_stream_network_us_index,
                              summary_type = "linear_density",
                              summary_field = NA,
                              rca_polygons = rca_polygons,
                              rca_env_data = rca_env_data,
                              utm_zone = utm_zone,
                              output_fieldname = "roads_density_km_per_km2")


  # head(us_da)

  strm <- merge(strm, us_da, by.x = "rid", by.y = "rid", all.x = TRUE)
  # plot(strm["roads_density_km_per_km2"])

  # ..........................
  # Perform some basic checks
  # ..........................

  # Check outlet values
  check_outlet_func <- round(strm$roads_density_km_per_km2[strm$rid == 701656068], 4)
  check_outlet_manual <- round((sum(as.numeric(st_length(rca_env_data)))/1000)/sum(as.numeric(st_area(rca_polygons))/1000000), 4)
  expect_true(round(check_outlet_func, 2) == round(check_outlet_manual, 2))


  check_outlet_func <- round(strm$roads_density_km_per_km2[strm$rid == 701653915], 4)
  us_area_km2 <- (482219 + 382212 + 487037)/(1000*1000)
  us_length_km <- 9.077
  rds <- rca_env_data[rca_env_data$rca_id %in% c(poly_stream_network_us_index$rca_id[poly_stream_network_us_index$rid == 701653915]), ]
  check_length <- sum(as.numeric(st_length(rds)))
  expect_true(abs(check_length/1000 - us_length_km) < 1)
  check_outlet_man <- round(check_length/1000/us_area_km2, 4)
  expect_true(round(check_outlet_func, 1) == round(check_outlet_man, 1))





})
