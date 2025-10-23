#' Summarize Upstream Values
#'
#' Accumulator function to calculate values upstream of each stream reach.
#'
#' @param net A `data.frame` representing network relationships between a `rid` (stream reach ID) to all upstream `rca_id`s. This data.frame must be in long format with two columns (`rid` and `rca_id`). `rid` represents the stream reach ID and `rca_id` represents all upstream RCA polygon IDs associated with that reach.
#' @param summary_type Character. One of:
#'   - `"total_upstream_area"`
#'   - `"percent_coverage"`
#'   - `"linear_density"`
#'   - `"feature_count"`
#'   - `"area_weighted_mean"`
#' @param summary_field Character. Name of the field in `rca_env_data` to summarize when
#'   `summary_type = "area_weighted_mean"`. Ignored for the other summary types.
#' @param rca_polygons An `sf` object of RCA (reach contributing area) polygons, each with a
#'   unique `rca_id` field (e.g. from `./network/NICA_rca.gpkg`).
#' @param rca_env_data A `data.frame` or `sf` object of environmental data summarized to the
#'   RCA polygons. Must include a matching `rca_id` column.
#' @param utm_zone Integer format EPSG code for the local UTM zone (e.g., 26910).
#'   Use `getUTMzone()` to determine the correct zone for your `sf` object.
#' @param output_fieldname Character. Name for the new output column (e.g.
#'   `"upstream_basin_elevation"`).
#'
#' @details
#' The following summary types are available:
#' - **total_upstream_area**
#'   Calculates the sum of the area of the upstream RCA polygons.
#'   Useful for total drainage area.
#'
#' - **percent_coverage**
#'   Calculates the area of the environmental layer upstream divided by the total upstream basin area (km²/km²).
#'   Useful for land‑cover metrics (e.g. % agricultural land).
#'
#' - **linear_density**
#'   Calculates the total length of linear features upstream divided by the upstream basin area (km/km²).
#'   Common example: road density.
#'
#' - **feature_count**
#'   Counts the number of features upstream and divides by the basin area (count/km²).
#'   Examples: number of water licenses or stream crossings per unit area.
#'
#' - **area_weighted_mean**
#'   Computes an area‑weighted mean of a continuous variable (e.g. elevation, temperature).
#'   Requires `summary_field` to specify which variable to average.
#'
#' @return
#' A `data.frame` with the upstream summary values joined back to the stream network.
#' @importFrom dplyr group_by summarise n
#' @importFrom stats weighted.mean
#' @importFrom rlang .data
#' @export
summarize_upstream <- function(net = NA,
                               summary_type = NA,
                               summary_field = NA,
                               rca_polygons = NA,
                               rca_env_data = NA,
                               utm_zone = NA,
                               output_fieldname = "field_name") {

  ## pacify “no visible binding” notes
  rid <- rca_id <- area_m2 <- rcavals <- tmp_area <- sfield <- . <- NULL

  # Make the network a data.table - remove duplicates (just to be safe)
  # print("Building network data table")
  data.table::setDT(net)
  class(net)

  # Index keys to improve speed
  keycols = c("rca_id", "rid")
  data.table::setkeyv(net, keycols)
  data.table::key(net)

  all_variables <- list()

  reach_out <- data.frame(rid = unique(net$rid))

  # If rca_env_data is missing for simple upstream summary then use rca polygons
  if(length(rca_env_data) == 1) {
    if(is.na(rca_env_data) & summary_type == "total_upstream_area") {
      rca_env_data <- rca_polygons
    }
  }


  # RCA summary data
  dat <- rca_env_data

  # Fix first colname if not rca_id
  if (!("rca_id" %in% colnames(dat))) {
    stop("rca_env_data must have an rca_id column...")
  }

  # Make sure utm zone is numeric
  utm_zone <- as.numeric(as.character(utm_zone))


  # ===================================================
  # Run simple area-based summary for percent coverage
  if (summary_type %in% c("total_upstream_area")) {

    # Look at area or length of target layer
    # print("Converting to utm projection...")

    dat <- sf::st_transform(dat, utm_zone)

    # Calculate area as m2
    if (summary_type == "total_upstream_area") {
      dat$area_m2 <- round(as.numeric(sf::st_area(dat)), 6)
    }

    area_metric <- sum(dat$area_m2, na.rm = TRUE)

    # Summarize area by RCA id
    dat_df <- dat

    if(class(dat_df)[1] != "data.frame" | length(class(dat_df)) > 1) {
      sf::st_geometry(dat_df) <- NULL
    }

    dat_sum <-
      dat_df %>% group_by(rca_id) %>% summarise(rcavals = sum(area_m2, na.rm = TRUE))
    mydt <- dat_sum

    # Convert RCA data to simple data.table object for speed.
    # from data.frame to data.table
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)

    # Merge to the stream to rid index table
    # A giant summary table will be produced to group-by
    # (rca polygons us and their associated values)
    # my_merge <- merge(mydt, net, by.x = "rca_id", by.y = "rca_id")
    my_merge <- mydt[net, nomatch=0]

    # Calculate the total sum for each stream reach (total upstream area)
    # data.table group-by/summarize function (dplyr slow)
    # rid is streamline edge id
    metric_summary <- my_merge[, .(ussum_metric = sum(rcavals)), .(rid)]

    # head(my_merge)
    # This object includes the total area coverage upstream
    # of each stream ID

    # -------------------------------------------------
    # print("Re-running summary for full RCA polygons...")

    # Also calculate area of RCA polygons as m2
    rca_polygons <- sf::st_transform(rca_polygons, utm_zone)
    rca_polygons$area_m2 <-
      as.numeric(sf::st_area(rca_polygons))
    rca_polygons <- rca_polygons[, c("rca_id", "area_m2")]

    area_rca <- sum(rca_polygons$area_m2, na.rm = TRUE)

    if (area_metric > area_rca) {
      warning("Metric area exceeds rca area...")
    }

    rca_polygons_df <- rca_polygons

    sf::st_geometry(rca_polygons_df) <- NULL

    # Redo above steps for full RCA polygons
    mydt <- rca_polygons_df
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)
    # Merge to the stream to rid index table
    # my_merge <- merge(mydt, net, by.x = "rca_id", by.y = "rca_id")
    my_merge <- mydt[net, nomatch=0]

    # Full RCA summary
    full_rca_summary <- my_merge[, .(ussum_full = sum(area_m2)), .(rid)]

    # This table includes the full drainage area upstream for
    # each rca polygon
    # head(full_rca_summary)

    # Calculate upstream percent coverage by merging and
    # dividing areas
    fsum <- merge(
      full_rca_summary,
      metric_summary,
      by.x = "rid",
      by.y = "rid",
      all.x = TRUE,
      all.y = FALSE
    )

    # Export and return
    ret_obj <- as.data.frame(fsum)

    # Update column names
    if(output_fieldname == "field_name") {
      use_output_fieldname <- "var_area_m2"
    } else {
      use_output_fieldname <- output_fieldname
    }

    colnames(ret_obj) <- c("rid", "total_us_area_m2", use_output_fieldname)

    return(ret_obj)

  }


  # ===================================================
  # Run simple area-based summary for percent coverage
  if (summary_type %in% c("percent_coverage", "linear_density", "weighted_percent_coverage")) {

    # Test if dat is a spatial layer or a data frame
    if(class(dat)[1] != "data.frame" | length(class(dat)) > 1) {
      # Look at area or length of target layer
      # print("Converting to utm projection...")
      dat <- sf::st_transform(dat, utm_zone)
    }

    # Make sure the rca_id column is present
    if (!("rca_id" %in% colnames(dat))) {
      print("rca_env_data must have an rca_id column. Run an intersection in GIS between your RCA polygons and the spatial layer of interest and then run summarize_upstream() afterwards with rca_env_data set to the product of your intersection.")
      stop("See note.")
    }


    # Polygon area coverage Calculate area as m2
    if (summary_type %in% c("percent_coverage", "weighted_percent_coverage")) {

      # Simple un-weighted coverage
      if(summary_type == "percent_coverage"){
        dat$area_m2 <- round(as.numeric(sf::st_area(dat)), 6)
      }

      # weight by an attribute
      if(summary_type == "weighted_percent_coverage"){
        dat$area_m2_pre <- round(as.numeric(sf::st_area(dat)), 6)
        dat$weights <- as.numeric(dat[[summary_field]])
        # weight area coverage
        dat$area_m2 <- dat$area_m2_pre * dat$weights
      }
    }


    # Line densities linear
    if (summary_type == "linear_density") {
      # note this variable is actually length
      # but stored here for convenience
      if(class(dat)[1] != "data.frame" | length(class(dat)) > 1) {
        # dat is a sf object. I know this should be labeled as length
        dat$area_m2 <- as.numeric(sf::st_length(dat))
      } else {
        # dat is a data.frame
        dat$area_m2 <- as.numeric(dat[[summary_field]])
      }
    }

    # Will be length for linear density
    area_metric <- sum(dat$area_m2, na.rm = TRUE)

    # Summarize area by RCA id
    dat_df <- dat

    if(class(dat_df)[1] != "data.frame" | length(class(dat_df)) > 1) {
      sf::st_geometry(dat_df) <- NULL
    }

    # Summarize area (or length) across for each RCA. Sometimes RCA polygons
    # will be split into sub units. We need to merge here
    dat_sum <-
      dat_df %>% group_by(rca_id) %>% summarise(rcavals = sum(area_m2, na.rm = TRUE))
    mydt <- dat_sum

    # Convert RCA data to simple data.table object for speed.
    # from data.frame to data.table
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)

    # Merge to the stream to rid index table
    # A giant summary table will be produced to group-by
    # (rca polygons us and their associated values)
    # my_merge <- merge(mydt, net, by.x = "rca_id", by.y = "rca_id")
    my_merge <- mydt[net, nomatch=0] # might be faster

    # Note the speed of data.table - takes 100X longer in base or dplyr

    # Calculate the total sum for each stream reach (total upstream area)
    # data.table group-by/summarize function (dplyr slow)
    # rid is streamline edge id
    metric_summary <- my_merge[, .(ussum_metric = sum(rcavals)), .(rid)]

    # head(metric_summary)
    # This object includes the total area coverage upstream
    # of each stream ID

    # -------------------------------------------------
    # print("Re-running summary for full RCA polygons...")

    # Also calculate area of RCA polygons as m2
    rca_polygons <- sf::st_transform(rca_polygons, utm_zone)
    rca_polygons$area_m2 <-
      as.numeric(sf::st_area(rca_polygons))
    rca_polygons <- rca_polygons[, c("rca_id", "area_m2")]

    area_rca <- sum(rca_polygons$area_m2, na.rm = TRUE)

    if (area_metric > area_rca) {
      warning("Metric area exceeds rca area...")
    }

    rca_polygons_df <- rca_polygons

    if(class(rca_polygons_df)[1] != "data.frame" | length(class(rca_polygons_df)) > 1) {
      sf::st_geometry(rca_polygons_df) <- NULL
    }

    # Redo above steps for full RCA polygons
    mydt <- rca_polygons_df
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)
    # Merge to the stream to rid index table
    # my_merge <- merge(mydt, net, key = "rca_id")
    my_merge <- mydt[net, nomatch=0] # faster

    # Full RCA summary
    full_rca_summary <- my_merge[, .(ussum_full = sum(area_m2)), .(rid)]

    # This table includes the full drainage area upstream for
    # each rca polygon
    # head(full_rca_summary)

    # Calculate upstream percent coverage by merging and
    # dividing areas
    fsum <- merge(
      full_rca_summary,
      metric_summary,
      by.x = "rid",
      by.y = "rid",
      all.x = TRUE,
      all.y = FALSE
    )

    if (summary_type == "linear_density") {
      print("Reporting linear density in units of km per km2...")
      # Convert from m/m2 to km/km2
      fsum$coverage <-
        (fsum$ussum_metric / 1000) / (fsum$ussum_full * 0.000001)

      fsum$coverage <- ifelse(fsum$ussum_metric == 0, 0, fsum$coverage)

    } else {
      # Area based m2/m2 = km2/km2
      fsum$coverage <- fsum$ussum_metric / fsum$ussum_full

      #head(fsum[fsum$coverage > 1, ]) # Should be impossible
      fsum$coverage <- ifelse(fsum$coverage > 1, 1, fsum$coverage)

    }

    fsum$coverage <- ifelse(is.na(fsum$coverage), 0, fsum$coverage)

    # Update variable name
    colnames(fsum)[colnames(fsum) == "coverage"] <- output_fieldname

    # Export and return
    ret_obj <- as.data.frame(fsum)
    ret_obj <- ret_obj[, c("rid", output_fieldname)]

    return(ret_obj)

  }




  # ===================================================
  # ===================================================

  # Run area-weighted mean summary
  if (summary_type == "area_weighted_mean") {

    # Covert to data frame
    print("Prepping data...")
    if (any(class(dat) == "sf")) {
      # Pre-summarize across RCAs
      dat$tmp_area <- as.numeric(sf::st_area(dat))

      if(class(dat)[1] != "data.frame" | length(class(dat)) > 1) {
        sf::st_geometry(dat) <- NULL
      }

      # summarize across RCAs (if duplicated)
      if(any(duplicated(dat$rca_id))) {
        dat_new <- dat %>% group_by(rca_id) %>% summarise(output = weighted.mean(.data[[summary_field]], w = tmp_area, na.rm = TRUE))
        colnames(dat_new)[2] <- summary_field
        dat <- dat_new
      }

    }



    # Filter to target fields
    dat <- dat[, c("rca_id", summary_field)]
    colnames(dat) <- c("rca_id", "sfield")
    dat$sfield <- as.numeric(as.character(dat$sfield))

    # Need to get area of each RCA ID polygon
    rca_polygons <- sf::st_transform(rca_polygons, utm_zone)
    rca_polygons$area_m2 <-
      round(as.numeric(sf::st_area(rca_polygons)), 4)
    rca_polygons <- rca_polygons[, c("rca_id", "area_m2")]
    rca_polygons_df <- rca_polygons
    sf::st_geometry(rca_polygons_df) <- NULL

    # Need to add on weights (rca polygon area)
    mydt <- merge(
      rca_polygons_df,
      dat,
      by.x = "rca_id",
      by.y = "rca_id",
      all.x = TRUE,
      all.y = FALSE
    )

    # Need to deal with duplicated per RCA

    # Convert RCA data to simple data.table object for speed.
    # from data.frame to data.table
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)

    # Next merge to the entire streamline index table
    # my_merge <- merge(mydt, net, key = "rca_id")
    my_merge <- mydt[net, nomatch=0]

    # For this metric we want to do a
    # weighted mean based on the RCA polygon area
    my_summary <-
      my_merge[, .(ussum = weighted.mean(sfield, area_m2, na.rm = TRUE)), .(rid)]


    mout <- as.data.frame(my_summary)

    # Update variable name
    colnames(mout)[colnames(mout) == "ussum"] <- output_fieldname

    # Export and return
    ret_obj <- as.data.frame(mout[, c("rid", output_fieldname)])

    return(ret_obj)

  }

  # ===================================================
  # ===================================================

  # Run min-max summary - will only for summaries
  if (summary_type %in% c("min_rca", "max_rca")) {

    # Covert to data frame
    print("Prepping data...")

    if (any(class(dat) == "sf")) {

      if(class(dat)[1] != "data.frame" | length(class(dat)) > 1) {
        sf::st_geometry(dat) <- NULL
      }

      # summarize across RCAs (if duplicated)
      if(any(duplicated(dat$rca_id))) {
        if(summary_type == "max_rca") {
          dat_new <- dat %>% group_by(rca_id) %>% summarise(output = max(.data[[summary_field]], na.rm = TRUE))
        }
        if(summary_type == "min_rca") {
          dat_new <- dat %>% group_by(rca_id) %>% summarise(output = min(.data[[summary_field]], na.rm = TRUE))
        }
        colnames(dat_new)[2] <- summary_field
        dat <- dat_new
      }

    }

    # Filter to target fields
    dat <- dat[, c("rca_id", summary_field)]
    colnames(dat) <- c("rca_id", "sfield")
    dat$sfield <- as.numeric(as.character(dat$sfield))
    mydt <- dat

    # Convert RCA data to simple data.table object for speed.
    # from data.frame to data.table
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)

    # Next merge to the entire streamline index table
    # my_merge <- merge(mydt, net, key = "rca_id")
    my_merge <- mydt[net, nomatch=0]

    # For this metric we want to do a
    # weighted mean based on the RCA polygon area
    if(summary_type == "max_rca") {
      suppressWarnings({
        my_summary <-
          my_merge[, .(ussum = max(sfield, na.rm = TRUE)), .(rid)]
      })
    }
    if(summary_type == "min_rca") {
      suppressWarnings({
        my_summary <-
          my_merge[, .(ussum = min(sfield, na.rm = TRUE)), .(rid)]
      })
    }

    mout <- as.data.frame(my_summary)

    # Update variable name
    colnames(mout)[colnames(mout) == "ussum"] <- output_fieldname

    # Export and return
    ret_obj <- as.data.frame(mout[, c("rid", output_fieldname)])

    return(ret_obj)

  }



  # ===================================================
  # ===================================================

  # Run feature count summary
  if (summary_type %in% c("feature_count", "weighted_feature_count")) {

    # Covert to data frame
    print("Prepping data...")
    if (any(class(dat) == "sf")) {
      sf::st_geometry(dat) <- NULL
    }

    if (!("rca_id" %in% colnames(dat))) {
      stop("column rca_id must be in environmental inputs")
    }

    dat$row_id <- seq(1, nrow(dat))


    # un-weighted count
    if(summary_type == "feature_count") {
      dat <- dat[, c("row_id", "rca_id")]

      # Summarize area by RCA id (count)
      dat_df <- dat
      dat_sum <-
        dat_df %>% group_by(rca_id) %>% summarise(rcavals = n())
      mydt <- dat_sum
    }

    # Weighted count
    if(summary_type == "weighted_feature_count") {

      # need to get summary field
      dat <- dat[, c("row_id", "rca_id", summary_field)]

      # Summarize area by RCA id (count)
      # weighted sum is just sum.. some features might have counts
      # in the 1000s or less than 1..
      dat_df <- dat
      dat_sum <-
        dat_df %>% group_by(rca_id) %>% summarise(rcavals = sum(.data[[summary_field]]))
      mydt <- dat_sum

    }

    # Convert RCA data to simple data.table object for speed.
    # from data.frame to data.table
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)

    # Merge to the stream to rid index table
    # A giant summary table will be produced to group-by
    # (rca polygons us and their associated values)
    # my_merge <- merge(mydt, net, key = "rca_id")
    my_merge <- mydt[net, nomatch=0]

    # Note the speed of data.table - takes 100X longer in base or dplyr

    # Calculate the total sum for each stream reach (total upstream area)
    # data.table group-by/summarize function (dplyr slow)
    # rid is streamline edge id
    metric_summary <- my_merge[, .(ussum_metric = sum(rcavals)), .(rid)]

    # head(metric_summary)
    # This object includes the total area coverage upstream
    # of each stream ID

    # -------------------------------------------------
    print("Re-running summary for full RCA polygons...")

    # Also calculate area of RCA polygons as m2
    rca_polygons <- sf::st_transform(rca_polygons, utm_zone)
    rca_polygons$area_m2 <-
      round(as.numeric(sf::st_area(rca_polygons)), 4)
    rca_polygons <- rca_polygons[, c("rca_id", "area_m2")]

    rca_polygons_df <- rca_polygons
    sf::st_geometry(rca_polygons_df) <- NULL

    # Redo above steps for full RCA polygons
    mydt <- rca_polygons_df
    data.table::setDT(mydt)
    class(mydt)
    data.table::setkey(mydt, rca_id)
    data.table::key(mydt)
    # Merge to the stream to rid index table
    # my_merge <- merge(mydt, net, key = "rca_id")
    my_merge <- mydt[net, nomatch=0]
    # head(my_merge)

    # Full RCA summary
    full_rca_summary <- my_merge[, .(ussum_full = sum(area_m2)), .(rid)]

    # This table includes the full drainage area upstream for
    # each rca polygon
    # head(full_rca_summary)

    # Calculate upstream percent coverage by merging and
    # dividing areas
    fsum <- merge(
      full_rca_summary,
      metric_summary,
      by.x = "rid",
      by.y = "rid",
      all.x = TRUE,
      all.y = FALSE
    )

    fsum$ussum_metric <- ifelse(is.na(fsum$ussum_metric), 0, fsum$ussum_metric)

    fsum$coverage <- fsum$ussum_metric / fsum$ussum_full

    fsum$coverage <- ifelse(is.na(fsum$coverage), 0, fsum$coverage)

    # Update variable name
    colnames(fsum)[colnames(fsum) == "coverage"] <- output_fieldname

    if(summary_type == "feature_count") {
      print("Count is in # meters squared")
    } else {
      print("Feature count is in sum(weight)/m2 meters squared")
    }

    # Export and return
    ret_obj <- as.data.frame(fsum)
    ret_obj <- ret_obj[, c("rid", output_fieldname)]

    return(ret_obj)

  }



}
