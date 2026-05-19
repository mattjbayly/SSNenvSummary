#' Build Longitudinal Profile Data
#'
#' Extract elevation and upstream distance for every vertex of every reach
#' upstream of a given outlet, producing a data.frame ready for profile plotting.
#'
#' @param strm An sf object of streamlines (MULTILINESTRING with Z coordinates).
#'   Must contain columns: `rid` (or `LINEAR_FEATURE_ID`), `STREAM_ORDER`,
#'   `GNIS_NAME`, `EDGE_TYPE`, and `GEOMETRY_Length`.
#' @param us_index A data.frame with columns `rid` and `usrid` representing the
#'   upstream network index.
#' @param outlet_rid Integer. The rid of the outlet reach to profile upstream from.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{dist}{Distance from outlet in meters.}
#'     \item{elev}{Elevation in meters (from Z coordinate).}
#'     \item{rid}{Reach ID.}
#'     \item{STREAM_ORDER}{Stream order (integer).}
#'     \item{GNIS_NAME}{Stream name (character, may be NA).}
#'   }
#'
#' @export
build_profile_data <- function(strm, us_index, outlet_rid) {

  # Accept either rid or LINEAR_FEATURE_ID
  if (!"rid" %in% names(strm) && "LINEAR_FEATURE_ID" %in% names(strm)) {
    strm$rid <- strm$LINEAR_FEATURE_ID
  }

  # Get all upstream reach IDs from outlet
  us_rids <- us_index$usrid[us_index$rid == outlet_rid]
  if (length(us_rids) == 0) {
    stop("No upstream reaches found for outlet_rid = ", outlet_rid)
  }

  # Subset streamlines and index to upstream network

  strm_sub <- strm[strm$rid %in% us_rids, ]
  us_sub <- us_index[us_index$rid %in% us_rids & us_index$usrid %in% us_rids, ]

  # Process each reach
  rids <- unique(strm_sub$rid)
  prof_list <- vector("list", length(rids))

  for (i in seq_along(rids)) {
    this_rid <- rids[i]
    target_line <- strm_sub[strm_sub$rid == this_rid, ]
    if (nrow(target_line) != 1) next

    # Extract Z coordinates
    coords <- sf::st_coordinates(target_line)
    elevations <- coords[, 3]
    if (length(elevations) == 0) next

    # Flatten lakes (EDGE_TYPE 1200)
    if (target_line$EDGE_TYPE == 1200) {
      x <- c(1, length(elevations))
      y <- c(elevations[1], elevations[length(elevations)])
      elevations <- sort(stats::approx(x, y, xout = seq_along(elevations))$y)
    }

    # Validate flow direction (first vertex should be lower than last)
    if (elevations[1] > elevations[length(elevations)] && length(elevations) >= 5) {
      warning("Reach ", this_rid, " appears to have reversed flow direction. Skipping.")
      next
    }

    # Calculate cumulative along-line distance (replacing rgeos::gProject)
    dx <- diff(coords[, "X"])
    dy <- diff(coords[, "Y"])
    seg_lengths <- sqrt(dx^2 + dy^2)
    distances <- c(0, cumsum(seg_lengths))

    # Calculate downstream offset: sum of GEOMETRY_Length for all reaches
    # downstream of this_rid (i.e., reaches that have this_rid as upstream)
    ds_reaches <- us_sub$rid[us_sub$usrid == this_rid]
    ds_only <- strm_sub[strm_sub$rid %in% ds_reaches & strm_sub$rid != this_rid, ]
    ds_length <- if (nrow(ds_only) > 0) sum(ds_only$GEOMETRY_Length) else 0

    distances <- distances + ds_length

    prof_df <- data.frame(
      dist = distances,
      elev = elevations,
      rid = this_rid,
      STREAM_ORDER = target_line$STREAM_ORDER,
      GNIS_NAME = target_line$GNIS_NAME,
      stringsAsFactors = FALSE
    )
    prof_df <- prof_df[order(prof_df$dist), ]
    prof_list[[i]] <- prof_df
  }

  result <- do.call(rbind, prof_list)
  if (is.null(result) || nrow(result) == 0) {
    stop("No profile data could be generated.")
  }
  rownames(result) <- NULL
  result
}
