library(SSNbler)
library(SSN2)
library(sf)
library(purrr)
library(dplyr)
library(sf)
library(lwgeom)
library(ggplot2)

# Remove all objects
rm(list = ls())

devtools::load_all()
devtools::document()

target_groups <- c("LFRA", "CHWK", "HARR")

target_species <- c("SSU", "NDC")

# Stream IDs
nd_ss_strms <- read.csv("../testing/NDSS/nd_ss_rids_streams.csv")


for(g in 1:length(target_groups)) {

  # this group # g = 1
  group <- target_groups[g]

  # Load representative data
  line_network_us_index <- read.csv(
    file = paste0("../1_bcfwa_attributes/01_line_network_us_index/", group, "_us_rid.csv"),
    stringsAsFactors = FALSE
  )

  streamline_geometry <- st_read(
    dsn = paste0("../1_bcfwa_attributes/01b_streamline_geometry/", group, "_strm.gpkg")
  )

  rca_polygons <- st_read(
    dsn = paste0("../1_bcfwa_attributes/02_rca_polygons/", group, "_rca.gpkg")
  )

  nodes <- st_read(
    dsn = paste0("../1_bcfwa_attributes/01c_node_geometry/", group, "_node.gpkg")
  )

  poly_stream_network_us_index <- read.csv(
    file = paste0("../1_bcfwa_attributes/04_poly_stream_network_us_index/", group, "_rid_us_rca.csv"),
    stringsAsFactors = FALSE
  )


  rca_to_stream_rid <- read.csv(
    file = paste0("../1_bcfwa_attributes/03_rca_to_stream_rid/", group, "_rca_rid.csv"),
    stringsAsFactors = FALSE
  )


  siss <- read.csv(
    file = paste0("../2_spatial_process/siss_snap/", group, ".csv"),
    stringsAsFactors = FALSE
  )




  # FISS Obs
  fiss_obs <- read.csv(
    file = paste0("../2_spatial_process/fiss_obs/", group, ".csv"),
    stringsAsFactors = FALSE
  )

  # Select only NS/DD
  fiss_obs_sub <- fiss_obs[fiss_obs$SPECIES_CD %in% target_species, ]
  target_features <- unique(fiss_obs_sub$lfid)

  # Get downstream segments

  ds_lines <- line_network_us_index$rid[line_network_us_index$usrid %in% target_features]
  us_lines <- line_network_us_index$usrid[line_network_us_index$rid %in% target_features]

  # subset and save
  sg_sub_ds <- streamline_geometry[streamline_geometry$rid %in% unique(ds_lines), ]
  sg_sub_us <- streamline_geometry[streamline_geometry$rid %in% unique(us_lines), ]

  st_write(
    sg_sub_ds,
    dsn = paste0("../testing/NDSS/us_ds/ds_", group, "_strm.gpkg"),
    driver = "GPKG",
    delete_dsn = TRUE
  )
  st_write(
    sg_sub_us,
    dsn = paste0("../testing/NDSS/us_ds/us_", group, "_strm.gpkg"),
    driver = "GPKG",
    delete_dsn = TRUE
  )







  # Gather all stream segments from target
  current_set <- nd_ss_strms$rid[nd_ss_strms$rid %in% streamline_geometry$rid]

  for(i in 1:length(current_set)) {

    # i = 1
    this_set <- current_set[i]
    us_segments <- line_network_us_index$usrid[line_network_us_index$rid == this_set]
    us_segments <- unique(us_segments)

    sub_line_network_us_index <- line_network_us_index[line_network_us_index$rid %in% us_segments, ]
    nrow(sub_line_network_us_index)


    sub_streamline_geometry <- streamline_geometry[streamline_geometry$rid %in% us_segments, ]
    nrow(sub_streamline_geometry)


    rca_ids <- poly_stream_network_us_index$rca_id[poly_stream_network_us_index$rid %in% us_segments]
    rca_ids <- unique(rca_ids)


    sub_rca_to_stream_rid <- rca_to_stream_rid[rca_to_stream_rid$rca_id %in% rca_ids, ]


    sub_rca_polygons <- rca_polygons[rca_polygons$rca_id %in% rca_ids, ]
    nrow(sub_rca_polygons)

    # plot(st_geometry(sub_streamline_geometry))
    # plot(st_geometry(sub_rca_polygons), add = TRUE, col = "lightgrey", border = "green")
    # plot(st_geometry(sub_streamline_geometry), add = TRUE, col = "blue")

    sub_poly_stream_network_us_index <- poly_stream_network_us_index[poly_stream_network_us_index$rca_id %in% rca_ids, ]
    nrow(sub_poly_stream_network_us_index)
    nrow(sub_line_network_us_index)


    #============================================
    # save outputs
    #============================================

    nname2 <- gsub(" ", "_", nname)
    fout <- paste0("../testing/NDSS/ssn/sub1/", nname2)

    dir.create(fout)

    st_write(sub_streamline_geometry,
             dsn = paste0(fout, "/strm.gpkg"),
             layer = "streamline_geometry",
             delete_dsn = TRUE)

    st_write(sub_rca_polygons,
             dsn =paste0(fout, "/rca.gpkg"),
             layer = "rca_polygons",
             delete_dsn = TRUE)

    write.csv(sub_poly_stream_network_us_index,
              file = paste0(fout, "/poly_stream_network_us_index.csv"),
              row.names = FALSE)

    write.csv(sub_rca_to_stream_rid,
              file = paste0(fout, "/rca_to_stream_rid.csv"),
              row.names = FALSE)

    write.csv(sub_line_network_us_index,
              file = paste0(fout, "/line_network_us_index.csv"),
              row.names = FALSE)



    #============================================
    # Now try to build SSN object
    nname <- nd_ss_strms$name[nd_ss_strms$rid == this_set]
    lsn.path <- paste0("../testing/NDSS/ssn/sub/", nname)
    streams <- st_zm(sub_streamline_geometry)
    streams <- st_cast(streams, "LINESTRING")
    nrow(streams)

    print("================================")
    print("================================")
    print("================================")
    print(group)
    print(this_set)
    print(nname)

    # Reverse direction of each line segment
    streams <- st_reverse(streams)

    streams$geom[1,]
    sub_streamline_geometry$geom[1,]

    # plot(st_geometry(streams))

    edges <- lines_to_lsn(
      streams = streams,
      lsn_path = lsn.path,
      check_topology = TRUE,
      snap_tolerance = 0.05,
      topo_tolerance = 20,
      overwrite = TRUE
    )

    # sub_siss
    sub_siss <- siss[siss$lfid %in% us_segments, ]
    # convert dataframe of siss to sf object
    sub_siss_sf <- st_as_sf(sub_siss, coords = c("new_x", "new_y"), crs = 3005)

    if(nrow(sub_siss_sf) > 0) {

      obs <- sites_to_lsn(
        sites = sub_siss_sf,
        edges = edges,
        lsn_path = lsn.path,
        file_name = "obs",
        snap_tolerance = 1,
        save_local = TRUE,
        overwrite = TRUE
      )

    }




    pred_sub <- nodes[nodes$rid %in% us_segments, ]

    preds <- sites_to_lsn(
      sites = pred_sub,
      edges = edges,
      save_local = TRUE,
      lsn_path = lsn.path,
      file_name = "pred.gpkg",
      snap_tolerance = 1,
      overwrite = TRUE
    )

    edges <- updist_edges(
      edges = edges,
      save_local = TRUE,
      lsn_path = lsn.path,
      calc_length = TRUE
    )


    if(nrow(sub_siss_sf) > 0) {
      site.list <- updist_sites(
        sites = list(
          obs = obs,
          preds = preds
        ),
        edges = edges,
        length_col = "Length",
        save_local = TRUE,
        lsn_path = lsn.path
      )

      ggplot() +
        geom_sf(data = edges, aes(color = upDist)) +
        geom_sf(data = site.list$obs, aes(color = upDist)) +
        coord_sf(datum = st_crs(edges)) +
        scale_color_viridis_c()

    } else {

      site.list <- updist_sites(
        sites = list(
          preds = preds
        ),
        edges = edges,
        length_col = "Length",
        save_local = TRUE,
        lsn_path = lsn.path
      )

      ggplot() +
        geom_sf(data = edges, aes(color = upDist)) +
        #geom_sf(data = site.list$obs, aes(color = upDist)) +
        coord_sf(datum = st_crs(edges)) +
        scale_color_viridis_c()

    }

      getUTMzone(sub_rca_polygons[1,])

      # Need to summarize the total upstream area
      us_area <- summarize_upstream(net = sub_poly_stream_network_us_index,
                          summary_type = "total_upstream_area",
                          summary_field = NA,
                          rca_polygons = sub_rca_polygons,
                          rca_env_data = sub_rca_polygons,
                          utm_zone = 26910,
                          output_fieldname = "h2oAreaKm2")

      us_area <- us_area[, c("rid", "us_area_m2")]

      edges <- merge(edges, us_area, by.x = "LINEAR_FEATURE_ID", by.y = "rid", all.x = TRUE)

      edges$h2oAreaKm2 <- edges$us_area_m2 / (1000*1000)

      plot(edges["h2oAreaKm2"])

      edges <- afv_edges(
        edges = edges,
        infl_col = "h2oAreaKm2",
        segpi_col = "areaPI",
        afv_col = "afvArea",
        lsn_path = lsn.path
      )

      site.list <- afv_sites(
        sites = site.list,
        edges = edges,
        afv_col = "afvArea",
        save_local = TRUE,
        lsn_path = lsn.path
      )


      ssn.path <- paste0("../testing/NDSS/ssn/sub2/", nname, ".ssn")

      site.list[c("preds")]$preds$ratio <- ifelse(site.list[c("preds")]$preds$ratio < 0, 0, site.list[c("preds")]$preds$ratio)

      mf04_ssn <- ssn_assemble(
        edges = edges,
        lsn_path = lsn.path,
        obs_sites = site.list$obs,
        preds_list = site.list[c("preds")],
        ssn_path = ssn.path,
        import = TRUE,
        check = TRUE,
        afv_col = "afvArea",
        overwrite = TRUE)



    #Sys.sleep(3)




  }


}

# Gather upstream values




