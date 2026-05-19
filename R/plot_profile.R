#' Plot Longitudinal Stream Profile
#'
#' Generate a ggplot2 longitudinal profile from the output of
#' [build_profile_data()].
#'
#' @param profile_data A data.frame from [build_profile_data()].
#' @param theme Character. One of `"dark"`, `"light"`, or `"bw"`.
#' @param show_names Logical. Whether to label streams with `GNIS_NAME`
#'   (requires the `geomtextpath` package).
#' @param min_label_order Integer. Minimum `STREAM_ORDER` for a stream to
#'   receive a name label. Default 2.
#'
#' @return A ggplot object.
#'
#' @export
plot_profile <- function(profile_data,
                         theme = c("dark", "light", "bw"),
                         show_names = TRUE,
                         min_label_order = 2) {

  theme <- match.arg(theme)

  pdb <- profile_data
  pdb$dist_km <- pdb$dist / 1000
  pdb$rid_chr <- as.character(pdb$rid)


  # Assign color categories by stream order
  pdb$color_cat <- "so7"
  pdb$color_cat[pdb$STREAM_ORDER == 1] <- "so1"
  pdb$color_cat[pdb$STREAM_ORDER == 2] <- "so2"
  pdb$color_cat[pdb$STREAM_ORDER >= 3 & pdb$STREAM_ORDER <= 4] <- "so34"
  pdb$color_cat[pdb$STREAM_ORDER >= 5 & pdb$STREAM_ORDER <= 6] <- "so56"

  # Order so higher stream orders plot on top
  pdb <- pdb[order(pdb$STREAM_ORDER), ]

  # Color palettes
  palettes <- list(
    dark = c(
      so1 = "#f7fbff10", so2 = "#deebf725",
      so34 = "#c6dbef50", so56 = "#9ecae190", so7 = "#6baed699"
    ),
    light = c(
      so1 = "#bdbdbd40", so2 = "#96969660",
      so34 = "#63636380", so56 = "#3b3b3b99", so7 = "#1a1a1aCC"
    ),
    bw = c(
      so1 = "#000000", so2 = "#000000",
      so34 = "#000000", so56 = "#000000", so7 = "#000000"
    )
  )

  g <- ggplot2::ggplot(pdb, ggplot2::aes(
    x = .data$dist_km, y = .data$elev, group = .data$rid_chr
  )) +
    ggplot2::geom_line(ggplot2::aes(color = .data$color_cat)) +
    ggplot2::scale_color_manual(values = palettes[[theme]]) +
    ggplot2::xlab("Upstream Distance (km)") +
    ggplot2::ylab("Elevation (m)") +
    ggplot2::theme(legend.position = "none")

  # Apply theme styling
  if (theme == "dark") {
    g <- g + ggplot2::theme(
      axis.title.x = ggplot2::element_text(colour = "#999999"),
      axis.title.y = ggplot2::element_text(colour = "#999999"),
      panel.background = ggplot2::element_rect(fill = NA, colour = "#3b3b3b"),
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.5, linetype = "dotted", colour = "#3b3b3b"
      ),
      panel.grid.minor = ggplot2::element_line(
        linewidth = 0.5, linetype = "dotted", colour = "#3b3b3b"
      ),
      plot.background = ggplot2::element_rect(fill = "black"),
      axis.text = ggplot2::element_text(colour = "#999999")
    )
  } else if (theme == "light") {
    g <- g + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "#999999"),
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.5, linetype = "dotted", colour = "#cccccc"
      ),
      panel.grid.minor = ggplot2::element_line(
        linewidth = 0.5, linetype = "dotted", colour = "#cccccc"
      ),
      plot.background = ggplot2::element_rect(fill = "white")
    )
  } else {
    g <- g + ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  # Stream name labels

  if (show_names) {
    if (requireNamespace("geomtextpath", quietly = TRUE)) {
      pdb_lab <- pdb[!is.na(pdb$GNIS_NAME) & pdb$STREAM_ORDER >= min_label_order, ]
      if (nrow(pdb_lab) > 0) {
        g <- g + geomtextpath::geom_textline(
          data = pdb_lab,
          ggplot2::aes(
            x = .data$dist_km, y = .data$elev,
            group = .data$GNIS_NAME, label = .data$GNIS_NAME
          ),
          size = 2.5, straight = FALSE, linecolor = NA,
          upright = TRUE, text_smoothing = 40
        )
      }
    } else {
      message("Install 'geomtextpath' for stream name labels.")
    }
  }

  g
}
