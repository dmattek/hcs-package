#' Plot heat-map of the plate
#'
#' Plots a heatmap of a chosen measured quantity. Colour scaling assigns blue gradient for negative numbers, white for zero, red gradient for positive values.
#'
#' @param in.dt Input data table. Should contain at least two columns: well names (e.g. 'A01', 'C03'), measurment values.
#' @param in.meas String of the column name with the measurement to plot.
#' @param in.well String of the column name with well names.
#' @param in.meas.min Minimum value for colour bar scale (default 'NULL'). If `NULL`, minimum is calculated from data.
#' @param in.meas.max Maximum value for colour bar scale (default 'NULL'). If `NULL`, maximum is calculated from data.
#' @param in.title.scale String with colour bar title (default '').
#' @param in.wells.neg String vector with names of wells with negative controls (default 'NULL'). If provided, yellow borders are drawn around those wells.
#' @param in.wells.pos String vector with names of wells with positive controls (default 'NULL'). If provided, green borders are drawn around those wells.
#' @param in.wells.untr String vector with names of untreated wells (default 'NULL'). If provided, grey borders are drawn around those wells.
#'
#' @return ggplot2 plot
#' @export
#' @import data.table
#' @import ggplot2
#' @import scales
#'
#' @examples
#' require(data.table)
#' dt = data.table(well = c('A01', 'A02', 'A03', 'B01', 'B02', 'B03', 'C01', 'C02', 'C03'),
#'                 npi = rnorm(9))
#' plotPlateHeatMap(dt, 'npi', 'well', -1.2, +1.2, 'NPI', 'B01', 'C03', 'A02')

plotPlateHeatMap = function(in.dt,
                            in.meas,
                            in.well,
                            in.meas.min = NULL,
                            in.meas.max = NULL,
                            in.title.scale = '',
                            in.wells.neg = NULL,
                            in.wells.pos = NULL,
                            in.wells.untr = NULL) {

  loc.dt = copy(in.dt)

  # extract column and row names from well names
  loc.dt[, Image_Metadata_Row := gsub('[0-9]+', '', get(in.well))]
  loc.dt[, Image_Metadata_Col := as.numeric(gsub('[A-Z]{1}', '', get(in.well)))]

  # min & max of meas col
  if(is.null(in.meas.min))
    loc.min = min(in.dt[[in.meas]], na.rm = TRUE)
  else
    loc.min = in.meas.min

  if(is.null(in.meas.max))
    loc.max = max(in.dt[[in.meas]], na.rm = TRUE)
  else
    loc.max = in.meas.max

  loc.p = ggplot(loc.dt,
                 aes(x = as.factor(Image_Metadata_Col), y = Image_Metadata_Row)) +
    geom_raster(aes_string(fill = in.meas))  +
    scale_fill_gradientn(
      name = in.title.scale,
      colours = c("blue", "white", "red"),
      limits = c(loc.min, loc.max),
      values = rescale(c(loc.min, 0, loc.max)),
      na.value = 'grey50'
    ) +
    scale_y_discrete(limits = rev(unique(loc.dt$Image_Metadata_Row))) +
    xlab("Plate column") +
    ylab("Plate row\n") +
    theme_grey(base_size = 18, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(colour = 'black', fill = 'grey50'),
      plot.background = element_rect(colour = 'black', fill = 'grey90'),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = 14),
      strip.text.x = element_text(size = 12, face = "bold"),
      strip.text.y = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.background = element_rect(fill = 'grey90'),
      legend.key = element_blank(),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines"),
      legend.position = "top"
    )


  if(!is.null(in.wells.neg)) {
    loc.n.platerows = length(unique(loc.dt$Image_Metadata_Row))
    loc.ctrl.neg = data.table(
      Image_Metadata_Row = abs(seq(1, length(LETTERS), 1)[LETTERS %in% gsub('{1}[0-9]*', '', in.wells.neg)] - loc.n.platerows - 1),
      Image_Metadata_Col = as.integer(gsub('[A-Z]*', '', in.wells.neg)),
      npi = 0
    )

    loc.p = loc.p +
      geom_rect(
        data = loc.ctrl.neg,
        size = 2,
        fill = NA,
        colour = "yellow",
        aes(
          xmin = Image_Metadata_Col - 0.5,
          xmax = Image_Metadata_Col + 0.5,
          ymin = Image_Metadata_Row - 0.5,
          ymax = Image_Metadata_Row + 0.5
        )
      )
  }

  if(!is.null(in.wells.pos)) {
    loc.n.platerows = length(unique(loc.dt$Image_Metadata_Row))
    loc.ctrl.pos = data.table(
      Image_Metadata_Row = abs(seq(1, length(LETTERS), 1)[LETTERS %in% gsub('{1}[0-9]*', '', in.wells.pos)] - loc.n.platerows - 1),
      Image_Metadata_Col = as.integer(gsub('[A-Z]*', '', in.wells.pos)),
      npi = 0
    )

    loc.p = loc.p +
      geom_rect(
        data = loc.ctrl.pos,
        size = 2,
        fill = NA,
        colour = "green",
        aes(
          xmin = Image_Metadata_Col - 0.5,
          xmax = Image_Metadata_Col + 0.5,
          ymin = Image_Metadata_Row - 0.5,
          ymax = Image_Metadata_Row + 0.5
        )
      )
  }

  if(!is.null(in.wells.untr)) {
    loc.n.platerows = length(unique(loc.dt$Image_Metadata_Row))
    loc.ctrl.untr = data.table(
      Image_Metadata_Row = abs(seq(1, length(LETTERS), 1)[LETTERS %in% gsub('{1}[0-9]*', '', in.wells.untr)] - loc.n.platerows - 1),
      Image_Metadata_Col = as.integer(gsub('[A-Z]*', '', in.wells.untr)),
      npi = 0
    )

    loc.p = loc.p +
      geom_rect(
        data = loc.ctrl.untr,
        size = 2,
        fill = NA,
        colour = "grey50",
        aes(
          xmin = Image_Metadata_Col - 0.5,
          xmax = Image_Metadata_Col + 0.5,
          ymin = Image_Metadata_Row - 0.5,
          ymax = Image_Metadata_Row + 0.5
        )
      )
  }

  return(loc.p)

}
