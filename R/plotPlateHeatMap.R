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
#' @return ggplot2 object plot
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
  loc.dt[, col.tmp.row := gsub('[0-9]+', '', get(in.well))]
  loc.dt[, col.tmp.col := as.numeric(gsub('[A-Z]{1}', '', get(in.well)))]

  # min & max of meas col
  if(is.null(in.meas.min))
    loc.min = min(in.dt[[in.meas]], na.rm = TRUE)
  else
    loc.min = in.meas.min

  if(is.null(in.meas.max))
    loc.max = max(in.dt[[in.meas]], na.rm = TRUE)
  else
    loc.max = in.meas.max

  if (loc.min > loc.max)
    stop(sprintf("Min/max bounds incorrect! min=%f max=%f", loc.min, loc.max))

  if ((loc.min < 0) & (loc.max > 0))
    loc.med = 0
  else if (loc.max < 0)
    loc.med = 0.5*(loc.min - loc.max)
  else
    loc.med = 0.5*(loc.min + loc.max)



  loc.p = ggplot(loc.dt,
                 aes(x = as.factor(col.tmp.col), y = col.tmp.row)) +
    geom_raster(aes_string(fill = in.meas))

  loc.p = loc.p +
    scale_fill_gradientn(
      name = in.title.scale,
      colours = c("blue", "white", "red"),
      limits = c(loc.min, loc.max),
      values = rescale(c(loc.min, loc.med, loc.max)),
      na.value = 'grey50'
    )



  loc.p = loc.p +
    scale_y_discrete(limits = rev(unique(loc.dt$col.tmp.row))) +
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
    loc.n.platerows = length(unique(loc.dt$col.tmp.row))

    loc.ctrl.neg = data.table(
      col.tmp.row = abs(match(gsub('{1}[0-9]*', '', in.wells.neg), LETTERS) - loc.n.platerows - 1),
      col.tmp.col = as.integer(gsub('[A-Z]*', '', in.wells.neg)),
      npi = 0
    )

    loc.p = loc.p +
      geom_rect(
        data = loc.ctrl.neg,
        size = 2,
        fill = NA,
        colour = "yellow",
        aes(
          xmin = col.tmp.col - 0.5,
          xmax = col.tmp.col + 0.5,
          ymin = col.tmp.row - 0.5,
          ymax = col.tmp.row + 0.5
        )
      )
  }

  if(!is.null(in.wells.pos)) {
    loc.n.platerows = length(unique(loc.dt$col.tmp.row))
    loc.ctrl.pos = data.table(
      col.tmp.row = abs(match(gsub('{1}[0-9]*', '', in.wells.pos), LETTERS) - loc.n.platerows - 1),
      col.tmp.col = as.integer(gsub('[A-Z]*', '', in.wells.pos)),
      npi = 0
    )

    loc.p = loc.p +
      geom_rect(
        data = loc.ctrl.pos,
        size = 2,
        fill = NA,
        colour = "green",
        aes(
          xmin = col.tmp.col - 0.5,
          xmax = col.tmp.col + 0.5,
          ymin = col.tmp.row - 0.5,
          ymax = col.tmp.row + 0.5
        )
      )
  }

  if(!is.null(in.wells.untr)) {
    loc.n.platerows = length(unique(loc.dt$col.tmp.row))
    loc.ctrl.untr = data.table(
      col.tmp.row = abs(match(gsub('{1}[0-9]*', '', in.wells.untr), LETTERS) - loc.n.platerows - 1),
      col.tmp.col = as.integer(gsub('[A-Z]*', '', in.wells.untr)),
      npi = 0
    )

    loc.p = loc.p +
      geom_rect(
        data = loc.ctrl.untr,
        size = 2,
        fill = NA,
        colour = "grey50",
        aes(
          xmin = col.tmp.col - 0.5,
          xmax = col.tmp.col + 0.5,
          ymin = col.tmp.row - 0.5,
          ymax = col.tmp.row + 0.5
        )
      )
  }

  return(loc.p)

}
