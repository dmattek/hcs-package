#' Calculate plate statistics
#'
#' Calculates NPI (normalized percentage inhibition) and z-score for a plate.
#'
#' @param in.dt Input data table with aggregate, per well values. Should contain:
#' numeric column with measurmeents, string column with well names (e.g. 'A01', 'A02'),
#' string vector with names of negative control wells, string vector with names of positive control wells.
#' @param in.col.well Names of the column with well names (e.g. 'A01', 'A02').
#' @param in.col.meas Numeric column with measurement.
#' @param in.wells.neg Names of wells with negative controls (e.g. 'A01', 'A02').
#' @param in.wells.pos Names of wells with positive controls (e.g. 'A01', 'A02').
#'
#' @return Data table with same columns as original but with columns \code{npi} and \code{zscore} added.
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#' dt = data.table(well = c('A01', 'A02', 'A03', 'B01', 'B02', 'B03', 'C01', 'C02', 'C03'),
#'                 x = rnorm(9))
#' calcPlateStats(dt, 'x', 'well', 'B01', 'C03')

calcPlateStats = function(in.dt, in.col.meas, in.col.well, in.wells.neg, in.wells.pos) {

  loc.dt = copy(in.dt)

  # calculate NPI (Normalized Percentage Inhibition)
  loc.neg.mn = mean(loc.dt[get(in.col.well) %in% in.wells.neg, get(in.col.meas)])
  loc.pos.mn = mean(loc.dt[get(in.col.well) %in% in.wells.pos, get(in.col.meas)])

  loc.dt[, npi := (loc.pos.mn - get(in.col.meas)) / (loc.pos.mn - loc.neg.mn)]

  # calculate Z-score
  loc.plate.mn = mean(loc.dt[!(get(in.col.well) %in% c(in.wells.neg, in.wells.pos)), get(in.col.meas)])
  loc.plate.sd =   sd(loc.dt[!(get(in.col.well) %in% c(in.wells.neg, in.wells.pos)), get(in.col.meas)])

  loc.dt[, zscore := (get(in.col.meas) - loc.plate.mn) / loc.plate.sd]

  return(loc.dt)
}
