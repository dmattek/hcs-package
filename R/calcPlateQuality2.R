#' Calculate plate quality
#'
#' Calculates z-factor and z-prime of a measurment. Requires the actual measurment,
#' negative and positive controls.
#'
#' @param in.dt Input data table. Rows correspond to well measurements.
#' Has to consist of at least 2 columns: value of the measurement in a well, and well names.
#'
#' @param in.col.meas Name of the column holding the measurement
#' @param in.col.well Name of the column holding well name
#' @param in.wells.neg String vector with well names of the negative control
#' @param in.wells.pos String vector with well names of the positive control
#' @param in.wells.untr String vector with well names of untreated wells; default NULL
#' @param in.screen String with either of \code{c('activation', 'inhibition')}
#'
#' @return List with two items, \code{zfactor} and \code{zprime}
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#'
#' v.wells = as.vector(outer(LETTERS[1:8],1:12,paste0))
#' dt = data.table(meanInt = c(rpois(76, 20), rnorm(10, 1), rnorm(10, 22)), well = v.wells)
#' calcPlateQuality2(dt, 'meanInt', 'well', v.wells[77:86], v.wells[87:96], 'act')
#'

calcPlateQuality2 = function(in.dt,
                             in.col.meas,
                             in.col.well,
                             in.wells.neg, in.wells.pos, in.wells.untr = NULL,
                             in.screen = c('activation', 'inhibition')) {

  in.screen = match.arg(in.screen)

  # create a list of wells to exclude from calculation of which mean and sd
  # can consist of neg and pos ctrl,
  # and untreated if in.wells.untr not NULL
  if (is.null(in.wells.untr))
    loc.wells.excl = c(in.wells.neg, in.wells.pos)
  else
    loc.wells.excl = c(in.wells.neg, in.wells.pos, in.wells.untr)

  loc.l = list()

  loc.l$sample.mn = mean(in.dt[!(get(in.col.well) %in% loc.wells.excl), get(in.col.meas)])
  loc.l$sample.sd = sd(in.dt[!(get(in.col.well) %in% loc.wells.excl), get(in.col.meas)])

  loc.l$ctrl.pos.mn = mean(in.dt[get(in.col.well) %in% in.wells.pos, get(in.col.meas)])
  loc.l$ctrl.pos.sd = sd(in.dt[get(in.col.well) %in% in.wells.pos, get(in.col.meas)])

  loc.l$ctrl.neg.mn = mean(in.dt[get(in.col.well) %in% in.wells.neg, get(in.col.meas)])
  loc.l$ctrl.neg.sd = sd(in.dt[get(in.col.well) %in% in.wells.neg, get(in.col.meas)])

  switch(in.screen,
         activation = {
           loc.l$zfactor = 1 - (3 * loc.l$sample.sd + 3 * loc.l$ctrl.neg.sd) / abs(loc.l$sample.mn - loc.l$ctrl.neg.mn)
         },
         inhibition = {
           loc.l$zfactor = 1 - (3 * loc.l$sample.sd + 3 * loc.l$ctrl.pos.sd) / abs(loc.l$sample.mn - loc.l$ctrl.pos.mn)
         })

  loc.l$zprime  = 1 - (3 * loc.l$ctrl.pos.sd + 3 * loc.l$ctrl.neg.sd) / abs(loc.l$ctrl.pos.mn - loc.l$ctrl.neg.mn)

  return(loc.l)
}

