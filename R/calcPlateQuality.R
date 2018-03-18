#' Calculate plate quality
#'
#' Calculates z-factor and z-prime of a measurment. Requires the actual measurment,
#' negative and positive controls.
#'
#' @param in.dt Input data table. Rows correspond to well measurements.
#' Has to consist of at least 2 columns: value of the measurement in a well,
#' description of well types (e.g.\code{'CTRL neg'}, \code{'CTRL pos'}, \code{'Compounds'}.)
#'
#' @param in.col.meas String with column name of the measurement
#' @param in.col.type String with column name of the well type
#' @param in.well.type Named list with strings that describe wells
#' with negaitve and positive controls, and compounds. The list has to have the following
#' names of elements: \code{list(ctrl.neg = 'CTRL neg', ctrl.pos = 'CTRL pos', compounds = 'Compounds')}
#' @param in.screen String with either of \code{c('activation', 'inhibition')}
#'
#' @return List with two items, \code{zfactor} and \code{zprime}
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#' l.well = list(compounds = 'Compounds', ctrl.neg = 'CTRL neg', ctrl.pos = 'CTRL pos')
#' dt = data.table(meanInt = c(rnorm(80, 2, 1),
#'                             rnorm(10, 0, 1),
#'                             rnorm(10, 2, 1)),
#'                 wellType = c(rep(l.well$compounds, 80),
#'                              rep(l.well$ctrl.neg, 10),
#'                              rep(l.well$ctrl.pos, 10)))
#' calcPlateQuality(dt, 'meanInt', 'wellType', l.well, 'act')
#'
calcPlateQuality = function(in.dt, in.col.meas,
                            in.col.type,
                            in.well.type,
                            in.screen = c('activation', 'inhibition')) {

  in.screen = match.arg(in.screen)

  sample.mn = mean(in.dt[get(in.col.type) == in.well.type$compounds, get(in.col.meas)])
  sample.sd = sd(in.dt[get(in.col.type) == in.well.type$compounds, get(in.col.meas)])

  ctrl.pos.mn = mean(in.dt[get(in.col.type) == in.well.type$ctrl.pos, get(in.col.meas)])
  ctrl.pos.sd = sd(in.dt[get(in.col.type) == in.well.type$ctrl.pos, get(in.col.meas)])

  ctrl.neg.mn = mean(in.dt[get(in.col.type) == in.well.type$ctrl.neg, get(in.col.meas)])
  ctrl.neg.sd = sd(in.dt[get(in.col.type) == in.well.type$ctrl.neg, get(in.col.meas)])

  loc.l = list()

  switch(in.screen,
         activation = {
           loc.l$zfactor = 1 - (3*sample.sd + 3 * ctrl.neg.sd) / abs(sample.mn - ctrl.neg.mn)
         },
         inhibition = {
           loc.l$zfactor = 1 - (3*sample.sd + 3 * ctrl.pos.sd) / abs(sample.mn - ctrl.pos.mn)
         })

  loc.l$zprime  = 1 - (3 * ctrl.pos.sd + 3 * ctrl.neg.sd) / abs(ctrl.pos.mn - ctrl.neg.mn)

  return(loc.l)
}

