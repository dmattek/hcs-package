#' Aggregate statistics
#'
#' Calculates the number of records, mean, standard deviation, confidence interval for the mean from t-test (optionally)
#' from a chosen column of a data table.
#'
#' @param in.dt Input data table. Should contain at least one numeric column.
#' @param in.col String with the column name in the data table from which the stats should be calculated.
#' @param in.bycol String with the column name for grouping (optional).
#' @param in.cilevel Number between 0 an 1 with confidence level for t-test (optional), If not provided CIs won't be calculated.
#'
#' @return Data Table with aggregaed statistics.
#' @export
#' @import data.table
#'
#' @examples
#' dt = data.table(x = c(rnorm(1000, 0, 1), rnorm(800, 1, 2)), gr = c(rep('a', 1000), rep('b', 800)))
#'
#' # aggregation of the entire table without CI
#' dt.aggr.all = aggrStats(dt, 'x')
#'
#' # aggregation within groups with CI
#' dt.aggr.gr = aggrStats(dt, 'x', 'gr', 0.95)
aggrStats = function(in.dt, in.col, in.bycol = NULL, in.cilevel = NULL) {

  if (!is.null(in.cilevel)) {
    loc.dt  = in.dt[, .(coltmp1 = .N,
                        coltmp2 = mean(get(in.col)),
                        coltmp3 = sd(get(in.col)),
                        coltmp4 = sapply(.SD, function(x) {loc.res = t.test(x, alternative = 'two.sided'); return(loc.res$conf.int[1])}),
                        coltmp5 = sapply(.SD, function(x) {loc.res = t.test(x, alternative = 'two.sided'); return(loc.res$conf.int[2])})), by = in.bycol, .SDcols = in.col]

    setnames(loc.dt, c(in.bycol,
                       paste0(in.col, '.ncells'),
                       paste0(in.col, '.mn'),
                       paste0(in.col, '.sd'),
                       paste0(in.col, '.CIlo'),
                       paste0(in.col, '.CIhi')))

  } else {
    loc.dt  = in.dt[, .(coltmp1 = .N,
                        coltmp2 = mean(get(in.col)),
                        coltmp3 = sd(get(in.col))), by = in.bycol]

    setnames(loc.dt, c(in.bycol,
                       paste0(in.col, '.ncells'),
                       paste0(in.col, '.mn'),
                       paste0(in.col, '.sd')))
  }


  return(loc.dt)
}
