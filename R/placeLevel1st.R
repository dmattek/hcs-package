#' Reorder levels: place a chosen factor level as first
#'
#' @param in.dt Input data.table
#' @param in.col String with the column name that contains factors to change
#' @param in.s String with the factor to place as first
#'
#' @return Reorders factors in a column within a dt
#' @import data.table
#' @export
#'
#' @examples
#' dt = data.table(x = 1:7, gr = factor(c('a', 'a', 'b', 'b', 'b', 'c', 'c')))
#'
#' placeLevel1st(dt, 'gr', 'c')
#' levels(dt[['gr']])

placeLevel1st = function(in.dt, in.col, in.s) {
  if (is.factor(in.dt[[in.col]])) {
    loc.levels = levels(in.dt[[in.col]])
    in.dt[, (in.col) := factor(get(in.col), levels = c(in.s, setdiff(loc.levels, in.s)))]
  } else
    stop('Selected column is not of type factor!')
}
