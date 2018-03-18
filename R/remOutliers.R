# remove outliers from left and/or right
# such that the inner 'keeperc' of data is kept
# in.dt - data.table
# in.colname - string with the oclumn name to process
# keeperc - percentage (fraction) of data to keep
# mode - specifiec where to remove data
#        'left' - data is removed on the left, and keeperc of data on the right is kept
#        'both' - data is cut on both sides and the inner keeperc of data is kept


#' Remove outliers from left and/or right of the distribution
#'
#' Data is removed such that the inner fraction (specified by parameter \code{keeperc}) is kept
#'
#' @param in.dt Input data.table
#' @param in.colname String with the column name to process. The column has to be of type numeric.
#' @param keeperc Percentage (fraction) of data to keep
#' @param mode Specifies where to remove data. The default \code{both} removes on both sides and keeps the inner fraction. If \code{left} (\code{right}), data on the left (right) hand side of the distribution is removed.
#'
#' @return data table without outliers. All columns are preserved.
#' @export
#' @import data.table
#'
#' @examples
#' require(data.table)
#' dt = data.table(x = rnorm(1000, 1, 0))
#'
#' # keep 99% of inner data
#' remOutliers(dt, 'x')
#'
#' # keep 90% of data; data will be removed from the left side of the histogram
#' remOutliers(dt, 'x', 0.9, 'left')

remOutliers = function(in.dt, in.colname, keeperc = 0.99, mode = c('both', 'left', 'right')) {
  mode = match.arg(mode)

  if(!is.numeric(in.dt[[in.colname]]))
    stop('Selected column is not of type \'numeric\'.')

  switch(mode,
         both = {
           loc.bnd = (1-keeperc)*0.5
           loc.qnt = quantile(in.dt[, get(in.colname)], probs = c( loc.bnd, 1 - loc.bnd ))
           out.dt = in.dt[get(in.colname) > loc.qnt[1] & get(in.colname) < loc.qnt[2]]
         },
         left = {
           loc.bnd = 1-keeperc
           loc.qnt = quantile(in.dt[, get(in.colname)], probs = loc.bnd)
           out.dt = in.dt[get(in.colname) > loc.qnt]
         },
         right = {
           loc.bnd = keeperc
           loc.qnt = quantile(in.dt[, get(in.colname)], probs = loc.bnd)
           out.dt = in.dt[get(in.colname) < loc.qnt]
         })

  return(out.dt)
}
