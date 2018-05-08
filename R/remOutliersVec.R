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
#' @param in.dt Input vector
#' @param keeperc Percentage (fraction) of data to keep
#' @param mode Specifies where to remove data. The default \code{both} removes on both sides and keeps the inner fraction. If \code{left} (\code{right}), data on the left (right) hand side of the distribution is removed.
#'
#' @return vector without outliers. All columns are preserved.
#' @export
#'
#' @examples
#' v = rnorm(1000, 1, 0)
#'
#' # keep 99% of inner data
#' remOutliers(v)
#'
#' # keep 90% of data; data will be removed from the left side of the histogram
#' remOutliers(v, 0.9, 'left')

remOutliersVec = function(in.v, keeperc = 0.99, mode = c('both', 'left', 'right')) {
  mode = match.arg(mode)

  if(!is.numeric(in.v))
    stop('Selected column is not of type \'numeric\'.')

  switch(mode,
         both = {
           loc.bnd = (1-keeperc)*0.5
           loc.qnt = quantile(in.v, probs = c( loc.bnd, 1 - loc.bnd ))
           out.v = in.v[in.v > loc.qnt[1] & in.v < loc.qnt[2]]
         },
         left = {
           loc.bnd = 1-keeperc
           loc.qnt = quantile(in.v, probs = loc.bnd)
           out.v = in.v[in.v > loc.qnt]
         },
         right = {
           loc.bnd = keeperc
           loc.qnt = quantile(in.v, probs = loc.bnd)
           out.v = in.v[in.v < loc.qnt]
         })

  return(out.v)
}
