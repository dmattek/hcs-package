
#' Read a CSV file with a 2-line header
#'
#' A CSV file with a 2-line header is frequently output by CellProfiler when merging separate object outputs is set
#' in ExportToSpreadsheet module. The output contains merged outputs from different objects (e.g. cytosol, nucleus),
#' which means that the header consists of 2 lines, first with object name, second with the measurement name.
#' These two lines are merged in this function.
#'
#' @param in.file Name of the CSV file.
#' @param in.col.rem String vector with column names to remove
#'
#' @return Data table with column names set according to 2 first lines of the header, i.e. merged with '_'.
#' @export
#' @import data.table

freadCSV2lineHeader = function(in.file, in.col.rem = NULL) {


  # Read the first two rows
  loc.dt.head = data.table::fread(in.file, nrows = 2, header = FALSE)

  # make a joint single-row header from two rows
  loc.s.head = paste0(loc.dt.head[1,], '_', loc.dt.head[2,])

  # read the rest of the output (except first two rows)
  loc.dt.nuc = data.table::fread(in.file, skip = 2)

  # set column names
  data.table::setnames(loc.dt.nuc, loc.s.head)

  # remove duplicated columns
  loc.dt.nuc = loc.dt.nuc[, loc.s.head[!duplicated(loc.s.head)], with = FALSE]

  # check whether the list of columns to remove provided as the f-n parameter
  # contains column names in the data table
  if (!is.null(in.col.rem)) {
    loc.col.rem = intersect(loc.s.head, in.col.rem)

    # remove columns if the list isn't empty
    if (length(loc.col.rem) > 0)
      loc.dt.nuc[, (loc.col.rem) := NULL]
  }

  return(loc.dt.nuc)
}
