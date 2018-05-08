#' Plot dose response
#'
#' Individual data points are plotted as dot plot, then a line average is plotted as function of concentrations. For concentration equal to 0, a circle with a plus is plotted. Concentrations are treated as factors.
#'
#' @param in.dt Input data table. Should contain at least two numeric columns, and one vategorical variable.
#' @param in.xvar String with the column name with the concentration.
#' @param in.yvar String with the column name with the measurement.
#' @param in.group String with the column name with grouping for the average line, e.g. drug name.
#' @param in.facet String with the column name for splitting data across plot facets, e.g. drug name or compound type (CTRL vs compounds)
#' @param in.xlab String for the x-axis label
#' @param in.ylab String for the y-axis label
#'
#' @return ggPlots2 object
#' @export
#' @import data.table
#' @import ggplot2
#' @import scales
#'
#' @examples
#' # Dose response of a single compound
#' dt = data.table(conc = rep(1:10,3), meas = rnorm(30), compound = rep('comp1', 30))
#' plotDoseResp(dt, in.xvar = 'conc', in.yvar = 'meas', in.group = 'compound', in.facet = 'compound', in.xlab = 'Concnetration (uM)', in.ylab = 'Mean. fl. int.')
#'
#' # Dose response of a single compound with control
#' dt = data.table(conc = c(rep(0, 5), rep(1:10, each = 3)), meas = c(rnorm(5, mean = 0, sd = .1), rnorm(30, mean = 2, sd = .1)), compound = c(rep('DMSO', 5), rep('comp1', 30)))
#' plotDoseResp(dt, in.xvar = 'conc', in.yvar = 'meas', in.group = 'compound', in.facet = 'compound', in.xlab = 'Concnetration (uM)', in.ylab = 'Mean. fl. int.')
#'
#' # Dose response of two compounds with control; two compounds plotted in the same facet, ctrol in a separate
#' dt = data.table(conc = c(rep(0, 5), rep(1:10, each = 3), rep(1:10, each = 3)),
#'                 meas = c(rnorm(5, mean = 0, sd = .1), rnorm(30, mean = 2, sd = .1), rnorm(30, mean = 1, sd = .1)),
#'                 compound = c(rep('DMSO', 5), rep('comp1', 30), rep('comp2', 30)),
#'                 type = c(rep('CTRL', 5), rep('compund', 60)))
#' plotDoseResp(dt, in.xvar = 'conc', in.yvar = 'meas', in.group = 'compound', in.facet = 'type', in.xlab = 'Concnetration (uM)', in.ylab = 'Mean. fl. int.')
#'

plotDoseRespDot = function(in.dt,
                           in.xvar = 'Concentration.num',
                           in.yvar = 'cytoNuc.mean',
                           in.group = 'Compound',
                           in.facet = 'Compound',
                           in.xlab = "\nConcentration (GDC0941 uM; Trametinib nM)",
                           in.ylab = "cytoplasmic / nuclear Foxo activity\n",
                           ...) {

  ggplot(in.dt, aes_string(x = sprintf("as.factor(%s)", in.xvar), y = in.yvar)) +
    geom_dotplot(binaxis = "y", stackdir = "center", ...) +
    stat_summary(aes_string(y = in.yvar, group = in.group),
                 fun.y = median,
                 colour = "red",
                 linetype = 'solid',
                 geom = "line",
                 size= 1) +
    stat_summary(data = in.dt[get(in.xvar) == 0], aes_string(y = in.yvar, group = in.group),
                 fun.y = median,
                 colour = "red",
                 size = 5,
                 geom = "point",
                 shape = 10) +
    facet_grid(as.formula(paste("~", in.facet)), scales = "free_x", space = "free_x") +
    xlab(in.xlab) +
    ylab(in.ylab) +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.border=element_blank(),
          axis.line.x = element_line(color="black", size = 0.25),
          axis.line.y = element_line(color="black", size = 0.25),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1),
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=14, face="bold"),
          strip.text.y = element_text(size=14, face="bold"),
          strip.background = element_blank(),
          legend.key = element_blank(),
          legend.key.height = unit(2, "lines"),
          legend.key.width = unit(2, "lines"),
          legend.position = "right")
}
