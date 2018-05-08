#' Plot single-cell dose response as violin plots
#'
#' Individual data points are plotted as points, then a line average is plotted as function of concentrations. For concentration equal to 0, a circle with a plus is plotted. Concentrations are treated as factors.
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

plotDoseRespViol = function(in.dt,
                            in.xvar,
                            in.yvar,
                            in.group.col = NULL,
                            in.facet = NULL,
                            in.ylim = NULL,
                            in.xlab = NULL,
                            in.ylab = NULL,
                            ...) {

  p.out = ggplot(in.dt, aes_string(x = sprintf("as.factor(%s)", in.xvar), y = in.yvar))

  p.out = p.out + geom_violin(aes_string(fill = in.group.col), scale = 'width', ...)

  if (!is.null(in.ylim))
    p.out = p.out +
    coord_cartesian(ylim = in.ylim)

  p.out = p.out +
    scale_fill_discrete(name = '') +
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
          legend.position = "top")
}
