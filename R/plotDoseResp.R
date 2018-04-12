#' Plot dose response
#'
#' @param in.dt
#' @param in.xvar
#' @param in.yvar
#' @param in.group
#' @param in.facet
#' @param in.xlab
#' @param in.ylab
#'
#' @return ggPlots2 object
#' @export
#' @import data.table
#' @import ggplot2
#' @import scales
#'
#' @examples
#'
plotDoseResp = function(in.dt,
                          in.xvar = 'Concentration.num',
                          in.yvar = 'cytoNuc.mean',
                          in.group = 'Compound',
                          in.facet = 'Compound',
                          in.xlab = "\nConcentration (GDC0941 uM; Trametinib nM)",
                          in.ylab = "cytoplasmic / nuclear Foxo activity\n") {

  ggplot(in.dt, aes_string(x = sprintf("as.factor(%s)", in.xvar), y = in.yvar)) +
    geom_point() +
    stat_summary(aes_string(y = in.yvar, group = in.group),
                 fun.y = mean,
                 colour = "red",
                 linetype = 'solid',
                 geom = "line") +
    stat_summary(data = in.dt[get(in.xvar) == 0], aes_string(y = in.yvar, group = in.group),
                 fun.y = mean,
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
