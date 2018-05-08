#' Density plot of a quantity
#'
#' @param in.dt
#' @param in.meas
#' @param in.facet
#' @param in.vline
#' @param in.bdepth
#' @param in.xlab
#'
#' @return ggPlot object
#' @export
#' @import data.table
#' @import ggplot2
#' @import scales
#'
#' @examples

plotDens = function(in.dt,
                    in.meas,
                    in.group = NULL,
                    in.facet = NULL,
                    in.vline = NULL,
                    in.bdepth = NULL,
                    in.xlab = "\nlog10 mean fl.int. per cell (nucleus + cytosol)") {

  loc.p = ggplot(in.dt, aes_string(in.meas))

  if (is.null(in.group))
    loc.p = loc.p +
      geom_density()
  else
    loc.p = loc.p +
      geom_density(aes_string(color = in.group))


  if (!is.null(in.vline) | !is.null(in.bdepth)) {
    if(in.meas %like% 'log10') {
      loc.p = loc.p +
        geom_vline(xintercept = log10(in.vline[1]/2^in.bdepth), colour = 'red', linetype = 'dashed') +
        geom_vline(xintercept = log10(in.vline[2]/2^in.bdepth), colour = 'red', linetype = 'dashed')
    } else {
      loc.p = loc.p +
        geom_vline(xintercept = (in.vline[1]/2^in.bdepth), colour = 'red', linetype = 'dashed') +
        geom_vline(xintercept = (in.vline[2]/2^in.bdepth), colour = 'red', linetype = 'dashed')
    }
  }

  if (!is.null(in.facet))
    loc.p = loc.p +
      facet_wrap(as.formula(paste("~", in.facet)))



  loc.p = loc.p +
    xlab(in.xlab) +
    ylab("Density\n") +
    theme_bw(base_size = 14, base_family = "Helvetica") +
    theme(panel.border=element_blank(),
          axis.line.x = element_line(color="black", size = 0.25),
          axis.line.y = element_line(color="black", size = 0.25),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=14, face="bold"),
          strip.text.y = element_text(size=14, face="bold"),
          strip.background = element_blank(),
          legend.key = element_blank(),
          legend.key.height = unit(2, "lines"),
          legend.key.width = unit(2, "lines"),
          legend.position = "right")

  return(loc.p)
}
