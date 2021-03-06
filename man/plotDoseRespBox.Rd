% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotDoseRespBox.R
\name{plotDoseRespBox}
\alias{plotDoseRespBox}
\title{Plot single-cell dose response as box-plots}
\usage{
plotDoseRespBox(in.dt, in.xvar = "Concentration.num",
  in.yvar = "cytoNuc.mean", in.group = "Compound", in.facet = "Compound",
  in.xlab = "\\nConcentration (GDC0941 uM; Trametinib nM)",
  in.ylab = "cytoplasmic / nuclear Foxo activity\\n", in.type = "box")
}
\arguments{
\item{in.dt}{Input data table. Should contain at least two numeric columns, and one vategorical variable.}

\item{in.xvar}{String with the column name with the concentration.}

\item{in.yvar}{String with the column name with the measurement.}

\item{in.group}{String with the column name with grouping for the average line, e.g. drug name.}

\item{in.facet}{String with the column name for splitting data across plot facets, e.g. drug name or compound type (CTRL vs compounds)}

\item{in.xlab}{String for the x-axis label}

\item{in.ylab}{String for the y-axis label}
}
\value{
ggPlots2 object
}
\description{
Individual data points are plotted as points, then a line average is plotted as function of concentrations. For concentration equal to 0, a circle with a plus is plotted. Concentrations are treated as factors.
}
\examples{
# Dose response of a single compound
dt = data.table(conc = rep(1:10,3), meas = rnorm(30), compound = rep('comp1', 30))
plotDoseResp(dt, in.xvar = 'conc', in.yvar = 'meas', in.group = 'compound', in.facet = 'compound', in.xlab = 'Concnetration (uM)', in.ylab = 'Mean. fl. int.')

# Dose response of a single compound with control
dt = data.table(conc = c(rep(0, 5), rep(1:10, each = 3)), meas = c(rnorm(5, mean = 0, sd = .1), rnorm(30, mean = 2, sd = .1)), compound = c(rep('DMSO', 5), rep('comp1', 30)))
plotDoseResp(dt, in.xvar = 'conc', in.yvar = 'meas', in.group = 'compound', in.facet = 'compound', in.xlab = 'Concnetration (uM)', in.ylab = 'Mean. fl. int.')

# Dose response of two compounds with control; two compounds plotted in the same facet, ctrol in a separate
dt = data.table(conc = c(rep(0, 5), rep(1:10, each = 3), rep(1:10, each = 3)),
                meas = c(rnorm(5, mean = 0, sd = .1), rnorm(30, mean = 2, sd = .1), rnorm(30, mean = 1, sd = .1)),
                compound = c(rep('DMSO', 5), rep('comp1', 30), rep('comp2', 30)),
                type = c(rep('CTRL', 5), rep('compund', 60)))
plotDoseResp(dt, in.xvar = 'conc', in.yvar = 'meas', in.group = 'compound', in.facet = 'type', in.xlab = 'Concnetration (uM)', in.ylab = 'Mean. fl. int.')

}
