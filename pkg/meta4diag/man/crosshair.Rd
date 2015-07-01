\name{crosshair.meta4diag}
\alias{crosshair}
\alias{crosshair.meta4diag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crosshair Plot.
}
\description{
Takes a \code{meta4diag} object produced by the function \code{meta4diag()} and produces a crosshair plot or adds such a plot to an existing plot. Crosshair plots go back to Phillips et al. (2010).
}
\usage{
crosshair(x,...)
\method{crosshair}{meta4diag}(x, est.type = "mean", add = FALSE, save = F, xlim, ylim, xlab, ylab, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A \code{meta4diag} object.
}
  \item{est.type}{
  The estimates type used to make crosshair plot. Options are "mean" and "median".
}
  \item{add}{
  If add is TRUE, the plots are added to an existing plot, otherwise a new plot is created.
}
  \item{save}{
  If save is TRUE, the plots are saved (pdf format) automatically in the folder where the data is loaded. save could also be a file name, i.e. \code{save="plot1.eps"}, a file with name plot1 and eps format will be saved in the folder where the data is loaded.
}
 \item{xlim}{
  A numeric value, giving the x coordinates ranges.
}
  \item{ylim}{
  A numeric value, giving the y coordinates ranges.
}
\item{xlab}{
  A title for the x axis.
}
  \item{ylab}{
  A title for the y axis.
}
\item{\dots}{
  graphics parameters can also be passed to this function.
}
}

\references{
  Phillips, B., Stewart, L.A., & Sutton, A.J. (2010). "'Cross hairs' plots for diagnostic meta-analysis." Research Synthesis Methods, 1, 308-315. 
}
\value{
  Besides plotting, the function returns an invisible NULL.
}

\author{
  Jingyi Guo
}

\seealso{
  \code{meta4diag}
}
\examples{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter)
  crosshair(res, est.type="mean")
}
}