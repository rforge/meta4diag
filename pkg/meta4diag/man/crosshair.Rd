\name{crosshair}
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
\method{crosshair}{meta4diag}(x, est.type = "mean", add = FALSE, main="Crosshair Plot", xlim, ylim, ...)
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
\item{main}{
  A overall title for the plot.
}
 \item{xlim}{
  A numeric value, giving the x coordinates ranges.
}
  \item{ylim}{
  A numeric value, giving the y coordinates ranges.
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
\dontrun{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter)
  crosshair(res, est.type="mean")
}
}
}