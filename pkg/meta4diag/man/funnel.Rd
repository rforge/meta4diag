\name{funnel}
\alias{funnel}
\alias{funnel.meta4diag}

\title{Funnel plot.}

\description{
  Takes a \code{meta4diag} object produced by the function \code{meta4diag()} and draws a funnel plot. 
}
\usage{
  \method{funnel}{meta4diag}(x, est.type="median",intervals=c(0.025,0.975), 
  lwd=1, arrow.lty=1,col="black",
  main="Funnel Plot", xlim, ylim, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{meta4diag} object obtained by running the main function \code{meta4diag()}.}
  \item{est.type}{The type of estimation of study specified summary points. Options are "mean" and "median".}
  \item{intervals}{A numerical vector with length 2 specifying the credible intervals that is of interst. The values should be taken from the argument quantiles (see meta4diag). The first value should be smaller than 0.5 and the second value should be larger than 0.5.}
  \item{arrow.lty}{Arrow line style.}
  \item{lwd}{Line width.}
  \item{col}{Color of cross.}
  \item{main}{An overall title for the plot.}
  \item{xlim}{A numeric value, giving the x coordinates ranges.}
  \item{ylim}{A numeric value, giving the y coordinates ranges.}
  \item{...}{Arguments to be passed to methods.}
}

\details{
  A funnel plot is a graphical representation of a meta-analysis. Funnel plots are used primarily as a visual aid for detecting bias or heterogeneity, and often it is not possible to distinguish between the two. A complete collection of unbiased studies, all estimating the same underlying effect with different levels of precision, would produce a symmetric funnel shape, with increasing scatter with decreasing precision. An asymmetric funnel indicates a relationship between effect size and precision in the studies at hand. This is most likely to be due to reporting bias, to a systematic difference between smaller and larger studies, or to the presence of subsets of studies with different mean effect sizes.
  
  The function \code{funnel()} gives the user flexibility to control the plots.
}

\value{Besides plotting, the function returns an invisible NULL.}

\references{
  Anzures-Cabrera J and Higgins J (2010). Graphical displays for meta-analysis: An
overview with suggestions for practice. Research Synthesis Methods,
  1, 66-80. 
}

\author{
  Jingyi Guo and Andrea Riebler 
}

\examples{
  \dontrun{
    if(requireNamespace("INLA", quietly = TRUE)){
      require("INLA", quietly = TRUE)
      data(Catheter)
      res <- meta4diag(data = Catheter)
      funnel(res, est.type = "median")
    }
  }
}