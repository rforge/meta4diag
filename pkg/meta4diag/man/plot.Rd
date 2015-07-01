\name{plot}
\alias{plot.meta4diag}
\title{Default meta4diag plotting.}
\description{
   Takes a \code{meta4diag} object produced by \code{meta4diag()} and plot the posterior marginals (possibly with underlying priors) for hyperparameters and fixed effects.
 }
\usage{
  \method{plot}{meta4diag}(x, var.type="var1", add=FALSE, overlay.prior = TRUE, 
      save = FALSE, width=5, height=5, ...)
}
\arguments{
  \item{x}{
    A \code{meta4diag} object.
  }
  \item{var.type}{
    Variable type that is of interest. Options are "var1", "var2", "rho" and names for fixed effects, which can be found after calling \code{summary()} function.
  }
  \item{add}{
    If add is TRUE, the plots are added to an existing plot, otherwise a new plot is created.
  }
  \item{overlay.prior}{
    Boolean to indicate whether the prior will be plotted overlay or not only for hyperparameters.
  }
  \item{save}{
    If save is TRUE, the plots are saved (pdf format) automatically in the working directory. save could also be a file name, i.e. \code{save="plot1.eps"}, a file with name plot1 and eps format will be saved in the working directory.
  }
  \item{width}{
    The width when used for saving the plot, unit of inches is used.
  }
  \item{height}{
    The height when used for saving the plot, unit of inches is used.
  }
  \item{\dots}{
    Arguments to be passed to methods, such as graphical parameters (see \link{par}) such as "main", "sub", "xlab", "ylab".
  }
}

\details{
Posterior marginal distribution possibly with underlying prior distribution is plotted.
}

\value{
  Besides plotting, the function returns an invisible NULL.
}
\author{
  Jingyi Guo \email{jingyi.guo@math.ntnu.no} and Andrea Riebler \email{andrea.riebler@math.ntnu.no}
}
\keyword{plot}
\seealso{\code{\link{meta4diag}}}
\examples{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter)
  plot(res, var.type="var1")
}
}