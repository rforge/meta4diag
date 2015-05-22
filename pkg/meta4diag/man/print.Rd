\name{print}
\alias{print.meta4diag}
\alias{print.summary.meta4diag}
\title{print meta4diag object.}
\description{
   Takes am \code{meta4diag} object produced by \code{meta4diag()} and print the model and data information such as which model type is using, how many studies in the dataset and data contains covariates or modality or not. The variable type can be used in \code{plot.meta4diag} can be found here.
 }
\usage{
  \method{print}{meta4diag}(x, ...)
}
\arguments{
  \item{x}{
    A \code{meta4diag} object.
  }
  \item{\dots}{...}
}
\details{
The basic model setting and dataset information are printed.
}
\value{The return value is a list of the files created (if any).}

\author{
  Jingyi Guo \email{jingyi.guo@math.ntnu.no} and Andrea Riebler \email{andrea.riebler@math.ntnu.no}
}
  
\keyword{print}
\seealso{\code{\link{plot}}}
  \examples{
  \dontrun{
    data(Catheter)
    res <- meta4diag(data = Catheter)
    print(res)
  }
  }