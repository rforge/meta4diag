\name{print}
\alias{print}
\alias{print.meta4diag}
\alias{print.summary.meta4diag}
\alias{print.fitted.meta4diag}
\title{print meta4diag object.}
\description{
   Takes an \code{meta4diag} object which is obtained by function \code{meta4diag()} and print the model and data information such as model type used in the model, number of studies in the dataset and the name of covariates which are contained in the model. The variable type used in \code{plot.meta4diag} can be found here.
 }
\usage{
  \method{print}{meta4diag}(x, ...)
}
\arguments{
  \item{x}{
    A \code{meta4diag} object.
  }
  \item{\dots}{Further arguments passed to or from other methods.}
}
\details{
The basic model setting and dataset information are printed.
}
\value{The return value is invisible NULL.}

\author{
  Jingyi Guo \email{jingyi.guo@math.ntnu.no} and Andrea Riebler \email{andrea.riebler@math.ntnu.no}
}
  
\keyword{print}
\seealso{\code{\link{plot}}}

\examples{
\dontrun{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter)
  print(res)
}
}
}