\name{summary}
\alias{summary}
\alias{summary.meta4diag}
\title{Provide summary information.}
\description{
Takes a fitted \code{meta4diag} object produced by \code{meta4diag()} and produces a summary from it.
}
\usage{
  \method{summary}{meta4diag}(object,...)
}
\arguments{
  \item{object}{a fitted \code{meta4diag} object as produced by \code{meta4diag()}.}
  \item{...}{ other arguments may be useful.}
}

\details{
Posterior mean and standard deviation (together with quantiles) are printed for the fixed effects in the model.

For the random effects the function \code{summary()} prints the
posterior mean and standard deviations of the hyperparameters.
}

\value{
  \code{summary.meta4diag} returns a list with components: 
  \item{cpu.used}{The cpu time used to fit the corresponding model and data.}
  \item{summary.fixed}{The posterior mean and standard deviation (together with quantiles) for the fixed effects.}
  \item{summary.expected.g}{The posterior mean and standard deviation (together with quantiles) for the summarized fixed effects.}
  \item{summary.hyperpar}{The posterior mean and standard deviation (together with quantiles) for model hyperparameters.}
  \item{mlik}{The marginal log-likelihood of the model.} 
  \item{var.type}{The variables type used in the \code{plot()} function.}
}

\author{
  Jingyi Guo \email{jingyi.guo@math.ntnu.no} and Andrea Riebler \email{andrea.riebler@math.ntnu.no}
}
\keyword{summary}
\seealso{\code{\link{plot}},\code{\link{meta4diag}}}
\examples{
\dontrun{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter)
  summary(res)
  }
}
}