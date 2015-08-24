\name{runModel}
\alias{runModel}
\title{Run the bivariate model.}
\description{
	Run the bivariate model with INLA. This function is used within the main function \code{meta4diag()} and can also be used as a separate function.
}
\usage{
runModel(outdata, outpriors, link = "logit", 
  quantiles = c(0.025, 0.5, 0.975), verbose = FALSE)
}
\arguments{
  \item{outdata}{
	A data file for internal use.
}
  \item{outpriors}{
	A list of prior settings prepared for internal use, see \code{makePriors}.
}
  \item{link}{
	A string specifying the link function used in the model. Options are "logit", "probit" and "cloglog".
}
  \item{quantiles}{
	A vector of quantiles, p(0), p(1),... to compute for each posterior marginal. The function returns, for each posterior marginal, the values x(0), x(1),... such that \deqn{Prob(X<x)=p.}{Prob(X<x)=p.}
  The default value are c(0.025, 0.5, 0.975). Not matter what other values are going to be given, the estimates for these 3 quantiles are always returned.
}
  \item{verbose}{
	Boolean (default:FALSE) indicating whether the program should run in a verbose mode.
}
}

\value{
	A \code{INLA} object which will be used into function \code{makeObject()}.
}
\references{
  Havard Rue, Sara Martino, and Nicholas Chopin (2009). Approximate
  Bayesian Inference for Latent Gaussian Models Using Integrated Nested
  Laplace Approximations. Journal of the Royal Statistical Society B,
  71, 319-392. (www.r-inla.org)  
}
\author{
  Jingyi Guo and Andrea Riebler 
}

\seealso{
  \code{makeData, makePriors, makeObject, meta4diag, inla}
}
\examples{
\dontrun{
if(requireNamespace("INLA", quietly = TRUE)) {
   require("INLA", quietly = TRUE)
   data(Catheter)

   outdata = makeData(data=Catheter,model.type=1,covariates="type")
   outpriors = makePriors(var.prior = "invgamma", cor.prior = "normal", 
                          var.par = c(0.25, 0.025), cor.par = c(0, 5))
   runModel(outdata, outpriors, link = "logit", 
            quantiles = c(0.025, 0.5, 0.975), verbose = FALSE)
}
}
}