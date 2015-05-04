\name{runModel}
\alias{runModel}
\title{Run the bivariate model.}
\description{
	Run the bivariate model with INLA. This function is used within the main function \code{meta4diag()} and can also be used as a separate function.
}
\usage{
runModel(outdata, outpriors, model.type = 1, link = "logit", 
  level = c(0.025, 0.5, 0.975), verbose = FALSE)
}
\arguments{
  \item{outdata}{
	A data file for internal use.
}
  \item{outpriors}{
	A list prior settings for internal use.
}
  \item{model.type}{
	A numerical value specifying the model type, options are 1(default), 2, 3 and 4. \code{model.type=1} indicates that the Sensitivity(se) and Specificity(sp) will be modelled in the bivariate model, i.e. \eqn{g(se)}{g(se)} and \eqn{g(sp)}{g(sp)} are bivariate normal distributed. \code{model.type=2,3,4} indicate that the Sensitivity(se) and False Negative Rate(1-sp), False Positive Rate(1-se) and Specificity(sp), False Positive Rate(1-se) and False Negative Rate(1-sp) are modelled in the bivariate model, respectively.
}
  \item{link}{
	A string specifying the link function used in the model. Options are "logit", "probit" and "cloglog".
}
  \item{level}{
	A vector of quantiles, p(0), p(1),... to compute for each posterior marginal. The function returns, for each posterior marginal, the values x(0), x(1),... such that \deqn{Prob(X<x)=p}{Prob(X<x)=p}
}
  \item{verbose}{
	Boolean (default:FALSE) indicating whether the program should run in a verbose model.
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
data(Catheter)
outdata = makeData(data=Catheter,model.type=1,covariates="type")
outpriors = makePriors(var.prior = "invgamma", cor.prior = "normal", 
  var.par = c(0.25, 0.025), cor.par = c(0, 5))
runModel(outdata, outpriors, model.type = 1, 
  link = "logit", level = c(0.025, 0.5, 0.975), verbose = FALSE)
}