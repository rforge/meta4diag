\name{meta4diag}
\alias{meta4diag}
\title{
    Function to analyse diagnostic meta-analysis with Bayesian methods using INLA.
}
\description{
    Estimate a Bayesian bivariate hierarchical model fitted within INLA. The bivariate model has two levels, in the first level, the observed number of individuals in a specific group in a 2 by 2 table is binomial distributed, for example, the numbers of individuals in the group of true positive and true negative of a study \eqn{i} are modelled jointly,
    \deqn{TP_{i} | Se_{i} \sim Binomial(TP_{i}+ FN_{i},Se_{i})}
    \deqn{TN_{i} | Sp_{i} \sim Binomial(TN_{i}+ FP_{i},Sp_{i})}
In the second level, two transformed accuracies with some link function (see argument \code{link}) are bivariate Gaussian distributed, continuous with the previous example,
    \deqn{g(Se_{i}) = \mu + V_{i}\alpha + \phi_{i}}
    \deqn{g(Sp_{i}) = \nu + U_{i}\beta + \psi_{i}}
where \eqn{\phi_{i}} and \eqn{\psi_{i}} are bivariate Gaussian distributed with mean 0 and covariance matrix \eqn{\Sigma}. The \eqn{se} and \eqn{sp} in the example could be changed to \eqn{se} and \eqn{(1-sp)}, \eqn{(1-se)} and \eqn{sp} or \eqn{(1-se)} and \eqn{(1-sp)}, see argument \code{model.type}.

The function \code{meta4diag()} depends on four internal functions which are also given in the \pkg{meta4diag} package in order to give the user flexibility and convienience to implement a list of dataset with the same prior setting or with the same dataset but different prior settings. The four internal functions are \code{makeData()}, \code{makePriors()}, \code{runModel()} and \code{makeObject()}. Details can be seen the corresponding documentations and examples.

After running the function \code{meta4diag()}, a \code{meta4diag} object will be returned which contains various estimated results for later analysis, such as the posterior marginals, estimated value, standard deviations and the coresponding quaniles of the accuracies. See \code{Values}.
}
\usage{
meta4diag(data = NULL, model.type = 1, var.prior = "invgamma", var2.prior, 
  cor.prior = "normal", var.par = c(0.25, 0.025), var2.par, cor.par = c(0, 5), 
  wishart.par = c(4, 1, 1, 0),
  init = c(0.01, 0.01, 0), link = "logit", level = c(0.025, 0.5, 0.975), 
  verbose = FALSE, covariates = NULL, nsample = FALSE)
}
\arguments{
  \item{data}{
	A data frame contains at least 4 columns specifying the number of True Positive, False Negative, True Negative and False Positive, called \code{TP}, \code{FN}, \code{TN} and \code{FP}, respectively. The additional columns besides columns named \code{studynames} or \code{modality} will be considered as potential covariates and the name or the column number of the potential covariates can be set in the argument \code{covariates} to use it in the model.
}
  \item{model.type}{
	A numerical value specifying the model type, options are 1(default), 2, 3 and 4. \code{model.type=1} indicates that the Sensitivity(se) and Specificity(sp) will be modelled in the bivariate model, i.e. \eqn{g^{-1}(se)}{g^(-1)(se)} and \eqn{g^{-1}(sp)}{g^(-1)(sp)} are bivariate normal distributed, where \eqn{g^{-1}}{g^(-1)} is the link function. \code{model.type=2,3,4} indicate that the Sensitivity(se) and False Negative Rate(1-sp), False Positive Rate(1-se) and Specificity(sp), False Positive Rate(1-se) and False Negative Rate(1-sp) are modelled in the bivariate model, respectively.
}
  \item{var.prior}{
  A string specifying the prior density for the first variance component, options are "PC" and "Invgamma". "PC" indicates the PC-prior and "Invgamma" indicates the inverse gamma prior.
}
  \item{var2.prior}{
	A string specifying the prior density for the second variance component, options are "PC" and "Invgamma". If not given, function will copy the setting for the first variance component.
}
  \item{cor.prior}{
	A string specifying the prior density for the correaltion, options are "PC" and "Normal".
}
  \item{var.par}{
  A numerical vector with length 2 specifying the parameters of the prior density for the first variance component. \code{var.par=c(rate, shape)} when \code{var.prior="Invgamma"} and \code{var.par=c(u, alpha)} when \code{var.prior="PC"}.
}
  \item{var2.par}{
	A numerical vector with length 2 specifying the parameter of the prior density for the first variance component. If not given, function will copy the setting for the first variance component. \code{var.par=c(rate, shape)} when \code{var.prior="Invgamma"} and \code{var.par=c(u, alpha)} when \code{var.prior="PC"}.
}
  \item{cor.par}{
	A numerical vector with length 2 when \code{cor.prior="Normal"} or length 7 when \code{cor.prior="PC"} specifying the parameter of the prior density for correlation. \code{cor.par=c(mean, variance)} when \code{var.prior="Normal"} and \code{cor.par=c(}\code{strategy}, \code{rho.ref}, \code{left.portion}, \code{u1}, \code{alpha1}, \code{u2}, \code{alpha2}) when \code{cor.prior="PC"}. When \code{strategy=1}, the reference value of correlation \eqn{rho}{rho}(\code{rho.ref}), the prior density weight on the left hand of the reference value(\code{left.portion}), the minimum value of interest(\code{u1}) and the prior density weight on the left hand of the minimum value(\code{alpha1}) must be given, and the maximum value of interest(\code{u2}) and the prior density weight on the right hand of the maximum value(\code{alpha2}) should be set to NA. When \code{strategy=2}, rho,ref, left.portion, u2, alpha2 must be given and u1, alpha1 set to be NA. When \code{strategy=3}, left.portion should be set to NA, other values must be given. See also \code{examples}.
}
  \item{wishart.par}{
  dependadfadfadfadfasda
  }
  \item{init}{
	A numerical vector specifying the initial value of the first variance, the second variance and correlation.
}
   \item{link}{
  A string specifying the link function used in the model. Options are "logit", "probit" and "cloglog".
}
  \item{level}{
	A vector of quantiles, p(0), p(1),... to compute for each posterior marginal. The function returns, for each posterior marginal, the values x(0), x(1),... such that 
  \deqn{Prob(X<x)=p}{Prob(X<x)=p}
}
  \item{verbose}{
	Boolean (default:FALSE) indicating whether the program should run in a verbose model.
}
  \item{covariates}{
	A vector specifying the covariates. Default value is NULL. See also \code{examples}.
}
  \item{nsample}{
	A numerical value specifying the number of posterior samples, default is 5000. The posterior samples are used to compute the marginals and estimates values of log radios and diagnostic odds radios. If \code{nsample} is given, \code{summary.summarized.statistics}, \code{summary.fitted.LRpos}, \code{summary.fitted.LRneg}, \code{summary.fitted.DOR} and samples of \eqn{E(se)}, \eqn{E(sp)}, \eqn{E(1-se)} and \eqn{E(1-sp)} will be returned.
}
}

\value{
	\code{meta4diag} returns a \code{meta4diag} object with components: 
  \item{data}{The original data.}
  \item{outdata}{The formed data that could be used in \pkg{INLA} from function \code{makeData()}.}
  \item{priors.density}{Prior distributions for the variance components and correlation from function \code{makePriors()}.}
  \item{priors.distance}{Prior distributions in distance scale for the variance components and correlation from function \code{makePriors()}.}
  \item{names.fitted}{Names of the jointly modeled accuracies in the model. For example, se and sp or (1-se) and sp.} 
  \item{names.transf.fitted}{Names of transformed accuracies. If se and sp are jointly estimated in model, then \code{names.transf.fitted=c("(1-se)","(1-sp").}}
  \item{cpu.used}{The cpu time used for implementing the model.}
  \item{call}{The matched call.}
  \item{summary.fixed}{Matrix containing the mean and standard
deviation (plus, possibly quantiles) of the the fixed
effects of the model.}
\item{marginals.fixed}{A list containing the posterior marginal
densities of the fixed effects of the model.}
\item{summary.summarized.fixed}{Matrix containing the mean and standard
deviation (plus, possibly quantiles) of transformed mean accuracies.}
\item{marginals.summarized.fixed}{A list containing the posterior marginal
densities of transformed mean accuracies.}
\item{summary.summarized.fitted}{Matrix containing the mean and standard
deviation (plus, possibly quantiles) of back-transformed mean accuracies.}
\item{marginals.summarized.fitted}{A list containing the posterior marginal
densities of back-transformed mean accuracies.}
\item{summary.hyperpar}{A matrix containing the mean and sd
(plus, possibly quantiles) of the hyperparameters of the model }
\item{marginals.hyperpar}{A list containing the posterior marginal
densities of the hyperparameters of the model.} 
\item{summarized.fixed.correlation.matrix}{A correlation matrix between transformed mean accuracies.} 
\item{summarized.fixed.covariance.matrix}{A covariance matrix between transformed mean accuracies.}
\item{summary.predict.(...)}{A matrix containing the mean and sd
 (plus, possibly quantiles) of the linear predictors one transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}.For example, the possible accuracy type could be \eqn{g(se)} and \eqn{g(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
\item{marginals.predict.(...)}{A list containing the posterior marginals of the linear predictors of one transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g(se)} and \eqn{g(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
\item{summary.fitted.(...)}{A matrix containing the mean and sd
 (plus, possibly quantiles) of the linear predictors one back-transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g^{-1}(se)} and \eqn{g^{-1}(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
\item{marginals.fitted.(...)}{A list containing the posterior marginals of the linear predictors of one back-transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g^{-1}(se)} and \eqn{g^{-1}(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
\item{misc}{Soem other settings that maybe useful.}
\item{dic}{The deviance information criteria and effective number of parameters.}
\item{cpo}{A list of three elements: \code{cpo$cpo} are the values of the conditional 
predictive ordinate (CPO), \code{cpo$pit} are the values of the 
probability integral transform (PIT) and \code{cpo$failure} 
indicates whether some assumptions are violated. In short, if 
cpo$failure[i] > 0 then some assumption is violated, the higher the 
value (maximum 1) the more seriously.}
\item{waic}{A list of two elements: \code{waic$waic} is the Watanabe-Akaike information criteria,  and 
\code{waic$p.eff} is the estimated effective number of parameters.}
\item{mlik}{The log marginal likelihood of the model}
\item{inla.result}{A \code{INLA} object that from function \code{runModel()} which implements INLA.}
\item{summary.summarized.statistics}{A matrix containing the mean and sd
 (plus, possibly quantiles) of mean positive and negative likelihood ratios and mean diagnostic odds ratios if\code{nsample} is given.}
 \item{summary.fitted.LRpos}{A matrix containing the mean and sd
 (plus, possibly quantiles) of fitted positive likelihood ratios for each study if\code{nsample} is given.}
  \item{summary.fitted.LRneg}{A matrix containing the mean and sd
 (plus, possibly quantiles) of fitted negative likelihood ratios for each study if\code{nsample} is given.}
  \item{summary.fitted.DOR}{A matrix containing the mean and sd
 (plus, possibly quantiles) of fitted diagnostic odds ratios for each study if\code{nsample} is given.}
 \item{mean(Se).samples}{A vector of mean sensitivity samples if\code{nsample} is given.}
 \item{mean(Sp).samples}{A vector of mean specificity samples if\code{nsample} is given.}
 \item{mean(1-Se).samples}{A vector of mean fnr samples if\code{nsample} is given.}
 \item{mean(1-Sp).samples}{A vector of mean fpr samples if\code{nsample} is given.}
}

\references{
  Havard Rue, Sara Martino, and Nicholas Chopin (2009). Approximate
  Bayesian Inference for Latent Gaussian Models Using Integrated Nested
  Laplace Approximations. Journal of the Royal Statistical Society B,
  71, 319-392. (www.r-inla.org) 

Simpson DP, Martins TG, Riebler A, Fuglstad GA, Rue H, Sorbye SH (2014) Penalised Model Component Complexity: A principled, Practical Approach to Constructing Priors. Arxiv e-prints. 1403.4630 
}
\author{
  Jingyi Guo and Andrea Riebler 
}

\seealso{
  \code{makeData, makePrior, runModel, forest, SROC, crosshair.}
}
\examples{
\dontrun{
data(Catheter)

meta4diag(data = Catheter, model.type = 1, var.prior = "invgamma", cor.prior = "normal", 
  var.par = c(0.25, 0.025), cor.par = c(0, 5), init = c(0.01, 0.01, 0), 
  link = "logit", level = c(0.025, 0.5, 0.975), verbose = FALSE, covariates = NULL, 
  nsample = FALSE)}
}