\name{makeObject}
\alias{makeObject}
\title{
  A function used to make a \code{meta4diag} object.
}
\description{
  Takes an internal data list, an internal prior setting list and an \code{INLA} object produced by \code{makeData()}, \code{makePriors()} and \code{runModel()}, respectively and makes a \code{meta4diag} object which contains various informations for later use. This function is used in the main function \code{meta4diag()} and can also be used as a separate function.
}
\usage{
  makeObject(model, nsample=FALSE, seed=0L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
    An \code{INLA} object. Get from function \code{runModel()}.
  }
  \item{nsample}{
    A numerical value specifying the number of posterior samples, default is FALSE. The posterior samples are used to compute the marginals and estimates values of non-linear functions, such as log ratios and diagnostic odds ratios. If \code{nsample} is given, \code{summary.summarized.statistics}, \code{summary.fitted.LRpos}, \code{summary.fitted.LRneg}, \code{summary.fitted.DOR} and samples of \eqn{E(se)}, \eqn{E(sp)}, \eqn{E(1-se)} and \eqn{E(1-sp)} will be returned.
  }
  \item{seed}{
    A numerical value specifying the random seed to control the RNG for generating posterior samples if nsample > 0. If you want reproducible results, you ALSO need to control the seed for the RNG in R by controlling the variable .Random.seed or using the function set.seed.
  }
}


\value{
  \code{makeObject} returns a \code{meta4diag} object with components: 
  \item{data}{The provided input data.}
  \item{outdata}{The internal data that could be used in \pkg{INLA} from function \code{makeData()}.}
  \item{priors.density}{Prior distributions for the variance components and correlation from function \code{makePriors()}.}
  \item{names.fitted}{Names of the jointly modelled accuracies in the model. For example, se and sp or (1-se) and sp.} 
  \item{cpu.used}{The cpu time used for running the model.}
  \item{call}{The matched call.}
  \item{summary.fixed}{Matrix containing the mean and standard deviation (plus, possibly quantiles) of the fixed effects of the model.}
  \item{marginals.fixed}{A list containing the posterior marginal densities of the fixed effects of the model.}
  \item{summary.expected.(...).accuracy}{Matrix containing the mean and standard deviation (plus, possibly quantiles) of the mean of accuracies transformed with the link function, i.e. E(g(Se)), E(g(Sp)), E(g(1-Se)) and E(g(1-Sp)). Dynamic name for this output. (...) indicates the name of link function used in \code{runModel()}, i.e. if link function is "logit", the full name of this output is "summary.expected.logit.accuracy".}
  \item{marginals.expected.(...).accuracy}{A list containing the posterior marginal densities of the mean of accuracies transformed with the link function, i.e. E(g(Se)), E(g(Sp)), E(g(1-Se)) and E(g(1-Sp)). Dynamic name for this output. (...) indicates the name of link function used in \code{runModel()}, i.e. if link function is "logit", the full name of this output is "marginals.expected.logit.accuracy".}
  \item{summary.expected.accuracy}{Matrix containing the mean and standard deviation (plus, possibly quantiles) of the mean of the accuracies, i.e.  E(Se), E(Sp), E(1-Se) and E(1-Sp).}
  \item{marginals.expected.accuracy}{A list containing the posterior marginal densities of of the mean of the accuracies, i.e.  E(Se), E(Sp), E(1-Se) and E(1-Sp).}
  \item{summary.hyperpar}{A matrix containing the mean and sd (plus, possibly quantiles) of the hyperparameters of the model.}
  \item{marginals.hyperpar}{A list containing the posterior marginal densities of the hyperparameters of the model.} 
  \item{correlation.expected.(...).accuracy}{A correlation matrix between the mean of the accuracies transformed with the link function. Dynamic name for this output. (...) indicates the name of link function used in \code{runModel()}.} 
  \item{covariance.expected.(...).accuracy}{A covariance matrix between the mean of the accuracies transformed with the link function. Dynamic name for this output. (...) indicates the name of link function used in \code{runModel()}.}
  \item{summary.predictor.(...)}{A matrix containing the mean and sd (plus, possibly quantiles) of the linear predictors one transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g(se)}, \eqn{g(sp)}, \eqn{(se)} or \eqn{(sp)}, where \eqn{g()} is the link function.}
  \item{marginals.predictor.(...)}{A list containing the posterior marginals of the linear predictors of one transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g(se)}, \eqn{g(sp)}, \eqn{(se)} or \eqn{(sp)}, where \eqn{g()} is the link function.}
  \item{misc}{Some other settings that maybe useful retruned by \pkg{meta4diag}.}
  \item{dic}{The deviance information criteria and effective number of parameters.}
  \item{cpo}{A list of three elements: \code{cpo$cpo} are the values of the conditional predictive ordinate (CPO), \code{cpo$pit} are the values of the 
probability integral transform (PIT) and \code{cpo$failure} 
indicates whether some assumptions are violated. In short, if 
cpo$failure[i] > 0 then some assumption is violated, the higher the 
value (maximum 1) the more seriously.}
  \item{waic}{A list of two elements: \code{waic$waic} is the Watanabe-Akaike information criteria,  and \code{waic$p.eff} is the estimated effective number of parameters.}
  \item{mlik}{The log marginal likelihood of the model}
  \item{inla.result}{A \code{INLA} object that from function \code{runModel()} which implements INLA.}
\item{samples.fixed}{A matrix of the fixed effects samples if \code{nsample} is given.}
\item{samples.hyperpar}{A matrix of the hyperparameter samples if \code{nsample} is given.}
\item{samples.overall.Se}{A matrix containing the mean and sd (plus, possibly quantiles) of overall sensitivity samples if \code{nsample} is given.}
\item{samples.overall.Sp}{A matrix containing the mean and sd (plus, possibly quantiles) of overall specificity samples if \code{nsample} is given.}
\item{summary.overall.statistics}{A matrix containing the mean and sd (plus, possibly quantiles) of mean positive and negative likelihood ratios and mean diagnostic odds ratios if \code{nsample} is given.}
\item{samples.study.specific.Se}{A matrix containing the mean and sd (plus, possibly quantiles) of study specific sensitivity samples if \code{nsample} is given.}
\item{samples.study.specific.Sp}{A matrix containing the mean and sd (plus, possibly quantiles) of study specific specificity samples if \code{nsample} is given.}
 \item{summary.study.specific.LRpos}{A matrix containing the mean and sd (plus, possibly quantiles) of positive likelihood ratios for each study if \code{nsample} is given.}
  \item{summary.study.specific.LRneg}{A matrix containing the mean and sd (plus, possibly quantiles) of negative likelihood ratios for each study if \code{nsample} is given.}
\item{summary.study.specific.DOR}{A matrix containing the mean and sd (plus, possibly quantiles) of diagnostic odds ratios for each study if \code{nsample} is given.}
\item{summary.study.specific.RD}{A matrix containing the mean and sd (plus, possibly quantiles) of risk difference for each study if \code{nsample} is given.}
\item{summary.study.specific.LDOR}{A matrix containing the mean and sd (plus, possibly quantiles) of log diagnostic odds ratios for each study if \code{nsample} is given.}
\item{summary.study.specific.LLRpos}{A matrix containing the mean and sd (plus, possibly quantiles) of log positive likelihood ratios for each study if \code{nsample} is given.}
\item{summary.study.specific.LLRneg}{A matrix containing the mean and sd (plus, possibly quantiles) of log negative likelihood ratios for each study if \code{nsample} is given.}
}

\author{
  Jingyi Guo
}

\seealso{
  \code{makeData, makePriors, runModel, meta4diag}
}
\examples{
\dontrun{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  outdata = makeData(Catheter)
  outpriors = makePriors()
  model = runModel(outdata=outdata, outpriors=outpriors, link="logit")
  res = makeObject(outdata, outpriors, model, nsample=2000)
}
}
}