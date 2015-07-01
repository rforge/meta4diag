\name{makeObject}
\alias{makeObject}
\title{
  A function used to make a \code{meta4diag} object.
}
\description{
  Takes an internal data list, an internal prior setting list and an \code{INLA} object produced by \code{makeData()}, \code{makePriors()} and \code{runModel()}, respectively and makes a \code{meta4diag} object which contains various informations for later use. This function is used in the main function \code{meta4diag()} and can also be used as a separate function.
}
\usage{
  makeObject(outdata, outpriors, model, nsample=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outdata}{
    A list of the internal data that can be used in \pkg{INLA} and the original data as obtained by the function \code{makeData()}.
  }
  \item{outpriors}{
    A list of prior settings as obtained by the function \code{makePriors()}. Contains the internal setting, priors of precisions and transfomed correlation that are used in \pkg{INLA}.
  }
  \item{model}{
    An \code{INLA} object. Get from function \code{runModel()}.
  }
  \item{nsample}{
    A numerical value specifying the number of posterior samples, default is FALSE. The posterior samples are used to compute the marginals and estimates values of non-linear functions, such as log ratios and diagnostic odds ratios. If \code{nsample} is given, \code{summary.summarized.statistics}, \code{summary.fitted.LRpos}, \code{summary.fitted.LRneg}, \code{summary.fitted.DOR} and samples of \eqn{E(se)}, \eqn{E(sp)}, \eqn{E(1-se)} and \eqn{E(1-sp)} will be returned.
  }
}


\value{
  \code{makeObject} returns a \code{meta4diag} object with components: 
  \item{data}{The provided input data.}
  \item{outdata}{The internal data that could be used in \pkg{INLA} from function \code{makeData()}.}
  \item{priors.density}{Prior distributions for the variance components and correlation from function \code{makePriors()}.}
  \item{names.fitted}{Names of the jointly modelled accuracies in the model. For example, se and sp or (1-se) and sp.} 
  \item{names.transf.fitted}{Names of transformed accuracies. If se and sp are jointly estimated in model, then \code{names.transf.fitted=c("(1-se)","(1-sp"))}.}
  \item{cpu.used}{The cpu time used for running the model.}
  \item{call}{The matched call.}
  \item{summary.fixed}{Matrix containing the mean and standard deviation (plus, possibly quantiles) of the fixed effects of the model.}
  \item{marginals.fixed}{A list containing the posterior marginal densities of the fixed effects of the model.}
  \item{summary.expected.gtransformed.accuracy}{Matrix containing the mean and standard deviation (plus, possibly quantiles) of the mean of accuracies transformed with the link function, i.e. E(g(Se)), E(g(Sp)), E(g(1-Se)) and E(g(1-Sp)).}
  \item{marginals.expected.gtransformed.accuracy}{A list containing the posterior marginal densities of the mean of accuracies transformed with the link function, i.e. E(g(Se)), E(g(Sp)), E(g(1-Se)) and E(g(1-Sp)).}
  \item{summary.expected.accuracy}{Matrix containing the mean and standard deviation (plus, possibly quantiles) of the mean of the accuracies, i.e.  E(Se), E(Sp), E(1-Se) and E(1-Sp).}
  \item{marginals.expected.accuracy}{A list containing the posterior marginal densities of of the mean of the accuracies, i.e.  E(Se), E(Sp), E(1-Se) and E(1-Sp).}
  \item{summary.hyperpar}{A matrix containing the mean and sd (plus, possibly quantiles) of the hyperparameters of the model.}
  \item{marginals.hyperpar}{A list containing the posterior marginal densities of the hyperparameters of the model.} 
  \item{correlation.linear.comb}{A correlation matrix between the mean of the accuracies transformed with the link function.} 
  \item{covariance.linear.comb}{A covariance matrix between the mean of the accuracies transformed with the link function.}
  \item{summary.predict.(...)}{A matrix containing the mean and sd (plus, possibly quantiles) of the linear predictors one transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g(se)} and \eqn{g(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
  \item{marginals.predict.(...)}{A list containing the posterior marginals of the linear predictors of one transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g(se)} and \eqn{g(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
  \item{summary.fitted.(...)}{A matrix containing the mean and sd (plus, possibly quantiles) of the linear predictors one back-transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g^{-1}(se)} and \eqn{g^{-1}(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
  \item{marginals.fitted.(...)}{A list containing the posterior marginals of the linear predictors of one back-transformed accuracy in the model. The accuracy type depends on the model type. See argument \code{model.type}. For example, the possible accuracy type could be \eqn{g^{-1}(se)} and \eqn{g^{-1}(sp)} when \code{model.type}=1, where \eqn{g()} is the link function.}
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
  \item{summary.summarized.statistics}{A matrix containing the mean and sd (plus, possibly quantiles) of mean positive and negative likelihood ratios and mean diagnostic odds ratios if \code{nsample} is given.}
 \item{summary.fitted.LRpos}{A matrix containing the mean and sd (plus, possibly quantiles) of fitted positive likelihood ratios for each study if \code{nsample} is given.}
  \item{summary.fitted.LRneg}{A matrix containing the mean and sd (plus, possibly quantiles) of fitted negative likelihood ratios for each study if \code{nsample} is given.}
\item{summary.fitted.DOR}{A matrix containing the mean and sd (plus, possibly quantiles) of fitted diagnostic odds ratios for each study if \code{nsample} is given.}
 \item{mean(Se).samples}{A vector of mean sensitivity samples if \code{nsample} is given.}
 \item{mean(Sp).samples}{A vector of mean specificity samples if \code{nsample} is given.}
 \item{mean(1-Se).samples}{A vector of mean fnr samples if \code{nsample} is given.}
 \item{mean(1-Sp).samples}{A vector of mean fpr samples if \code{nsample} is given.}
}

\author{
  Jingyi Guo
}

\seealso{
  \code{makeData, makePriors, runModel, meta4diag}
}
\examples{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  outdata = makeData(Catheter)
  outpriors = makePriors()
  model = runModel(outdata=outdata, outpriors=outpriors, link="logit")
  res = makeObject(outdata, outpriors, model, nsample=2000)
}
}