\name{meta4diag}
\alias{meta4diag}
\title{
    Function to analyse diagnostic meta-analysis with Bayesian methods using INLA.
}
\description{
    Estimate a Bayesian bivariate hierarchical model fitted within INLA. 
}
\usage{
meta4diag(data=NULL, model.type = 1, 
          var.prior = "invgamma", var2.prior = "invgamma", cor.prior = "normal",
          var.par = c(0.25, 0.025), var2.par, cor.par = c(0,5),
          wishart.par = c(4, 1, 1, 0),
          init = c(0.01,0.01,0), link="logit", quantiles=c(0.025,0.5,0.975),
          modality = NULL, covariates = NULL,
          verbose = FALSE, nsample=FALSE)
}
\arguments{
  \item{data}{
  A data frame contains at least 4 columns specifying the number of True Positive(\code{TP}), False Negative(\code{FN}), True Negative(\code{TN}) and False Positive(\code{FP}). The additional columns other than \code{studynames} will be considered as potential covariates and the name or the column number of the potential covariates can be set in the arguments \code{modality} and \code{covariates} to use them in the model.
}
  \item{model.type}{
  A numerical value specifying the model type, options are 1(default), 2, 3 and 4. \code{model.type=1} indicates that the Sensitivity(se) and Specificity(sp) will be modelled in the bivariate model, i.e. \eqn{g(se)}{g(se)} and \eqn{g(sp)}{g(sp)} are bivariate normal distributed. \code{model.type=2,3,4} indicate that the Sensitivity(se) and False Negative Rate(1-sp), False Positive Rate(1-se) and Specificity(sp), False Positive Rate(1-se) and False Negative Rate(1-sp) are modelled in the bivariate model, respectively.
}
  
 \item{var.prior}{
  A string specifying the prior density for the first variance component, options are "PC" for penalised complexity prior, "Invgamma" for inverse gamma prior, "Tnormal" for truncated normal prior, "Unif" for uniform prior which allow the standard deviation uniformaly distributed on [0,1000], "Hcauchy" for half-cauchy prior and  "table" for user specific prior. \code{var.prior} can also be set to "Invwishart" for inverse wishart prior for covariance matrix. When \code{var.prior="Invwishart"}, no matter what \code{var2.prior} and \code{cor.prior} are given, the inverse Wishart prior covariance matrix is used for covariance matrix and the \code{wishart.par} must be given. The definition of the priors is as following,
  
  \itemize{
    \item \code{var.prior="Invgamma"}: This is a prior for a variance \eqn{\sigma^2}. The inverse gamma prior has density, 
          \deqn{\pi(\sigma^2)=\frac{1}{\Gamma(a)b^a}(\sigma^2)^{-a-1}exp(-\frac{1}{b\sigma^2}),}{%
                \pi(\sigma^2)=1/\Gamma(a)b^a(\sigma^2)^{-a-1}exp(-1/b\sigma^2),}
          for \eqn{\sigma^2>0}{\sigma^2>0} where: \eqn{a>0}{a>0} is the shape parameter, \eqn{b>0}{b>0} is the rate (1/scale) parameter.
          The parameters are here \code{c(a, b)}, see arguments \code{var.par}.
  
    \item \code{var.prior="Tnormal"}: This is a prior for a variance \eqn{\sigma^2}{\sigma^2} and defined as follows. 
          The standard deviation \eqn{\sigma=\sqrt{\sigma^2}}{\sigma=\sqrt{\sigma^2}} is Gaussian distributed with mean \eqn{m}{m} and variance \eqn{v}{v} but truncated to be positive.
          The parameters are here \code{c(m, v)}, see arguments \code{var.par}.
    
    \item \code{var.prior="PC"}: This is a prior for a variance \eqn{\sigma^2}{\sigma^2} and defined as follows. 
          The left tail of the distribution of standard deviation \eqn{\sigma}{\sigma} has behavior
          \deqn{P(\sigma>u)=\alpha,}{P(\sigma>u)=\alpha,}
          which means it is unlikely that the standard deviation \eqn{\sigma}{\sigma} to be larger than a value \eqn{u}{u} with probability \eqn{\alpha}{\alpha}.
          The parameters are here \code{c(u, alpha)}, see arguments \code{var.par}.
  
    \item \code{var.prior="HCauchy"}: This is a prior for a variance \eqn{\sigma^2}{\sigma^2} and defined as follows.  
          The standard deviation \eqn{\sigma=\sqrt{\sigma^2}}{\sigma=\sqrt{\sigma^2}} is half-Cauchy distributed with density,
          \deqn{\pi(\sigma)=\frac{2\gamma}{\pi(\sigma^2+\gamma^2)},}{\pi(\sigma)=2\gamma/\pi(\sigma^2+\gamma^2),}
          where \eqn{\gamma>0}{\gamma>0} is the rate parameter.
          The parameters are here \code{c(gamma)}, see arguments \code{var.par}.
  
    \item \code{var.prior="Unif"}: This is a prior for a variance \eqn{\sigma^2}{\sigma^2} and defined as follows.  
          The standard deviation \eqn{\sigma=\sqrt{\sigma^2}}{\sigma=\sqrt{\sigma^2}} is uniform distributed on \eqn{(0,\infty)}{(0,\infty)}.
          No parameters need to be given for this prior, see arguments \code{var.par}.
    
    \item \code{var.prior="Table"}:  This is a prior for a variance \eqn{\sigma^2}{\sigma^2} and defined as follows.  
          Users have to specify a data.frame with 2 columns, 
          one indicates the values of \eqn{\sigma^2}{\sigma^2} and the other one indicates the values of \eqn{\pi(\sigma^2)}{\pi(\sigma^2)}.
          The parameters are this data frame, see arguments \code{var.par}.
    
    \item \code{var.prior="Invwishart"}: Instead of specifying separate prior distributions for the variance and correlation parameters we could also assume that the covariance matrix \eqn{\Sigma}{\Sigma} 
          \deqn{\Sigma \sim Wishart^{-1}_{p}(\nu,R),p=2,}{\Sigma \sim Wishart^{-1}_{p}(\nu,R),p=2,}
          where the Wishart distribution has density
          \deqn{\pi(\Sigma)=\frac{|R|^{\frac{\nu}{2}}}{2^{\frac{p\nu}{2}}\Gamma_{p}(\frac{\nu}{2})}|\Sigma|^{-\frac{\nu+p+1}{2}}exp(-\frac{1}{2}Trace(\frac{R}{\Sigma})), \nu>p+1,}{%
          \pi(\Sigma)=|R|^{\nu/2}/(2^{p\nu/2}\Gamma_{p}(\nu/2))|\Sigma|^{-(\nu+p+1)/2}exp(-0.5*Trace(R\Sigma^{-1})), \nu>p+1,}
          Then,
          \deqn{E(\Sigma)=\frac{R}{\nu-p-1}.}{E(\Sigma)=R/(\nu-p-1).}
          The parameters are \eqn{\nu, R_{11},R_{22} and R_{12}}{\nu, R_{11},R_{22} and R_{12}}, where
          \deqn{R=\left(\begin{array}{cc}R_{11} & R_{12} \\ R_{21} & R_{22}\end{array}\right)}{%
                R=\left(\begin{array}{cc}R_{11} & R_{12} \\ R_{21} & R_{22}\end{array}\right)}
          The parameters are here \code{c(nu, R11, R22, R12)}, see arguments \code{var.par}.
  }  
}

  \item{var2.prior}{
  See \code{var.prior}.
	%A string specifying the prior density for the second variance component, options are "PC" for penalised complexity prior, "Invgamma" for invers gamma prior, "Tnormal" for truncated normal prior, "Unif" for uniform prior, "Hcauchy" for half-cauchy prior and  "table" for user specific prior. \code{var2.prior} can also be set to "Invwishart" for wishart prior for covariance matrix. When \code{var2.prior="Invwishart"}, no matter what \code{var.prior} and \code{cor.prior} are given, the inverse wishart prior for covariance matrix is used and the \code{wishart.par} must be given. If not given, function will copy the setting for the first variance component. The defination of the priors is the same as for \code{var.prior}.
}

  \item{cor.prior}{
	A string specifying the prior density for the correlation, options are "PC" for penalised complexity prior, "Invgamma" for inverse gamma prior, "beta" for beta prior and "table" for user specific prior. \code{cor.prior} can also be set to "Invwishart" for inverse wishart prior for covariance matrix. When \code{cor.prior="Invwishart"}, no matter what \code{var.prior} and \code{var2.prior} are given, the inverse Wishart prior for covariance matrix is used and the \code{wishart.par} must be given. The definition of the priors is as following,
  
  \itemize{
    \item \code{cor.prior="Normal"}: This is a prior for a correlation \eqn{\rho}{\rho} and defined as follows. 
          The correlation parameter is constrained to \eqn{[-1, 1]}. We reparameterise the correlation parameter \eqn{\rho}{\rho} using Fisher's z-transformation as 
          \deqn{\rho^{\star}=logit(\frac{\rho+1}{2}),}{\rho^{\star}=logit((\rho+1)/2),}
          which assumes values over the whole real line and assign the following prior distribution to \eqn{\rho}{\rho},
          \deqn{\rho \sim Gaussian(\mu,\sigma^2).}{\rho \sim Gaussian(\mu,\sigma^2).}
          The prior variance of \eqn{2.5}{2.5} and prior mean of \eqn{0}{0} corresponds, roughly, to a uniform prior on \eqn{[-1,1]}{[-1,1]} for \eqn{\rho}{\rho} .
          The parameters are here \code{c(mean, variance)}, see arguments \code{cor.par}.
  
    \item \code{cor.prior="PC"}: This is a prior for a correlation \eqn{\rho}{\rho} and defined as follows. 
          The prior is defined around at a reference point with value \eqn{\rho_{0}}{\rho_{0}}. 
          To define the density behavior, three \code{strategy} can be applied. 
          The first strategy is to define the left tail behavior and the density weight on the left-hand side of the reference point \eqn{\rho_{0}}{\rho_{0}},
           \deqn{P(\rho<u_{1})=a_{1},}{P(\rho<u_{1})=a_{1},} and \deqn{P(\rho<\rho_{0})=\omega,}{P(\rho<\rho_{0})=\omega,}
           which means it is unlikely that the value of \eqn{\rho} is smaller than a small value \eqn{u_{1}} with probability \eqn{a_{1}} and 
           the probability that \eqn{\rho}{\rho} is smaller than \eqn{\rho_0}{\rho_0} is \eqn{\omega}{\omega}.
          The parameters for the first strategy are here \code{c(1, rho0, omega, u1, a1, NA, NA)}, see arguments \code{cor.par}.
          The second strategy is to define the right tail behavior and the density weight on the left-hand side of the reference point \eqn{\rho_{0}}{\rho_{0}},
          \deqn{P(\rho>u_{2})=a_{2},}{P(\rho>u_{2})=a_{2},} and \deqn{P(\rho<\rho_{0})=\omega,}{P(\rho<\rho_{0})=\omega,}
          which means it is unlikely that the value of \eqn{\rho} is larger than a big value \eqn{u_{2}} with probability \eqn{a_{2}} and 
          the probability that \eqn{\rho}{\rho} is smaller than \eqn{\rho_0}{\rho_0} is \eqn{\omega}{\omega}. 
          The parameters for the second strategy are here \code{c(2, rho0, omega, NA, NA, u2, a2)}, see arguments \code{cor.par}.
           The third strategy is to define both tail behaviors,
            \deqn{P(\rho<u_{1})=a_{1},}{P(\rho<u_{1})=a_{1},} and \deqn{P(\rho>u_{2})=a_{2}.}{P(\rho>u_{2})=a_{2}.}
            The parameters for the third strategy are here \code{c(3, rho0, NA, u1, a1, u2, a2)}, see arguments \code{cor.par}.
            The parameters of the PC prior for the correlation here is \code{c(strategy, rho0, omega, u1, a1, u2, a2)}, see arguments \code{cor.par}.
    
    \item \code{cor.prior="beta"}: This is a prior for a correlation \eqn{\rho}{\rho} and defined as follows. 
            The correlation parameter \eqn{\rho}{\rho} has a \eqn{Beta(a,b)}{Beta(a,b)} distribution scaled to have domain in \eqn{(-1, 1)}{(-1, 1)}:
            \deqn{\pi(\rho)=0.5\frac{\Gamma(a+b)}{\Gamma(a)\Gamma(b)}\rho^{a-1}(1-\rho)^{b-1}}{\pi(\rho)=0.5\Gamma(a+b)/(\Gamma(a)\Gamma(b))\rho^{a-1}(1-\rho)^{b-1}},
            where \eqn{a,b>0} are the shape parameter.
            The parameters are here \code{c(a, b)}, see arguments \code{cor.par}.
  
    \item \code{cor.prior="Table"}: This is a prior for a correlation \eqn{\rho}{\rho} and defined as follows.  
          Users have to specify the data.frame with 2 columns, one indicates the values of \eqn{\rho}{\rho} and the other one indicates the values of \eqn{\pi(\rho)}{\pi(\rho)}.
          The parameters are this data frame, see arguments \code{cor.par}.
  
    \item \code{cor.prior="Invwishart"}: Instead of specifying separate prior distributions for the hyperparameters we could also assume that the covariance matrix \eqn{\Sigma}{\Sigma} 
          \deqn{\Sigma \sim Wishart^{-1}_{p}(\nu,R),p=2,}{\Sigma \sim Wishart^{-1}_{p}(\nu,R),p=2,}
          where the inverse Wishart distribution has density
          \deqn{\pi(\Sigma)=\frac{|R|^{\frac{\nu}{2}}}{2^{\frac{p\nu}{2}}\Gamma_{p}(\frac{\nu}{2})}|\Sigma|^{-\frac{\nu+p+1}{2}}exp(-\frac{1}{2}Trace(\frac{R}{\Sigma})), \nu>p+1,}{%
          \pi(\Sigma)=|R|^{\nu/2}/(2^{p\nu/2}\Gamma_{p}(\nu/2))|\Sigma|^{-(\nu+p+1)/2}exp(-0.5*Trace(R\Sigma^{-1})), \nu>p+1,}
          Then,
          \deqn{E(\Sigma)=\frac{R}{\nu-p-1}.}{E(\Sigma)=R/(\nu-p-1).}
          The parameters are \eqn{\nu, R_{11},R_{22} and R_{12}}{\nu, R_{11},R_{22} and R_{12}}, where
          \deqn{R=\left(\begin{array}{cc}R_{11} & R_{12} \\ R_{21} & R_{22}\end{array}\right)}{%
                R=\left(\begin{array}{cc}R_{11} & R_{12} \\ R_{21} & R_{22}\end{array}\right)}
          The parameters are here \code{c(nu, R11, R22, R12)}, see arguments \code{cor.par}.
  }
  
}
  \item{var.par}{
	A numerical vector specifying the parameter of the prior density for the first variance component. 
  \itemize{
    \item \code{var.par=c(rate, shape)} when \code{var.prior="Invgamma"}.
    \item \code{var.par=c(u, alpha)} when \code{var.prior="PC"}.
    \item \code{var.par=c(m, v)} when \code{var.prior="Tnormal"}.
    \item \code{var.par=c(gamma)} when \code{var.prior="Hcauchy"}.
    \item \code{var.par=c()} when \code{var.prior="Unif"}.
    \item \code{var.par} is a data frame with 2 columns, one indicates the values of \eqn{\sigma^2}{\sigma^2} and the other one indicates the values of \eqn{\pi(\sigma^2)}{\pi(\sigma^2)} when \code{var.prior="Table"}.
    \item \code{var.par} doesn't need to be given when \code{var.prior="Invwishart"}.
  }
  See also argument \code{var.prior}.
}
  \item{var2.par}{
	A numerical vector specifying the parameter of the prior density for the second variance component. If not given, function will copy the setting for the first variance component. The definition of the priors is the same as for \code{var.par}. 
}
  \item{cor.par}{
	A numerical vector specifying the parameter of the prior density for the correlation. See also \code{examples}.
  \itemize{
    \item \code{cor.par=c(mean, variance)} when \code{cor.prior="normal"}.
    \item \code{cor.par=c(strategy, rho0, omega, u1, a1, u2, a2)} when \code{cor.prior="PC"}.
    \item \code{cor.par=c(a, b)} when \code{var.prior="beta"}.
    \item \code{cor.par} is a data frame with 2 columns, one indicates the values of \eqn{\rho}{\rho} and the other one indicates the values of \eqn{\pi(\rho)}{\pi(\rho)} when \code{cor.prior="Table"}.
    \item \code{cor.par} doesn't need to be given when \code{cor.prior="Invwishart"}.
  }
  See also argument \code{cor.prior}.
}

 \item{wishart.par}{
  A numerical vector specifying the parameter of the prior density for the covariance matrix. \code{wishart.par} must be given and \code{wishart.par=c(nu, R11, R22, R12)} when any of \code{var.prior}, \code{var2.prior} or \code{cor.prior} is \code{"Invwishart"}. See also \code{examples}.
}
  \item{init}{
  A numerical vector specifying the initial value of the first variance, the second variance and correlation.
}
   \item{link}{
  A string specifying the link function used in the model. Options are "logit", "probit" and "cloglog".
}
  \item{quantiles}{
	A vector of quantiles, p(0), p(1),... to compute for each posterior marginal. The function returns, for each posterior marginal, the values x(0), x(1),... such that \deqn{Prob(X<x)=p.}{Prob(X<x)=p.}
  The default value are c(0.025, 0.5, 0.975). Not matter what other values are going to be given, the estimates for these 3 quantiles are always returned.
}
  \item{verbose}{
	Boolean (default:FALSE) indicating whether the program should run in a verbose model.
}
\item{modality}{
  A string specifying the modality variable, which is a categorical variable, such as test threshold. Default value is NULL. See also \code{examples}. At the moment, only one categorical covariate variable can be used in the model.
}
  \item{covariates}{
  A vector, which could be either the name of columns or the number of columns, specifying the continuous covariates variables, such as disease prevalence or average individual patients status of each study. Default value is NULL. See also \code{examples}.
}
  \item{nsample}{
    A numerical value specifying the number of posterior samples, default is 5000. The posterior samples are used to compute the marginals and estimates values of non-linear functions, such as log radios and diagnostic odds radios. If \code{nsample} is given, \code{summary.summarized.statistics}, \code{summary.fitted.LRpos}, \code{summary.fitted.LRneg}, \code{summary.fitted.DOR} and samples of \eqn{E(se)}, \eqn{E(sp)}, \eqn{E(1-se)} and \eqn{E(1-sp)} will be returned.
  }
}

\details{
The bivariate model has two levels, in the first level, the observed number of individuals in a specific group in a 2 by 2 table is binomial distributed, for example, the numbers of individuals in the group of true positive and true negative of a study \eqn{i} are modelled jointly,
    \deqn{TP_{i} | Se_{i} \sim Binomial(TP_{i}+ FN_{i},Se_{i})}
    \deqn{TN_{i} | Sp_{i} \sim Binomial(TN_{i}+ FP_{i},Sp_{i})}
In the second level, two transformed accuracies with some link function (see argument \code{link}) are bivariate Gaussian distributed, continuous with the previous example,
    \deqn{g(Se_{i}) = \mu + V_{i}\alpha + \phi_{i}}
    \deqn{g(Sp_{i}) = \nu + U_{i}\beta + \psi_{i}}
where \eqn{\phi_{i}} and \eqn{\psi_{i}} are bivariate Gaussian distributed with mean 0 and covariance matrix \eqn{\Sigma}. The \eqn{se} and \eqn{sp} in the example could be changed to \eqn{se} and \eqn{(1-sp)}, \eqn{(1-se)} and \eqn{sp} or \eqn{(1-se)} and \eqn{(1-sp)}, see argument \code{model.type}.

The function \code{meta4diag()} depends on four internal functions which are also given in the \pkg{meta4diag} package in order to flexibly implement a list of dataset with the same prior setting. The four internal functions are \code{makeData()}, \code{makePriors()}, \code{runModel()} and \code{makeObject()}. Details can be seen the corresponding documentations and examples.

After running the function \code{meta4diag()}, a \code{meta4diag} object will be returned which contains various estimated results for later analysis, such as the posterior marginals, estimated value, standard deviations and the coresponding quaniles of the accuracies. See \code{Values}.
}

\value{
	\code{meta4diag} returns a \code{meta4diag} object with components: 
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
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  meta4diag(data = Catheter, model.type = 1, var.prior = "invgamma", cor.prior = "normal", 
    var.par = c(0.25, 0.025), cor.par = c(0, 5), init = c(0.01, 0.01, 0), 
    link = "logit", quantiles = c(0.025, 0.5, 0.975), verbose = FALSE, covariates = NULL, 
    nsample = FALSE)
}
}