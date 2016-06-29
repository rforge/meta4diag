\name{makePriors}
\alias{makePriors}
\title{Priors for internal use.}
\description{
	Prepare the given prior settings for internal use in \pkg{INLA}. The input of the function \code{makePriors} are prior settings for variances and correlation or the full covariance matrix, these are then transformed to the internal parameterisation that \pkg{INLA} requires. This function is used internally in the main function \code{meta4diag()} but can also be used as a separate function.
}
\usage{
makePriors(var.prior = "PC", var2.prior="PC", cor.prior = "PC",
  var.par = c(3, 0.05), var2.par, cor.par = c(1,-0.1,0.5,-0.95,0.05,0.95,0.05),
  wishart.par = c(4,1,1,0), init = c(0.01, 0.01, -0.1))
}

\arguments{
  \item{var.prior}{
	A string specifying the prior density for the first variance component, options are "PC" for penalised complexity prior, "Invgamma" for inverse gamma prior, "Tnormal" for truncated normal prior, "Unif" for uniform prior which allow the standard deviation uniformaly distributed on [0,1000], "Hcauchy" for half-cauchy prior and  "table" for user specific prior. \code{var.prior} can also be set to "Invwishart" for inverse wishart prior for covariance matrix. When \code{var.prior="Invwishart"}, no matter what \code{var2.prior} and \code{cor.prior} are given, the inverse Wishart prior covariance matrix is used for covariance matrix and the \code{wishart.par} must be given. Of note, the values of this argument is not case sensitive. The definition of the priors is as following,
  
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
  
    \item \code{var.prior="Hcauchy"}: This is a prior for a variance \eqn{\sigma^2}{\sigma^2} and defined as follows.  
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
	A string specifying the prior density for the correlation, options are "PC" for penalised complexity prior, "Invgamma" for inverse gamma prior, "Beta" for beta prior and "table" for user specific prior. \code{cor.prior} can also be set to "Invwishart" for inverse wishart prior for covariance matrix. When \code{cor.prior="Invwishart"}, no matter what \code{var.prior} and \code{var2.prior} are given, the inverse Wishart prior for covariance matrix is used and the \code{wishart.par} must be given. Of note, the values of this argument is not case sensitive. The definition of the priors is as following,
  
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
    
    \item \code{cor.prior="Beta"}: This is a prior for a correlation \eqn{\rho}{\rho} and defined as follows. 
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
}

\value{
	A list of prior settings with the components:
  \item{prec1}{a list of prior settings for the first log precision (the log inverse of the first variance in the model).}
  \item{prec2}{a list of prior settings for the second log precision (the log inverse of the second variance in the model).}
  \item{cor}{a list of prior settings for the transformed correlation (some functions of correlation in the model).}
  \item{lambdas}{a vector of rate parameters for the PC correlation if \code{cor.prior="PC"}.}
  \item{density}{a list of prior densities for precisions and correlations.}
  \item{original.setting}{a list of input prior settings.}
  \item{wishart.flag}{Boolean indicating whether a inverse Wishart prior is setting or not.}
}
\references{
Simpson DP, Martins TG, Riebler A, Fuglstad GA, Rue H, Sorbye SH (2014) Penalised Model Component Complexity: A principled, Practical Approach to Constructing Priors. Arxiv e-prints. 1403.4630  
}
\author{
  Jingyi Guo and Andrea Riebler 
}

\examples{
\dontrun{
if(requireNamespace("INLA", quietly = TRUE)){
require("INLA", quietly = TRUE)
makePriors(var.prior = "PC", cor.prior = "PC", var.par = c(3, 0.05), 
  cor.par = c(1, -0.1, 0.5, -0.95, 0.05, NA, NA))
makePriors(var.prior = "PC", cor.prior = "PC", var.par = c(3, 0.05), 
  cor.par = c(2, -0.1, 0.5, NA, NA, 0.95, 0.05))
makePriors(var.prior = "PC", cor.prior = "PC", var.par = c(3, 0.05), 
  cor.par = c(3, -0.1, NA, -0.95, 0.05, 0.95, 0.05))
makePriors(var.prior = "invgamma", cor.prior = "normal", 
  var.par = c(0.25, 0.025), cor.par = c(0, 5))
makePriors(var.prior = "invwishart", wishart.par=c(4,1,2,0.1))
}
}
}