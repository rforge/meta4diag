\name{AUC}
\alias{AUC}
\alias{AUC.meta4diag}
\title{
  A function that calculate the area under summary receiver operating chracteristic line.
}
\description{
Takes a \code{meta4diag} object produced by \code{meta4diag()} and calculate the area under summary receiver operating chracteristic line. 
}
\usage{
\method{AUC}{meta4diag}(x, sroc.type=1, est.type="median", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{meta4diag} object.}
  \item{est.type}{The estimates type used to make SROC plot. Options are "mean" and "median".}
  \item{sroc.type}{
  A numerical value specifying the function used to make SROC line. Options are 1, 2, 3, 4, 5. When \code{sroc.type=1}, the SROC line is plotted as " The regression line 1" according to Arends et al.(2008),
  \deqn{y = \mu + \rho\sqrt{\frac{\sigma_{\mu}^2}{\sigma_{\nu}^2}}(x-\nu)}{y = \mu + \rho\sqrt{\sigma_{\mu}^2/\sigma_{\nu}^2}(x-\nu)}
  When \code{sroc.type=2}, the SROC line is plotted as "The major axis method",
  \deqn{y = \frac{\sigma_{\mu}^2-\sigma_{\nu}^2\pm\sqrt{(\sigma_{\mu}^2-\sigma_{\nu}^2)^2+4\rho^2\sigma_{\mu}^2\sigma_{\nu}^2}}{2\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2}}(x-\nu)+\mu}{y = (\sigma_{\mu}^2-\sigma_{\nu}^2\pm\sqrt{(\sigma_{\mu}^2-\sigma_{\nu}^2)^2+4\rho^2\sigma_{\mu}^2\sigma_{\nu}^2})/(2\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2})(x-\nu)+\mu}
  When \code{sroc.type=3}, the SROC line is plotted as "The Moses and Littenberg's regression line",
  \deqn{y = \frac{\sigma_{\mu}^2 + \rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2}}{\sigma_{\nu}^2+\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2}}(x-\nu)+\mu}{y = (\sigma_{\mu}^2 + \rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2})/(\sigma_{\nu}^2+\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2})(x-\nu)+\mu}
  When \code{sroc.type=4}, the SROC line is plotted as "The regression line 2",
  \deqn{y = \mu + \frac{1}{\rho}\sqrt(\frac{\sigma_{\mu}^2}{\sigma_{\nu}^2})(x-\nu)}{y = \mu + 1/\rho\sqrt(\sigma_{\mu}^2/\sigma_{\nu}^2)(x-\nu)}
  When \code{sroc.type=5}, the SROC line is plotted as "The Rutter and Gatsonis's SROC curve",
  \deqn{y = \mu + \sqrt{\frac{\sigma_{\mu}^2}{\sigma_{\nu}^2}}(x-\nu)}{y = \mu + \sqrt{\sigma_{\mu}^2/\sigma_{\nu}^2}(x-\nu)}
}
\item{\dots}{
    Arguments to be passes to methods.
  }
}

\details{
The \code{AUC()} returns an estimate of the area under the SROC curve. When the number of samples is given in the \code{makeObject()} or \code{meta4diag()}, the uncertainty of AUC is estimated. Otherwise only the estimates of the AUC respect to the estimated SROC curve is returned.
}

\value{A vector containing the "est", which indicate the estimates of AUC of the estimated SROC vurve and "mean" and "sd" (plus, possibly quantiles) of AUC if \code{nsample} is given.
}
\author{
  Jingyi Guo and Andrea Riebler 
}

\references{
  Chappell F, Raab G, Wardlaw J (2009). "When are Summary ROC Curves Appropriate for Diagnostic Meta-analyses?" Statistics in Medicine, 28(21), 2653-2668. 
  
  Arends et al.(2008) "Bivariate random effects meta-analysis of ROC curves." Medical Decision Making, 28(5), 621-638.
}

\seealso{
  \code{SROC, meta4diag}
}
\examples{
\dontrun{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter, nsample=1000)
  AUC(res, est.type="mean")
}
}
}