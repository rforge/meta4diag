\name{meta4diag-package}
\alias{meta4diag-package}
\docType{package}
\title{
  Meta-Analysis for Diagnostic Test Studies
}
\description{
  Bayesian inference analysis for bivariate meta-analysis of diagnostic test studies using integrated nested Laplace approximation with INLA.
}
\details{
  \tabular{ll}{
    Package: \tab meta4diag\cr
    Type: \tab Package\cr
    Version: \tab 2.0.7\cr
    Date: \tab 2018-02-14\cr
    License: \tab GPL-2 \cr
    LazyLoad: \tab yes\cr
  }
  
  The meta4diag package provides tools to implement Bayesian bivariate
meta-analyses of diagnostic test studies. meta4diag is a purpose-built front
end of the R package INLA (Rue H., Martino S, and Chopin N. 2009). It allows the user a straightforward model specification and offers user-specific
prior distributions. Further, the newly proposed penalized complexity prior framework (Simpson et al. 2014) is supported, which builds on prior intuitions about the behaviors of the variance and correlation parameters (Guo, J., Riebler, A. and Rue H. 2017). Accurate posterior marginal distributions for sensitivity and specificity as well as all hyperparameters, and covariates are directly obtained without Markov chain Monte Carlo sampling. Further, univariate estimates of interest, such as odds ratios, as well as the summary receiver operating characteristic (SROC) curve and other common graphics are directly available for interpretation. An interactive graphical user interface provides the user with the full functionality of the package without requiring any R programming.
}

\author{
  Jingyi Guo and Andrea Riebler
}

\references{
  \cite{Rue H., Martino S, and Chopin N. (2009). Approximate
  Bayesian Inference for Latent Gaussian Models Using Integrated Nested
  Laplace Approximations. \emph{Journal of the Royal Statistical Society B}
  \bold{71}: 319--392. (www.r-inla.org)}

\cite{Simpson DP, Martins TG, Riebler A, Fuglstad GA, Rue H, Sorbye SH (2014) Penalised Model Component Complexity: A principled, Practical Approach to Constructing Priors. \emph{Arxiv e-prints. 1403.4630}}

  \cite{Guo, J., Riebler, A. and Rue H. (2017) Bayesian bivariate meta-analysis of diagnostic test studies with interpretable priors. \emph{Statistics in Medicine}
    \bold{36(19)}: 3039--3058.}
  
  \cite{Guo, J. and Riebler, A. (2018) meta4diag: Bayesian Bivariate Meta-Analysis of Diagnostic Test Studies for Routine Practice. \emph{Journal of Statistical Software}
    \bold{83(1)}: 1--31.}
  
}