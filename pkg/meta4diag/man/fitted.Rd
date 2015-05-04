\name{fitted}
\alias{fitted.meta4diag}

\title{Get the estimated values of accuracies for each study.}
\description{
   Takes am \code{meta4diag} object produced by \code{meta4diag()} and returns the estimated accuracy, such as sensitivity, specificity, odds ratios and likelihood ratios for each study.
 }
\usage{
\method{fitted}{meta4diag}(object, accuracy.type = "sens",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A \code{meta4diag} object.}
  \item{accuracy.type}{
    A string specifying the accuracy type. Options are "sens", "TPR", "spec", "TNR", "FPR", "FNR", "LRpos", "LRneg" and "DOR".
  }
  \item{\dots}{
    ...
  }
}

\value{
  A dataframe with the estimated mean, standard deviation and the corresponding quantiles of the accuracies of each sudy.
}

\author{
  Jingyi Guo and Andrea Riebler 
}

\examples{
  data(Catheter)
  res <- meta4diag(data = Catheter, nsample=2000)
  fitted(res, accuracy.type="sens")
  fitted(res, accuracy.type="DOR")
}