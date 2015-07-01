\name{fitted}
\alias{fitted}
\alias{fitted.meta4diag}

\title{Get the estimated values of accuracies for each study.}
\description{
   Takes a \code{meta4diag} object produced by \code{meta4diag()} and returns the estimated study specified accuracies, such as sensitivity, specificity, odds ratios and likelihood ratios for each study.
 }
\usage{
\method{fitted}{meta4diag}(object, accuracy.type = "sens",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A \code{meta4diag} object.}
  \item{accuracy.type}{
    A string specifying the accuracy type. Options are "sens", "TPR", "spec", "TNR", "FPR", "FNR", "LRpos", "LRneg", "RD", "LLRpos", "LLRneg", "LDOR", and "DOR".
   \itemize{
      \item \code{"sens" and "TPR"}: The true positive rate, known as sensitivity, \eqn{sens = \frac{TP}{TP+FN}}{sens = TP/(TP+FN)}.
      \item \code{"spec" and "TNR"}: The true negative rate, known as specificity, \eqn{spec = \frac{TN}{TN+FP}}{spec = TN/(TN+FP)}.
      \item \code{"FPR"}: The false positive rate, \eqn{FPR = \frac{FP}{FP+TN}}{FPR = FP/(FP+TN)}.
      \item \code{"FNR"}: The false negative rate, \eqn{FNR = \frac{FN}{FN+TP}}{FNR = FN/(FN+TP)}.
      \item \code{"LRpos"}: The positive likelihood ratio, \eqn{LRpos = \frac{sens}{1-spec}}{LRpos = sens/(1-spec)}.
      \item \code{"LRneg"}: The negative likelihood ratio, \eqn{LRneg = \frac{1-sens}{spec}}{LRneg = (1-sens)/spec}.
      \item \code{"RD"}: The risk difference, \eqn{RD = sens-(1-spec)}{RD = sens-(1-spec)}.
      \item \code{"DOR"}: The diagnostic odds ratio, \eqn{DOR = \frac{LRpos}{LRneg}}{DOR = LRpos/LRneg}.
      \item \code{"LLRpos"}: The log positive likelihood ratio, \eqn{LLRpos = log(\frac{sens}{1-spec})}{LLRpos = log(sens/(1-spec))}.
      \item \code{"LLRneg"}: The log negative likelihood ratio, \eqn{LLRneg = log(\frac{1-sens}{spec})}{LLRneg = log((1-sens)/spec)}.
      \item \code{"LDOR"}: The log diagnostic odds ratio, \eqn{LDOR = log(\frac{LRpos}{LRneg})}{LDOR = log(LRpos/LRneg)}.
    }
          
  }
  \item{\dots}{
    Arguments to be passes to methods.
  }
}

\value{
  A data frame with the estimated mean, standard deviation and the corresponding quantiles, which are specified by user when the user runs the main function \code{meta4diag()}, of the accuracies of each sudy.
}

\author{
  Jingyi Guo
}

\examples{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter, nsample=2000)
  fitted(res, accuracy.type="sens")
  fitted(res, accuracy.type="DOR")
}
}