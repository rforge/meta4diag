\name{makeData}
\alias{makeData}
\title{Make internal used data structure.}
\description{
	Make given dataset for internal use. This function is used within the main function \code{meta4diag()} and can also be used as a separate function.
}
\usage{
makeData(data = NULL, model.type = 1, covariates = NULL)
}

\arguments{
  \item{data}{
	A data frame contains at least 4 columns specifying the number of True Positive(\code{TP}), False Negative(\code{FN}), True Negative(\code{TN}) and False Positive(\code{FP}). The additional columns other than \code{studynames} and \code{modality} will be considered as potential covariates and the name or the column number of the potential covariates can be set in the argument \code{covariates} to make it to be covariates that will be used in model.
}
  \item{model.type}{
	A numerical value specifying the model type, options are 1(default), 2, 3 and 4. \code{model.type=1} indicates that the Sensitivity(se) and Specificity(sp) will be modelled in the bivariate model, i.e. \eqn{g(se)}{g(se)} and \eqn{g(sp)}{g(sp)} are bivariate normal distributed. \code{model.type=2,3,4} indicate that the Sensitivity(se) and False Negative Rate(1-sp), False Positive Rate(1-se) and Specificity(sp), False Positive Rate(1-se) and False Negative Rate(1-sp) are modelled in the bivariate model, respectively.
}
  \item{covariates}{
	A vector specifying the covariates. Default value is NULL. See also \code{examples}.
}
}

\value{
	A constructed list with components:
  \item{internaldata}{a data frame used in \pkg{INLA}.}
  \item{originaldata}{a data frame which is the input.}
}

\author{
  Jingyi Guo and Andrea Riebler 
}

\examples{
data(Catheter)

makeData(data=Catheter,model.type=1,covariates="type")
makeData(data=Catheter,model.type=1,covariates=2)
makeData(data=Catheter,model.type=1,covariates=c("type","term"))
makeData(data=Catheter,model.type=1,covariates=c(2,3))
}