\name{makeData}
\alias{makeData}
\title{Make internal used data structure.}
\description{
	Make given dataset for internal use. This function is used within the main function \code{meta4diag()} and can also be used as a separate function.
}
\usage{
makeData(data = NULL, model.type = 1, modality = NULL, covariates = NULL)
}

\arguments{
  \item{data}{
	A data frame contains at least 4 columns specifying the number of True Positive(\code{TP}), False Negative(\code{FN}), True Negative(\code{TN}) and False Positive(\code{FP}). The additional columns other than \code{studynames} and \code{modality} will be considered as potential covariates and the name or the column number of the potential covariates can be set in the argument \code{covariates} to make it to be covariates that will be used in model.
}
  \item{model.type}{
	A numerical value specifying the model type, options are 1(default), 2, 3 and 4. \code{model.type=1} indicates that the Sensitivity(se) and Specificity(sp) will be modelled in the bivariate model, i.e. \eqn{g(se)}{g(se)} and \eqn{g(sp)}{g(sp)} are bivariate normal distributed. \code{model.type=2,3,4} indicate that the Sensitivity(se) and False Negative Rate(1-sp), False Positive Rate(1-se) and Specificity(sp), False Positive Rate(1-se) and False Negative Rate(1-sp) are modelled in the bivariate model, respectively.
}
  \item{modality}{
  A vector specifying the modality variables, which is a categorical variable, such as test threshold. Default value is NULL. See also \code{examples}.
}
  \item{covariates}{
	A vector specifying the continuous covariates variables, such as disease prevalence or average individual patients status oc each study. Default value is NULL. See also \code{examples}.
}

}

\value{
	A constructed list with components:
  \item{internaldata}{a data frame used in \pkg{INLA}.}
  \item{originaldata}{a data frame which is the input.}
  \item{covariates.setting}{a vecter specified the setting of covariates, if \code{covariates} is given.}
  \item{modality.setting}{a vecter specified the setting of modality, if \code{modality} is given.}
  \item{model.type}{a value specified the model type.}
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