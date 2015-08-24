\name{makeData}
\alias{makeData}
\title{Make internally used data structure.}
\description{
	Transform the format of the input dataset into internal use format. This function is used within the main function \code{meta4diag()} and can also be used as a separate function.
}
\usage{
makeData(data = NULL, model.type = 1, modality = NULL, covariates = NULL)
}

\arguments{
  \item{data}{
	A data frame contains at least 4 columns specifying the number of True Positive(\code{TP}), False Negative(\code{FN}), True Negative(\code{TN}) and False Positive(\code{FP}). The additional columns other than \code{studynames} will be considered as potential covariates and the name or the column number of the potential covariates can be set in the arguments \code{modality} and \code{covariates} to use them in the model.
}
  \item{model.type}{
	A numerical value specifying the model type, options are 1(default), 2, 3 and 4. \code{model.type=1} indicates that the Sensitivity(se) and Specificity(sp) will be modelled in the bivariate model, i.e. \eqn{g(se)}{g(se)} and \eqn{g(sp)}{g(sp)} are bivariate normal distributed. \code{model.type=2,3,4} indicate that the Sensitivity(se) and False Negative Rate(1-sp), False Positive Rate(1-se) and Specificity(sp), False Positive Rate(1-se) and False Negative Rate(1-sp) are modelled in the bivariate model, respectively.
}
  \item{modality}{
  A string specifying the modality variable, which is a categorical variable, such as test threshold. Default value is NULL. See also \code{examples}.
}
  \item{covariates}{
	A vector specifying the continuous covariates variables, such as disease prevalence or average individual patients status of each study. Default value is NULL. See also \code{examples}.
}

}

\value{
	A constructed list with components:
  \item{internaldata}{a data frame used as internal data in \pkg{INLA}.}
  \item{originaldata}{a data frame which is equal to the provided input data.}
  \item{covariates.setting}{a vector specified the setting of covariates, if \code{covariates} is given.}
  \item{modality.setting}{a vector specified the setting of modality, if \code{modality} is given.}
  \item{model.type}{a value specified the model type.}
}

\author{
  Jingyi Guo and Andrea Riebler 
}

\examples{
\dontrun{
data(Catheter)

a = makeData(data=Catheter,model.type=1,modality="type")
b = makeData(data=Catheter,model.type=1,modality=2)
c = makeData(data=Catheter,model.type=1,covariates="prevalence")
d = makeData(data=Catheter,model.type=1,covariates=3)
e = makeData(data=Catheter,model.type=1,modality="type",covariates="prevalence")
}
}