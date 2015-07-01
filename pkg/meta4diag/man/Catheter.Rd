\name{Catheter}
\alias{Catheter}
\docType{data}
\title{The Catheter Segment Culture data.}
\description{
  The Cathether dataset is a collection of studies which studied semi-quantitative (19 studies) and quantitative (14 studies) catheter segment culture for the diagnosis of intravascular device-related blood stream infection.
}
\usage{data(Catheter)}
\format{
  A data frame with 33 rows and 8 columns. Each row provides data for one study. The columns are "studynames", which indicates the name of studies; "type", which indicates the study quality; "prevalence", which denotes the study disease prevalence and is considered as a continuous covariate here; "TP", "FP", "TN" and "FN", which are the observations of this dataset and must be given. 
}

\references{Chu, Haitao, Hongfei Guo, and Yijie Zhou. "Bivariate Random Effects Meta-Analysis of Diagnostic Studies Using Generalized Linear Mixed Models." Medical Decision Making 30.4 (2010): 499-508.}

\examples{
  data(Catheter)
}
\keyword{datasets}