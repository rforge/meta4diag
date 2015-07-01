\name{Scheidler}
\alias{Scheidler}
\docType{data}
\title{Scheidler dataset.}
\description{
  The dataset is used to compare the utility of three types of diagnostic imaging to detect lymph node metastases in patients with cervical cancer: lymphangiography (LAG), computed tomography (CT) and magnetic resonance (MR).
}
\usage{data(Scheidler)}
\format{
  A data frame with 44 rows and 6 columns. Each row provides data for one study. The columns are "studynames" indicates the name of studies, "modality" indicates which diagnostic study that the observation is conducted from and is considered as a covariate, "TP", "FP", "FN" and "TN" are the number of true positive, false positive, false negative and true negative, respectively.
}
%%\details{}
\references{Scheidler J, Hricak H, Kyle K Y, et al. Radiological evaluation of lymph node metastases in patients with cervical cancer: a meta-analysis. Jama, 1997, 278(13): 1096-1101.}

\examples{
  data(Scheidler)
}
\keyword{datasets}