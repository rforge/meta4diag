\name{table_cor}
\alias{table_cor}
\docType{data}
\title{The example table prior for correlation.}
\description{
  An example of a table prior density for the correlation parameter.
}
\usage{data(table_cor)}
\format{
  A data frame with 2 columns named "x" and "y". The first column indicates the value of correlation points, usually in [-1,1]. The second column indicates the density of the corresponding points.
}

\examples{
  data(table_cor)
  plot(table_cor)
}
\keyword{datasets}