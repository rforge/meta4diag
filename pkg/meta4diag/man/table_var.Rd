\name{table_var}
\alias{table_var}
\docType{data}
\title{The example table prior for variance.}
\description{
  An example of a table prior density for the variance parameter.
}
\usage{data(table_var)}
\format{
  A data frame with 2 columns named "x" and "y". The first column indicates the value of variance points, usually in [0, some positive value]. The second column indicates the density of the corresponding points.
}

\examples{
  data(table_var)
  plot(table_var)
}
\keyword{datasets}