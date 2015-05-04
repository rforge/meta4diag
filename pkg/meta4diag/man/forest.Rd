\name{forest}
\alias{forest}
\alias{forest.grid}
\alias{forest.grid.meta4diag}
\alias{forest.meta4diag}

\title{
 Forest plot.
}
\description{
Takes am \code{meta4diag} object produced by \code{meta4diag()} and draws a forest plot together with a table of text. A forest plot is a graphical representation of a meta-analysis. It is usually accompanied by a table listing references (author and date) of the studies included in the meta-analysis. In the main device each line represents one study in the meta-analysis, plotted according to the estimated result. The estimated accuracy for each study is plotted as points in the middle and credible intervals as arrows. The size of the estimated accuracy for each study can be chosen to be propotional to the number of individuals in each study. A diamond indicates the overall summary point.
}
\usage{
\method{forest}{meta4diag}(x, accuracy.type = "sens", est.type = "mean", 
  p.cex = "scaled", p.pch = 15, p.col = "black", 
  nameShow = "right", dataShow = "center", ciShow = "left", cex = 1, 
  shade.col = "gray", arrow.col = "black", arrow.lty = 1, arrow.lwd = 1, 
  main = "Forest Plot", main.cex = 1.5,...)
\method{forest.grid}{meta4diag}(x, accuracy.type = "sens", est.type = "mean", 
nameShow = T, dataShow = F, ciShow = T, graphwidth = 1, main, xlab = "",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A \code{meta4diag} object, getting from running the main function \code{meta4diag()}.
}
  \item{accuracy.type}{
  A string specifying the accuracy type. Options are "sens", "TPR", "spec", "TNR", "FPR", "FNR", "LRpos", "LRneg" and "DOR".
}
  \item{est.type}{
  The estimates type used to make SROC plot. Options are "mean" and "median".
}
  \item{p.cex}{
  Points size of study specific estimate.
}
  \item{p.pch}{
  Points symbol of study specific estimate..
}
  \item{p.col}{
  Points color of study specific estimate..
}
  \item{nameShow}{
  Boolean indicating whether the study names is shown or not. Can also be a string indicating the position to show the study names. Options are "left", "center" and "right".
}
  \item{dataShow}{
  Boolean indicating whether the original data is shown or not. Can also be a string indicating the position to show the original data. Options are "left", "center" and "right".
}
  \item{ciShow}{
  Boolean indicating whether the credible intervals is shown or not. Can also be a string indicating the position to show the values of credible intervals. Options are "left", "center" and "right".
}
  \item{cex}{
  The magnification to be used for the table text.
}
  \item{shade.col}{
  Color of shaded place. 
}
  \item{arrow.col}{
  Arrow color.
}
  \item{arrow.lty}{
  Arrow line style.
}
  \item{arrow.lwd}{
  Arrow line width.
}
  \item{main}{
  A overall title for the plot.
}
  \item{main.cex}{
  Title size.
}
\item{graphwidth}{
  A numerical value indicating the main plot device size.
}
  \item{xlab}{
  A title for the x axis.
}
  \item{...}{
  Arguments to be passed to methods.
}
}

\details{
Estimated accuracies and the correspoing credible intervals for each study are plotted. Together shown in the plot with the study names, the original dataset and the values of accuracies and the credible intervals.

The function \code{forest()} gives the user flexibility to control the plots.
}

\value{Besides plotting, the function returns an invisible NULL.}

\author{
  Jingyi Guo and Andrea Riebler 
}

\examples{
data(Catheter)
res <- meta4diag(data = Catheter)
forest(res, accuracy.type = "sens")
}