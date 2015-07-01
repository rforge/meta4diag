\name{forest}
\alias{forest}
\alias{forest.grid}
\alias{forest.grid.meta4diag}
\alias{forest.meta4diag}

\title{
 Forest plot.
}
\description{
Takes a \code{meta4diag} object produced by the function \code{meta4diag()} and draws a forest plot. 
}
\usage{
\method{forest}{meta4diag}(x, accuracy.type="sens", est.type="mean", p.cex="scaled", p.pch=15, p.col="black",
    nameShow="right", dataShow="center", ciShow="left", text.cex=1,
    shade.col="gray", arrow.col="black", arrow.lty=1, arrow.lwd=1,
    cut=c(0,1), intervals=c(0.025,0.975),
    main="Forest plot", main.cex=1.5, axis.cex=1,...)
\method{forest.grid}{meta4diag}(x, accuracy.type = "sens", est.type = "mean", 
nameShow = T, dataShow = F, ciShow = T, graphwidth = 1, main, xlab = "",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A \code{meta4diag} object obtained by running the main function \code{meta4diag()}.
}
  \item{accuracy.type}{
  A string specifying the accuracy type. Options are "sens", "TPR", "spec", "TNR", "FPR", "FNR", "LRpos", "LRneg", "RD", "LLRpos", "LLRneg", "LDOR", and "DOR".
}
  \item{est.type}{
  The type of estimation of study specified summary points. Options are "mean" and "median".
}
  \item{p.cex}{
  Points size of study specific estimate.
}
  \item{p.pch}{
  Points symbol of study specific estimate.
}
  \item{p.col}{
  Points color of study specific estimate.
}
  \item{nameShow}{
  Boolean indicating whether the study names are shown or not. Can also be a string indicating the alignment of the study names. Options are "left", "center" and "right".
}
  \item{dataShow}{
  Boolean indicating whether the original data is shown or not. Can also be a string indicating the position to show the original data. Options are "left", "center" and "right".
}
  \item{ciShow}{
  Boolean indicating whether the credible intervals are shown or not. Can also be a string indicating the position to show the values of credible intervals. Options are "left", "center" and "right".
}
  \item{text.cex}{
  The size to be used for the table text.
}
  \item{shade.col}{
  Color of shaded area. 
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
\item{cut}{
 Boolean indicating the arrows should be cut or not. Or a length 2 numerical vector indicating the cut position.
}
  \item{main}{
  An overall title for the plot.
}
  \item{main.cex}{
  The size to be used for main titles.
}
  \item{axis.cex}{
  The size to be used for axis annotation.
}
\item{intervals}{
  A numerical vector with length 2 specifying the credible intervals that is of interst. The values should be taken from the argument quantiles (see meta4diag). The first value should be smaller than 0.5 and the second value should be larger than 0.5. 
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
A forest plot is a graphical representation of a meta-analysis. It is usually accompanied by a table listing references (author and date) of the studies included in the meta-analysis. In the main device each line represents one study in the meta-analysis, plotted according to the estimated result. The estimated accuracy for each study is plotted as point and credible intervals as arrows. The size of the estimated accuracy for each study can be chosen to be propotional to the number of individuals in each study. A diamond indicates the overall summary point. Estimated accuracies and the correspoing credible intervals for each study are plotted. Together shown in the plot with the study names, the original dataset and the values of accuracies and the credible intervals.

The function \code{forest()} gives the user flexibility to control the plots.
}

\value{Besides plotting, the function returns an invisible NULL.}

\author{
  Jingyi Guo and Andrea Riebler 
}

\examples{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter)
  forest(res, accuracy.type = "sens")
}
}