\name{SROC}
\alias{SROC}
\alias{SROC.meta4diag}
\title{
  A standard summary receiver operating chracteristic plot.
}
\description{
Takes a \code{meta4diag} object produced by \code{meta4diag()} and makes the standard summary receiver operating chracteristic plot. 
}
\usage{
SROC(x,...)
\method{SROC}{meta4diag}(x, est.type="mean", sp.cex=1.5,sp.pch="*",sp.col="red",
              dataShow="o", data.col="#FF0000FF", data.cex="bubble", data.pch=1, 
              lineShow=T, sroc.type=1, line.lty=1, line.lwd=2, line.col="black",
              crShow=T, cr.lty=2, cr.lwd=1.5, cr.col="blue",
              prShow=T, pr.lty=3, pr.lwd=1,  pr.col="darkgray",
              dataFit = T, add=FALSE, save=F, main="", xlim, ylim,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A \code{meta4diag} object.
}
  \item{est.type}{
  The estimates type used to make SROC plot. Options are "mean" and "median".
}
  \item{sp.cex}{
  Summary points size. The summary points are mean or median of sensitivities and specificities of all sudies.
}
  \item{sp.pch}{
  Point symbols of summary points.
}
  \item{sp.col}{
  Color of summary points.
}
  \item{dataShow}{
  A character indicating whether the original dataset or the fitted dataset is shown or not. If is "o", the original data will be plotted. If is "f", the fitted data will be plotted. No dataset will be plotted if \code{dataShow} is not "o" or "f". 
}
  \item{data.col}{
  Color of orignal data bubbles.
}
\item{data.cex}{
  A string or a numerical value indicating the size of the plotted dataset points. If is "bubble" or "scaled", the size of the data points are proportional to the total number of individuals in each study. 
}
\item{data.pch}{
  A string or a numerical value indicating the symbol of the plotted dataset points.
}
  \item{sroc.type}{
  A numerical value specifying the function used to make SROC line. Options are 1, 2, 3, 4, 5. When \code{sroc.type=1},
  \deqn{y = \mu + \rho\sqrt{\frac{\sigma_{\mu}^2}{\sigma_{\nu}^2}}(x-\nu)}{y = \mu + \rho\sqrt{\sigma_{\mu}^2/\sigma_{\nu}^2}(x-\nu)}
  When \code{sroc.type=2},
  \deqn{y = \frac{\sigma_{\mu}^2-\sigma_{\nu}^2\pm\sqrt{(\sigma_{\mu}^2-\sigma_{\nu}^2)^2+4\rho^2\sigma_{\mu}^2\sigma_{\nu}^2}}{2\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2}}(x-\nu)+\mu}{y = (\sigma_{\mu}^2-\sigma_{\nu}^2\pm\sqrt{(\sigma_{\mu}^2-\sigma_{\nu}^2)^2+4\rho^2\sigma_{\mu}^2\sigma_{\nu}^2})/(2\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2})(x-\nu)+\mu}
  When \code{sroc.type=3},
  \deqn{y = \frac{\sigma_{\mu}^2 + \rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2}}{\sigma_{\nu}^2+\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2}}(x-\nu)+\mu}{y = (\sigma_{\mu}^2 + \rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2})/(\sigma_{\nu}^2+\rho\sqrt{\sigma_{\mu}^2\sigma_{\nu}^2})(x-\nu)+\mu}
  When \code{sroc.type=4},
  \deqn{y = \mu + \frac{1}{\rho}\sqrt(\frac{\sigma_{\mu}^2}{\sigma_{\nu}^2})(x-\nu)}{y = \mu + 1/\rho\sqrt(\sigma_{\mu}^2/\sigma_{\nu}^2)(x-\nu)}
  When \code{sroc.type=5},
  \deqn{y = \mu + \sqrt{\frac{\sigma_{\mu}^2}{\sigma_{\nu}^2}}(x-\nu)}{y = \mu + \sqrt{\sigma_{\mu}^2/\sigma_{\nu}^2}(x-\nu)}
}
  \item{lineShow}{
  Boolean indicating whether the SROC line is shown or not.
}
    \item{line.lty}{
  SROC line type.
}
  \item{line.lwd}{
  SROC line width.
}
  \item{line.col}{
  Color for the SROC line.
}
  \item{crShow}{
  Boolean indicating whether the confidence region is shown or not.
}
  \item{cr.lty}{
  Confidence region line width.
}
  \item{cr.lwd}{
  Confidence region line width.
}
  \item{cr.col}{
  Color for the confidence region line.
}
  \item{prShow}{
  Boolean indicating whether the prediction region is shown or not.
}
  \item{pr.lty}{
  Prediction region line type.
}
  \item{pr.lwd}{
  Prediciton region line width.
}
  \item{pr.col}{
  Color for the prediction region line.
}
\item{dataFit}{
Boolean indicating the length SROC line. Either plotted from -1 to 1, or fit the data.
}
  \item{add}{
  If add is TRUE, the plots are added to an existing plot, otherwise a new plot is created.
}
  \item{save}{
  If save is TRUE, the plots are saved(pdf format) automatically in the working directory. save could also be a file name, i.e. \code{save="plot1.eps"}, a file with name plot1 and eps format will be saved in the working directory.
}
  \item{main}{
  A overall title for the plot.
}
  \item{xlim}{
  A numeric value, giving the x coordinates ranges.
}
  \item{ylim}{
  A numeric value, giving the y coordinates ranges.
}
 \item{\dots}{
  Further arguments passed to or from other methods.
}
}

\details{
The \code{SROC()} returns a plot of the true positive rate against the false positive rate (1-specificity) (ROC space) for the different possible cutpoints of a diagnostic test. It shows the tradeoff between sensitivity and specificity (any increase in sensitivity will be accompanied by a decrease in specificity). The closer the curve follows the left-hand border and then the top border of the ROC space, the more accurate the test. The closer the curve comes to the 45-degree diagonal of the ROC space, the less accurate the test. The area under the curve is a measure of text accuracy. Summary points, summary receiver operating characteristic line, confidence region and prediction region of the summary points are plotted.

The function \code{SROC()} gives the user flexibility to control the plots.
}

\value{The return value is a list of the files created (if any).}
\author{
  Jingyi Guo and Andrea Riebler 
}

\references{
  Chappell F, Raab G, Wardlaw J (2009). "When are Summary ROC Curves Appropriate for Diagnostic Meta-analyses?" Statistics in Medicine, 28(21), 2653-2668. 
}

\seealso{
  \code{meta4diag}
}
\examples{
if(requireNamespace("INLA", quietly = TRUE)){
  require("INLA", quietly = TRUE)
  data(Catheter)
  res <- meta4diag(data = Catheter)
  SROC(res, est.type="mean")
}
}