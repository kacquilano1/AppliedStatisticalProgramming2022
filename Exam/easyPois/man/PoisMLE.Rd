% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PoisMLE.R
\docType{class}
\name{PoisMLE-class}
\alias{PoisMLE-class}
\alias{initialize,PoisMLE-method}
\title{An object using the Poisson Distribution}
\description{
Objects of class \code{PoisMLE} are created by the \code{estimatePois} function
}
\details{
An object of the class `PoisMLE' has the following slots:
\itemize{
\item \code{y} A vector of data
\item \code{MLE} The maximum likelihood estimator
\item \code{LL} The log likelihood calculated from the vector of data
\item \code{SE} The standard error for the MSE
\item \code{SEtype} The method used to calculate the standard error
}
}
\author{
Kimberly Acquilano \email{k.a.acquilano@wustl.edu}
}
