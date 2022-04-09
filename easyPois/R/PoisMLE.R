#' An object using the Poisson Distribution
#'
#' Objects of class \code{PoisMLE} are created by the \code{estimatePois} function
#'
#'
#' An object of the class `PoisMLE' has the following slots:
#' \itemize{
#' \item \code{y} A vector of data
#' \item \code{MLE} The maximum likelihood estimator
#' \item \code{LL} The log likelihood calculated from the vector of data
#' \item \code{SE} The standard error for the MSE
#' \item \code{SEtype} The method used to calculate the standard error
#' }
#'
#' @author Kimberly Acquilano \email{k.a.acquilano@@wustl.edu}
#' @import methods
#' @aliases PoisMLE-class initialize,PoisMLE-method
#' @rdname PoisMLE
#' @export
setClass(Class="PoisMLE",
         representation = representation(
           y = "numeric",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = 0,
           MLE = 0,
           LL = 0,
           SE = 0,
           SEtype = ""
         )
)

#' @export
setMethod("initialize", "PoisMLE",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })


