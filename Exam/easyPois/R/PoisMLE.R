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


#Validity test on class PoisMLE
#' @export
setValidity("PoisMLE", function(object) {
  #If any of the following are TRUE, then they will put out a message
 #First check that the input for y is a vector of more than 1 element
 if (length(object@y) <= 1) {"y must be a vector of more than 1 element"
 } #Check that MLE is a vector of 1 element
  else if (length(object@MLE) != 1) {"MLE must be a vector of 1"
 } #Check that LL is a vector of 1 element
  else if (length(object@LL) != 1) {"LL must be a vector of 1"
 } #Check that SE is a vector of 1 element
  else if (length(object@SE) != 1) {"SE must be a vector of 1"
  } #If none of these statements are TRUE (in other words, everything is of the correct vecotr length), then return TRUE
  else {TRUE}
})

