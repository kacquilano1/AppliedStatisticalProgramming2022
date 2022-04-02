#' An approximated value object from Trapezoid Rule
#'
#' Object of class \code{Trapezoid} is created by the \code{trapezoid_fun} function
#'
#'
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{ab} the start/end values
#' \item \code{x} the vector of values
#' \item \code{y} the vector of evaluated values
#' \item \code{result} The approximated value
#' }
#'
#' @import methods
#' @author Kimberly Acquilano: \email{k.a.acquilano@@wustl.edu}
#' @aliases Trapezoid-class initialize,Trapezoid-method
#' @rdname Trapezoid
#' @export
setClass(Class="Trapezoid",
         representation = representation(
           ab = "numeric",
           x = "numeric",
           y = "numeric",
           result = "numeric"
         ),
         prototype = prototype(
           ab = NA_real_,
           x = NA_real_,
           y = NA_real_,
           result = NA_real_
           )
)

#' @export
setMethod("initialize", "Trapezoid",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })



#' @export
setValidity("Trapezoid", function(object) {
  #valid test, to ensure the inputs are valid
  if (length(object@ab) != 2) {
    "@ab must be a vector of 2"
  } else if (length(object@x) != length(object@y)) {
    "@x and @y must be the same length"
  } else {
    TRUE
  }
})
