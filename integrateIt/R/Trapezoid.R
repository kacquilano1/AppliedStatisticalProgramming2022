#' An approximated value object from Trapezoid Rule
#'
#' Object of class \code{Trapezoid} is created by the \code{trapezoid_fun} function
#'
#'
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{result} The approximated value
#' \item \code{ab} the start/end values
#' \item \code{w} the vector of values
#' \item \code{y} the vector of evaluated values
#' \item \code{f} the function to be integrated
#' }
#'
#' @import methods
#' @author Kimberly Acquilano: \email{k.a.acquilano@@wustl.edu}
#' @aliases Trapezoid-class initialize,Trapezoid-method integrateIt,Trapezoid-method print,Trapezoid-method
#' @rdname Trapezoid
#' @export
setClass(Class="Trapezoid",
         representation = representation(
           result = "numeric",
           ab = "numeric",
           w = "numeric",
           y = "numeric"
           ),
         prototype = prototype(
           result = NA_real_,
           ab = NA_real_,
           w = NA_real_,
           y = NA_real_
           )
)

#' @export
setMethod("initialize", "Trapezoid",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })

#' @export
setGeneric("integrateIt",
           function(object)
             standardGeneric("integrateIt"),
          )

#' @export
setMethod("integrateIt", signature = "Trapezoid",
          definition = function(object) {
            output <- list(Rule = "Trapezoid", Values = c(object@w, object@y), Result = object@result)
            return(output)
})

#' @export
setGeneric("print",
           function(object)
             standardGeneric("print"),
)

#' @export
setMethod("print", signature = "Trapezoid", definition = function(object) {
cat(is(object)[[1]], "\n",
    "  Result: ", object@result, "\n",
    sep = ""
)
})

#' @export
setValidity("Trapezoid", function(object) {
  if (length(object@ab) != 2) {
    "@ab must be a vector of 2"
  } else if (length(object@w) != length(object@y)) {
    "@w and @y must be the same length"
  } else if (length(object@w) >= length(object@ab)) {
    "@w and @y have a length smaller than @ab"
  } else {
    TRUE
  }
})
