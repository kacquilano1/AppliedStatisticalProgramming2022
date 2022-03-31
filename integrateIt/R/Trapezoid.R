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
#' @author Kimberly Acquilano: \email{k.a.acquilano@@wustl.edu}
#' @aliases Trapezoid-class initialize,integrateIt-method,print-method
#' @rdname Trapezoid
#' @export
setClass(Class="Trapezoid",
         representation = representation(
           result = "numeric",
           ab = "numeric",
           w = "numeric",
           y = "numeric",
           ),
         prototype = prototype(
           result = c(),
           ab = c(),
           w = c(),
           y = c(),
           )
)

#' @export
setMethod("initialize", "Trapezoid",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
)

#' @rdname Trapezoid
#' @export
setGeneric("integrateIt",
           function(object="Trapezoid")  {
             standardGeneric("integrateIt")
           }
)

#' @export
setMethod("integrateIt", "Trapezoid",
          function(rule = object, ab, w, y){
            return(list = object, c(object@w, object@y), object@result)
          }
)

#' @export
setMethod("print", "Trapezoid",
          function(object) {
            return(object@result)
          })
