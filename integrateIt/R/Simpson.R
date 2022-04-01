#' An approximated value object from Simpson Rule
#'
#' Object of class \code{Simpson} is created by the \code{simpson_fun} function
#'
#'
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{ab} the start/end values
#' \item \code{w} the vector of values
#' \item \code{y} the vector of evaluated values
#' \item \code{result} The approximated value
#' }
#'
#' @import methods
#' @author Kimberly Acquilano: \email{k.a.acquilano@@wustl.edu}
#' @aliases Simpson-class initialize,Simpson-method getResult,Simpson-method getW,Simpson-method getY,Simpson-method
#' @rdname Simpson
#' @export
setClass(Class="Simpson",
         representation = representation(
           ab = "numeric",
           w = "numeric",
           y = "numeric",
           result = "numeric"
         ),
         prototype = prototype(
           ab = NA_real_,
           w = NA_real_,
           y = NA_real_,
           result = NA_real_
         )
)

#' @export
setMethod("initialize", "Simpson",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })

#' @rdname getResult
#' @export
setGeneric("getResult",
           function(object = "Simpson")
             standardGeneric("getResult")
)

#' @export
setMethod("getResult", signature = "Simpson",
          definition = function(object) {
            return(object@result)
          })

#' @rdname getW
#' @export
setGeneric("getW",
           function(object = "Simpson")
             standardGeneric("getW")
)

#' @export
setMethod("getW", signature = "Simpson",
          definition = function(object) {
            return(object@w)
          })


#' @rdname getY
#' @export
setGeneric("getY",
           function(object = "Simpson")
             standardGeneric("getY")
)

#' @export
setMethod("getY", signature = "Simpson",
          definition = function(object) {
            return(object@y)
          })


#' @export
setValidity("Simpson", function(object) {
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
