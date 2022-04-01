#' An approximated value object from Simpson Rule
#'
#' Object of class \code{Smpson} is created by the \code{simpson_fun} function
#'
#'
#' An object of the class `Simpson' has the following slots:
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
#' @aliases Simpson-class initialize,Simpson-method integratIt-generic integrateIt,Simpson-method show,Simpson-method
#' @rdname Simpson
#' @export
setClass(Class="Simpson",
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
setMethod("initialize", "Simpson",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          })

#' @export
setGeneric("integrateIt",
           function(object="Simpson")
             standardGeneric("integrateIt"),
           signature = "object"
)

#' @export
setMethod("integrateIt", "Simpson",
          function(object) {
            output <- list(Rule = "Simpson", Values = c(object@w, object@y), Result = object@result)
            return(output)
          })

#' @export
setMethod("show", "Simpson", function(object) {
  cat(is(object)[[1]], "\n",
      "  Result: ", object@result, "\n",
      sep = ""
  )
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
