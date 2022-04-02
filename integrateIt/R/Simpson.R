#' An approximated value object from Simpson Rule
#'
#' Object of class \code{Simpson} is created by the \code{simpson_fun} function
#'
#'
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{x} the vector of values
#' \item \code{y} the vector of evaluated values
#' \item \code{result} The approximated value
#' }
#'
#' @import methods
#' @author Kimberly Acquilano: \email{k.a.acquilano@@wustl.edu}
#' @aliases Simpson-class initialize,Simpson-method
#' @rdname Simpson
#' @export
setClass(Class="Simpson",
         representation = representation(
           x = "numeric",
           y = "numeric",
           result = "numeric"
         ),
         prototype = prototype(
           x = NA_real_,
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

#' @export
setValidity("Simpson", function(object) {
  #validity function to ensure the inputs are valid
  if (length(object@result) != 1) {
    "@result must be a vector of 1"
  } else if (length(object@x) != length(object@y)) {
    "@x and @y must be the same length"
  } else {
    TRUE
  }
})

