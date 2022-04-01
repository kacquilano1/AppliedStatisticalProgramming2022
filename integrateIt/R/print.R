#' A method for intgrateIt
#'
#'Method of \code{print} is created by the \code{print} function
#'
#'
#'
#' @import methods
#' @author Kimberly Acquilano: \email{k.a.acquilano@@wustl.edu}
#' @aliases print,Simpson-method print,Trapezoid-method
#' @rdname print
#' @param object object of class Simpson or Trapezoid
#'
#' @export
setGeneric("print",
           function(object)
             standardGeneric("print"),
)

#' @export
setMethod("print", signature = "Simpson", definition = function(object) {
  cat(is(object)[[1]], "\n",
      "  Result: ", object@result, "\n",
      sep = ""
  )
})

#' @export
setMethod("print", signature = "Trapezoid", definition = function(object) {
  cat(is(object)[[1]], "\n",
      "  Result: ", object@result, "\n",
      sep = ""
  )
})
