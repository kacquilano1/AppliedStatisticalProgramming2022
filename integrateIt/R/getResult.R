#' Gets the result of integral approximation
#'
#' Gets the result from the Trapezoid and Simpson Rules' calculation of the integral
#'
#' @param object An object of class Simpson or Trapezoid
#'
#' @return An object of class Trapezoid or Simpson containing
#' \item{result}{the approximated result}
#'
#' @author Kimberly Acquilano
#' @note This method is used by the integrateIt and print methods
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}, \code{\link{integrateIt}}, \code{\link{print}}
#' @aliases getResult,ANY-method
#' @rdname getResult
#'
#' @export
setGeneric("getResult",
           function(object)
             standardGeneric("getResult")
)

#' @export
setMethod("getResult",
          definition = function(object) {
            return(object@result)
          })



