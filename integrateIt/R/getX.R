#' Gets the vector of values under the integral
#'
#' Gets the vector of values under the curve for the Trapezoid and Simpson Rules' calculation of the integral
#'
#' @param object An object of class Simpson or Trapezoid
#'
#' @return An object of class Trapezoid or Simpson containing
#' \item{x}{the vector of values}
#'
#' @author Kimberly Acquilano
#' @note This method is used by the integrateIt and print methods
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}, \code{\link{integrateIt}}, \code{\link{print}}
#' @aliases getX,ANY-method
#' @rdname getX
#'
#' @export
setGeneric("getX",
           function(object)
             standardGeneric("getX")
)

#' @export
setMethod("getX",
          definition = function(object) {
            return(object@x)
          })



