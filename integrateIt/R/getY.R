#' Gets the vector of evaluated values under the integral
#'
#' Gets the vector of evaluated values under the curve for the Trapezoid and Simpson Rules' calculation of the integral
#'
#' @param object An object of class Simpson or Trapezoid
#'
#' @return An object of class Trapezoid or Simpson containing
#' \item{y}{the vector of evaluated values}
#'
#' @author Kimberly Acquilano
#' @note This method is used by the integrateIt and print methods
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}, \code{\link{integrateIt}}, \code{\link{print}}
#' @aliases getY,ANY-method
#' @rdname getY
#'
#' @export
setGeneric("getY",
           function(object)
             standardGeneric("getY")
)

#' @export
setMethod("getY",
          definition = function(object) {
            return(object@y)
          })
