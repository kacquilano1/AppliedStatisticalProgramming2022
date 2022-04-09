#' Estimating the Poisson Distribution
#'
#' Calculates the log likelihood, the maximum likelihood, and the standard error for a vector of values.
#'
#' @param y A vector of values
#' @param SEtype The method used to calculate the standard error
#'
#'
#' @return Returns an object of class \code{'PoisMLE'}, which contains:
#'  \item{y}{The original vector of data}
#'  \item{MLE}{The maximum likelihood estimator for the data}
#'  \item{LL}{The log likelihood calculated from the observed data}
#'  \item{SE}{The standard error for the MLS}
#'  \item{SEtype}{The method used to calculate the standard error}
#' @author Kimberly Acquilano
#'
#'
#' #' @examples
#'
#'
#' @seealso PoisMLE-class
#' @rdname estimatePois
#' @aliases estimatePois,ANY-method
#'
#' @export
setGeneric(name="estimatePois",
           def=function(y, SEtype)
           {standardGeneric("estimatePois")}
)

#' @export
setMethod(f="estimatePois",
          definition=function(y, SEtype){

            #Create a value for the number of observations in the vector y
            #This value will be used for the LL, MLE, and SE functions
            n <- length(y)

            #Create the MLE function, which will calculate the maximum likelihood estimator for lambda
            MLE <- sum(y)/n

)
