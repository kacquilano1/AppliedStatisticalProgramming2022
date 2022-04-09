#' Estimating the Poisson Distribution
#'
#' Calculates the log likelihood, the maximum likelihood, and the standard error for a vector of values.
#'
#' @param y A vector of values
#' @param SEtype The method used to calculate the standard error: basic or bootstrap
#'
#'
#' @return Returns an object of class \code{'PoisMLE'}, which contains:
#'  \item{y}{The original vector of data}
#'  \item{MLE}{The maximum likelihood estimator for the data}
#'  \item{LL}{The log likelihood calculated from the observed data}
#'  \item{SE}{The standard error for the MLS}
#'  \item{SEtype}{The method used to calculate the standard error: basic or bootstrap}
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
            #The MLE is the sum of the values in y, divided by the number of observations
            MLE <- sum(y)/n

            #Create the LL function, which will calculated the log likelihood for the data.
            #This function will use the MLE calculated above as the estimated lambda, the n calculated above, and the origianl data input as y
            #The function is  - [n * MLE] - sum(ln(y!)) - [ln(MLE)*sum(y)]
            LL <- function(y, n, MLE) {
              #Calculate the first term
              #Multiply it by -1 to turn it negative
              first <- n * MLE * (-1)

              #Calculate the second term
                #First calculate the part inside the sum, called ln_y_i
                  #use sapply to apply the factorial to every element in the vector y, and to return another vector
                  #Wrap it in log() to calculate the natural log of each result
                #Sum all of the elements in ln_y_i
              ln_y_i <- log(sapply(y, factorial))
              second <- sum(ln_y_i)

              #Calculate the third term
              #The natural log of MLE * sum of y
              third <- log(MLE) * sum(y)

              #Result is the LL(MLE) = first - second + third
              result <- first - second + third

              #return the result
              return(result)
            }#End of LL function

            #Calculate the standard error of the MLE
            #There will be two different possible calculations, depending on which SEtype the user put in
            SE <- if (SEtype == "basic") {

            }





})
