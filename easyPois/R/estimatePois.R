#' Estimating the Poisson Distribution
#'
#' Calculates the log likelihood, the maximum likelihood, and the standard error for a vector of values.
#'
#' @param y A vector of values
#' @param SEtype The method used to calculate the standard error: basic or bootstrap
#' @param B The number of bootstrapped resamplings
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
           def=function(y, SEtype, B)
           {standardGeneric("estimatePois")}
)

#' @export
setMethod(f="estimatePois",
          definition=function(y, SEtype, B){

            #Create a value for the number of observations in the vector y
            #This value will be used for the LL, MLE, and SE functions
            n <- length(y)

            #Create the MLE function, which will calculate the maximum likelihood estimator for lambda
            #The MLE is the sum of the values in y, divided by the number of observations
            MLE <- sum(y)/n

            #Create the LL function, which will calculated the log likelihood for the data.
            #This function will use the MLE calculated above as the estimated lambda, the n calculated above, and the origianl data input as y
            #The function is  - [n * MLE] - sum(ln(y!)) - [ln(MLE)*sum(y)]
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

              #return LL(MLE) = first - second + third
              LL <- first - second + third


            #Calculate the standard error of the MLE
              #There will be two different possible calculations, depending on which SEtype the user put in
            if (SEtype == "basic") {
              #if the user chooses basic, then SE will be calculated as the square root of (MLE / n)
              #This will be saved as SE
              SE <- sqrt(MLE/n)
            } else if (SEtype == "bootstrap") {
              #Create a matrix, matB, of samples. matB will have dimensions n x B
                #follow the example of how to bootstrap here:https://stats.oarc.ucla.edu/r/library/r-library-introduction-to-bootstrapping/
                #B is the number of samples from y data, with replacement, samples are size n
                #Use sapply here to extract B number of n samples, sapply will turn this into a matrix
                #function(i) for each time 1 through B
                #Set replace to TRUE for replacement
              matB <- sapply(1:B, function(i) sample(y, n, replace = TRUE))

              #Calculate the MLE for each column of samples
                #use the apply function, set to 2 for columns
                #Use the same function as used above in MLE to get the MLE of each column
              vectorMLE <- apply(matB, 2, function(i) sum(i)/n)
                #calculate the newMLE, using the same MLE formula as above
                #This newMLE is the total MLE for all the bootstrapped samples
              newMLE <- sum(vectorMLE)/B
              #Calculate the SE, using the same formula as in basic SE, using newMLE
              SE <- sqrt(newMLE/n)
            } else {stop("Error: SEtype is invalid. Must be basic or bootstrap")
            }

          #Return an object that is of class PoisMLE
              #This will output the original data, MLE, LL, SE, and SEtype
              return(new("PoisMLE", y = y, MLE = MLE, LL = LL, SE = SE, SEtype = SEtype))


})
