#' Calculate the number of litters needed to obtain a fixed numberf of mice
#' with a certain confidence
#'
#' Here the number of offsprings one animal produces, i.e the litter size,
#' is assumed to follow a Gaussian distribution with mean \code{litter_mean} and
#' the standard deviation \code{litter_sd}. The function returns
#' the number of litters needed to obtain \code{n_needed} offsprings
#' with probability
#' no less than \code{confidence_p}.
#'
#' Note: In the textbook, a litter cannot have size 0.
#'
#'
#' @param confidence_p a number between 0 and 1
#' @param litter_mean the average litter size of the bred mice
#' @param litter_sd standart deviation of the litter size, equals 2.5
#' by default - as specified in the textbook
#' @param n_needed  number of baby mice needed to be born
#' @return The function returns the number of litters
#' needed to obtain \code{n_needed} offsprigns with probability no less than
#' \code{confidence_p}.
#' @examples
#' calculate_needed_breedings_textbook(
#'     confidence_p=0.95,
#'     fertility_p=0.6,
#'     n_litters=10)
#'
#' @export
calculate_needed_litters_textbook <- function(
    confidence_p,
    litter_mean,
    litter_sd=2.5,
    n_needed
){
    # Nlitters
    quantile_of_sum <- function(Nlitters){
        qnorm(p=1-confidence_p,
            mean=litter_mean*Nlitters,
            sd = litter_sd*sqrt(Nlitters))
    }
    neededL <- optimise(
        f=function(nn){
            ifelse( quantile_of_sum(nn) >= n_needed, 
                    quantile_of_sum(nn)-n_needed, 
                    10^5)
          },
        lower = 1, upper = n_needed)
    # WARNING! mathematically does not guarantee P >= confidence_p
    # res = round(neededL[[1]]) 
    
    
    # doofN_quantile = 0
    # k=1
    # while(doofN_quantile < n_needed){
    #     #doofN_quantile <- sapply(search_interval, FUN = function(k){distr::convpow(doof1, N=k)@q(confidence_p)})
    #     doofN_quantile <- distr::convpow(doof1, N=k)@q(1-confidence_p)
    #     k <- k+1
    # }
    # return(k-1)
    
    res = ceiling(round(neededL[[1]], digits=3))
    return(res)
}
