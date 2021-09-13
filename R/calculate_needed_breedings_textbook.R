#' Calculate the number of breedings needed to obtain a fixed numberf of litters with a certain confidence
#'
#' Here the number of fertile mice \code{n_litters} follows the binomial
#' distribution with probability of success being \code{effective_fertility_p}
#' and the number of trials being the number of breedings. The function returns
#' the number of breedings needed to achieve  \code{n_litters} with probability
#' no less than \code{confidence_p}.
#'
#' @param confidence_p a number between 0 and 1
#' @param effective_fertility_p a number between 0 and 1
#' @param n_litters an integer >= 1, the number of litters
#' determined by the function calculate_needed_litters with the
#' method='textbook' option
#' @return The function returns the number of breedings
#' needed to achieve  \code{n_litters} with probability no less than \code{confidence_p}.
#' @examples
#' calculate_needed_breedings_textbook(
#'     confidence_p=0.95,
#'     effective_fertility_p=0.6,
#'     n_litters=10)
#' 
#' @export
calculate_needed_breedings_textbook <- function(
    confidence_p, 
    effective_fertility_p,
    n_litters,
    textbook_error=FALSE
    #calculation_type='textbook_exact')
){  
    stopifnot(confidence_p < 1 & confidence_p > 0)
    stopifnot(effective_fertility_p <= 1 & effective_fertility_p > 0)
    stopifnot(is.wholenumber(n_litters) & n_litters >= 1)
    
    if(textbook_error){
        Nbreedings <- round( max( solve_quadratic(
            a = effective_fertility_p, 
            b = -round(qnorm(p = 1-confidence_p), digits = 1)*sqrt(
                effective_fertility_p*(1-effective_fertility_p)), 
            c = -n_litters
            )^2   )   )
    }else{
        search_interval <- c((n_litters+1):10^5)
        Nbreedings <- search_interval[
           min(which(pbinom(
               q = n_litters-1, 
               size = search_interval, 
               prob = effective_fertility_p, lower.tail = F) >= confidence_p))
           ]
    }
    return(Nbreedings)
}
