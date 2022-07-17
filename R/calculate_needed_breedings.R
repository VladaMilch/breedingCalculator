#' Calculate the number of breedings needed to guarantee a fixed number of
#' offsprings with a certain probability
#'
#' At the moment only one model is implemented
#'
#' @param confidence_p a number between 0 and 1
#' @param fertility_p a number between 0 and 1
#' @param litter_mean the average litter size of the bred animals
#' @param litter_sd standart deviation of the litter size, equals 2.5
#' by default - as specified in the textbook
#' @param n_needed  number of offsprings needed to be born
#' @param method the model used for the calculation, can be "festing"
#'
#' @return The function returns the number of breedings
#' needed to guarantee \code{n_needed} offprings with probability no less than
#' \code{confidence_p}.
#' @examples
#' calculate_needed_breedings(
#'     confidence_p=0.95,
#'     fertility_p=0.6,
#'     n_needed = 30,
#'     litter_mean = 7,
#'     litter_sd = 2.5,
#'     n_litters=10)
#'
#' 
#' Minimal number of breedings
#' 
#' Calculates the minimal number of animals to be bred in order
#' to achieve a given number of offsprings with a certain confidence, 
#' under the one of the models described below.
#'
#' @param confidence_p desired confidence level (probability to achieve 
#' n_needed offsprings), a number between 0 and 1, excluding 1
#' @param fertility_p how likely is an animal to be fertile, a number 
#' between 0 and 1, including 1
#' @param n_needed desired total number of offsprings 
#' @param litter_mean average number of offsprings one fertile animal produces, 
#' needed for method values "festing", "binomial" and "poisson"
#' @param offsprings_n_sample a sample of the offspring numbers one animal 
#' produces, needed for method value "empirical"; 
#' for instance, c(3,1,5,8) is a the data from four animals: The first 
#' animal had 3 offsprings, the second had 1 offspring, the third animal had 5 
#' offsprings and the fourth animal had 8 offsprings; 
#' preferably, this should be a large sample
#' @param litter_sd standard deviation of the litter size, needed for method 
#' value "festing"
#' @param method can be "festing", "poisson" or "empirical" # keep pnly poisson? @TODO
#' @param calculation_type 
#'
calculate_needed_breedings <- function(
    confidence_p,
    fertility_p,
    n_needed,
    litter_mean = NULL,
    offsprings_n_sample = NULL, # should be a large vector, n of offs for a single mouse
    litter_sd = 2.5,
    binomial_p=NULL,
    method = "festing"
){
    # arguments needed for every function
    stopifnot(confidence_p < 1 & confidence_p > 0)
    stopifnot(is.wholenumber(n_needed) & n_needed > 0)
    stopifnot(method %in% 
                  c("festing", "empirical", "poisson", "binomial"))
    
    if(method=="festing"){
        stopifnot(litter_sd > 0)
        stopifnot(litter_mean > 0)
        confidence_p1 = 1 - (1-confidence_p)/2
        nlit <- calculate_needed_litters_textbook(
            confidence_p=confidence_p1,
            litter_mean=litter_mean,
            litter_sd=litter_sd,
            n_neede=n_needed)
        nbre <- calculate_needed_breedings_textbook(
            confidence_p=confidence_p1,
            fertility_p=fertility_p,
            n_litters=nlit
            )
        return(nbre)
    }
    # if(method=="binomial"){
    #     stopifnot(litter_mean > 0)
    #     nbre <- calculate_needed_breedings_binomial(
    #         confidence_p = confidence_p, 
    #         fertility_p = fertility_p, 
    #         n_needed = n_needed, 
    #         litter_mean = litter_mean
    #     )
    #     return(nbre)
    # }
    if(method=="empirical"){
        nbre <- calculate_needed_breedings_empirical(
            confidence_p = confidence_p, 
            fertility_p = fertility_p, 
            n_needed = n_needed, 
            offsprings_n_sample = offsprings_n_sample
        )
        return(nbre)
    }
    if(method=="poisson"){
        stopifnot(litter_mean > 0)
        nbre <- calculate_needed_breedings_poisson(
            confidence_p = confidence_p, 
            fertility_p = fertility_p, 
            n_needed = n_needed, 
            litter_mean = litter_mean
        )
        return(nbre)
    }
}


# @TODO same input - animals born as output: move expect_born_poisson here 