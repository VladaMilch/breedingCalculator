#' #' Calculates the smallest number of breeding needed to yield a desired number 
#' #' of offsprings of a single genotype with a given probability of success
#' #'
#' #' @param confidence_p 
#' #' @param birth_days takes values 1,2,3 or 4: 
#' #' the desired offsprings should be born within `birth_days`; 
#' #' alternatibely, a number between 0 and 1 specifying the effective fertility 
#' #' of the animal
#' #' @param n_offsprings the desired number of offsprings
#' #' @param strain mouse strains, currently available are:   
#' #' 129/SvJa,   
#' #' A/J,   
#' #' AKR/J, 
#' #' BALB/cJ,   
#' #' C3H/HeJ,   
#' #' C3H/HeOuJ,   
#' #' C56BL/6J,   
#' #' C57_BL/10SnJ,   
#' #' CBA/CaJ,   
#' #' DBA/2J,   
#' #' FVB/N, 
#' #' SJL/J, 
#' #' Festing (when the strain is unknown, use "Festing" as a general 
#' #' textbook example) and "manual" (to set the strain parameters - litter mean 
#' #' and effective fertility - manually)
#' #' @param method takes values "festing" or "poisson"
#' #' @param litter_average only used when method="manual", the average number 
#' #' of pups in a litter (litter mean)
#' #' @param effective_fertility only used when method="manual", the probability 
#' #' that withing the required time period a mouse will produce at least one pup
#' #'
#' #'@examples
#' #'singleGenotype(confidence_p = 0.9, 
#' #'               n_offsprings = 16, 
#' #'               strain = "FVB/N")
#' #'# shorter time period when offspring should be born
#' #'singleGenotype(confidence_p = 0.9, 
#' #'               birth_days = 3,
#' #'               n_offsprings = 16, 
#' #'               strain = "FVB/N")  
#' #'# manual input of strain parameters   
#' #'singleGenotype(confidence_p = 0.9, 
#' #'               birth_days = 3,
#' #'               n_offsprings = 16, 
#' #'               strain = "manual") 
#' #' @return
#' # @export @TODO: deprecate
#' singleGenotype <- function(
#'   confidence_p = 0.9,
#'   birth_days = 4,
#'   n_offsprings,
#'   strain = "manual",
#'   method = "poisson",
#'   litter_average = NULL,
#'   effective_fertility = NULL
#' ){
#'   # arguments needed for every function
#'   stopifnot(confidence_p < 1 & confidence_p > 0)
#'   stopifnot(is.wholenumber(n_offsprings) & n_offsprings > 0)
#'   stopifnot(method %in% c("festing", "poisson"))
#'   try(
#'     if(!( (is.wholenumber(birth_days) & birth_days > 0) ))
#'     {stop("birth_days can only take whole positive values (1,2,3,...) \n")} 
#'   )
#'   
#'   # birthday --> effective_fertility_p
#'   if (strain == "manual"){
#'     effective_fertility_p = effective_fertility
#'     litter_mean = litter_average
#'     # do not do day adjustment here! so that to control the function output.. 
#'   }else{ # percentages from the Festing book, Table 3.11
#'     strain_params = strain_f_adjust(
#'       birth_days = birth_days, 
#'       strain = strain)
#'     effective_fertility_p = strain_params$fertility
#'     litter_mean = strain_params$lit_mean
#'     try( if(!is.null(litter_average) | !is.null(effective_fertility))
#'       warning("Litter mean and effective fertility parameters are overwritten 
#'               because a particular mouse strain is chosen. If the input 
#'               parameters should be used, choose strain='manual'.")
#'     )
#'   }
#'   
#'   if(method=="festing"){
#'     litter_sd = 2.5
#'   }else{
#'     litter_sd = NULL
#'   }
#'   
#'   
#'   # add?? makes it quite ugly then..
#'   # # sex_distribution
#'   # stopifnot(
#'   #   sex_distribution %in% c("unimportant", "all one sex", "balanced")
#'   # )
#'   
#'   
#'   
#'   nbre <- calculate_needed_breedings(
#'     confidence_p = confidence_p, 
#'     effective_fertility_p = effective_fertility_p,
#'     n_needed = n_offsprings, 
#'     litter_mean = litter_mean,
#'     litter_sd = litter_sd,
#'     method = method)
#'   
#'   return(nbre)
#' }
#' 
#' 
#' # @TODO same input - animals born as output: move expect_born_poisson here 