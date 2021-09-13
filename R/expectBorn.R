#' Expected total number of animals born in a breeding setup
#'
#' @param breedingObj an object created by singleGenotype or multiGenotype function
#'
#' @return
#' a plot object, the distribution of the total number of born animals reflected as a histogram
#' @examples 
#' breObj <- multiGenotype(
#'   confidence_p = 0.9, 
#'   birth_days = 3, 
#'   genotypes_p = c(0.25, 0.5, 0.25), 
#'   genotypes_N = c(10,0,10), 
#'   sex_distribution = "unimportant",
#'   litter_average = 7,
#'   strain = "C56BL/6J")
#' expectBorn(breObj)
#' 
#' @export
expectBorn <- function(breedingObj){
  kk = breedingObj$required_breedings
  doofN <- distr::convpow(breedingObj$doof1_obj, N=kk)
  
  return( hist(
    doofN@r(n = 10000), 
    freq = FALSE, 
    xlab = "Pups born", 
    ylab="Probability",
    main = "Total number of born animals", 
    col = "lightblue",
    breaks = 20
    )
  )
}

