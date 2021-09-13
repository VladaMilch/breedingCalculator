#' Calculates the smallest number of breeding needed to yield a desired number 
#' of offsprings of a single genotype with a given probability of success
#'
#' @param confidence_p 
#' @param birth_days takes values 1,2,3 or 4: 
#' the desired offsprings should be born within `birth_days`; 
#' alternatibely, a number between 0 and 1 specifying the effective fertility 
#' of the animal
#' @param n_offsprings the desired number of offsprings
#' @param sex_distribution takes values "unimportant", "all one sex", "balanced"; 
#' reflects the distribution of sexes among the offsprings: If "balanced", 
#' equal numbers of male and female offspring should be born for each genotype; 
#' if "all one sex", all of the required offsprings should be either male or female
#' if "unimportant", the offsprings of any sex suffice. 
#' Female and male offpsings are born with the same frequency (0.5).
#' @param strain mouse strains, currently available are:   
#' 129/SvJa,   
#' A/J,   
#' AKR/J, 
#' BALB/cJ,   
#' C3H/HeJ,   
#' C3H/HeOuJ,   
#' C56BL/6J,   
#' C57_BL/10SnJ,   
#' CBA/CaJ,   
#' DBA/2J,   
#' FVB/N, 
#' SJL/J, 
#' Festing (when the strain is unknown, use "Festing" as a general 
#' textbook example) and "manual" (to set the strain parameters - litter mean 
#' and effective fertility - manually)
#' @param method takes values "festing" or "poisson"
#' @param litter_average only used when method="manual", the average number 
#' of pups in a litter (litter mean)
#' @param effective_fertility only used when method="manual", the probability 
#' that withing the required time period a mouse will produce at least one pup
#'
#'@examples
#'singleGenotype(confidence_p = 0.9, 
#'               n_offsprings = 16, 
#'               strain = "FVB/N")
#'# shorter time period when offspring should be born
#'singleGenotype(confidence_p = 0.9, 
#'               birth_days = 3,
#'               n_offsprings = 16, 
#'               strain = "FVB/N")  
#'# manual input of strain parameters   
#'singleGenotype(confidence_p = 0.9, 
#'               birth_days = 3,
#'               n_offsprings = 16, 
#'               strain = "manual") 
#' @return
#' @export
singleGenotype <- function(
  confidence_p = 0.9,
  birth_days = 4,
  desired_genotype_p = 1, 
  n_offsprings,
  sex_distribution = c("unimportant", "all one sex", "balanced"),
  strain = "manual",
  method = "poisson",
  litter_average = NULL,
  effective_fertility = NULL
){
  if(desired_genotype_p==1){
    genotypes_p_current <- c(1)
    genotypes_N_current <- n_offsprings
    
  }else{
    genotypes_p_current <- c(desired_genotype_p, 1-desired_genotype_p)
    genotypes_N_current <- c(n_offsprings, 0)
  }
  
  nbre <- multiGenotype(
    confidence_p = confidence_p, 
    birth_days = birth_days, 
    genotypes_p = genotypes_p_current, 
    genotypes_N = genotypes_N_current, 
    sex_distribution = sex_distribution, 
    strain = strain, 
    litter_average = litter_average, 
    effective_fertility = effective_fertility)
  
  return(nbre)
}


# @TODO same input - animals born as output: move expect_born_poisson here 