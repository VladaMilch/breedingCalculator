#' Optimal number of breedings, multiple genotypes setup
#' 
#' Calculates the smallest number of breedings needed to yield a desired number 
#' of offsprings of multiple genotypes with a given probability of success
#'
#' @param confidence_p 
#' @param birth_days takes values 1,2,3 or 4: 
#' the desired offsprings should be born within `birth_days`; 
#' alternatibely, a number between 0 and 1 specifying the effective fertility 
#' of the animal
#' @param genotypes_p probabilities of a single offspring to have each of the respective genotypes, should sum to 1
#' @param genotypes_N required number of offsprings of the respective genotypes
#' @param genotype_names names of the genotypes, should be the same length as genotypes_p and genotypes_N
#' @param sex_distribution takes values "unimportant", "all one sex", "balanced"; 
#' reflects the distribution of sexes among the offsprings: If "balanced", 
#' equal numbers of male and female offspring should be born for each genotype; 
#' if "all one sex", all of the required offsprings should be either male or female
#' if "unimportant", the offsprings of any sex suffice. 
#' Female and male offpsings are born with the same frequency (0.5).
#' @param litter_average average number of offsprings in a single litter
#' @param strain mouse strains, currently available are: 
#' 129/SvJa, 
#' A/J, 
#' AKR/J, 
#' BALB/cJ, 
#' C3H/HeJ, 
#' C3H/HeOuJ, 
#' C57BL/6J, 
#' C57BL/10SnJ, 
#' CBA/CaJ, 
#' DBA/2J, 
#' FVB/N 
#' SJL/J, 
#' Festing (when the strain is unknown, use "Festing" as a general 
#' textbook example)
#'
#' @return
#' object of the breedingMulti class
#' 
#' @examples 
#' multiGenotype(
#'   confidence_p = 0.9, 
#'   birth_days = 3, 
#'   genotypes_p = c(0.25, 0.5, 0.25), 
#'   genotypes_N = c(10,0,10), 
#'   sex_distribution = "unimportant",
#'   litter_average = 7,
#'   strain = "C57BL/6J")
#' 
#' @export
multiGenotype <- function(
  confidence_p = 0.9,
  birth_days = 3,
  genotypes_p = c(0.25,0.5,0.25),
  genotypes_N = c(10,0,10),  
  genotype_names = NULL,
  sex_distribution = c("unimportant", "all one sex", "balanced"),
  strain="Festing", # http://www.informatics.jax.org/silver/tables/table4-1.shtml
  litter_average = NULL,
  fertility = NULL
){
  
  ################# integrity checks ###############
  
  # confidence_p
  stopifnot(confidence_p < 1 & confidence_p > 0)
  # birth_days
  try(
    if(!( (is.wholenumber(birth_days) & birth_days > 0) ))
    {stop("birth_days can only take whole positive values (1,2,3,...) \n")} 
  )
  
  # strain --> litter mean and fertility
  if (strain == "manual"){
    fert_by_day <- c(13.4, 13.4, 35, 17.7) # as in Festing
    fertility_p <- cumsum(fert_by_day/sum(fert_by_day))[birth_days] * fertility
    litter_mean = litter_average
    # do not do day adjustment here! so that to control the function output.. 
  }else{ # percentages from the Festing book, Table 3.11
    strain_params = strain_f_adjust(
      birth_days = birth_days, 
      strain = strain)
    fertility_p = strain_params$fertility
    litter_mean = strain_params$lit_mean
    try( if(!is.null(litter_average) | !is.null(fertility))
      warning("Litter mean and effective fertility parameters are overwritten 
              because a particular mouse strain is chosen. If the input 
              parameters should be used, choose strain='manual'.")
    )
  }
  # genotypes_p
  if(!(all(genotypes_p >=0) & sum(genotypes_p)==1)){
    stop("Genotype probabilities should be non-negative and sum to 1.\n")
  }
  # genotypes_N
  if(!all(is.wholenumber(genotypes_N))){
    stop("Required number of offsprings should be an integer for each genotype.\n")
  }
  # sex_distribution
  stopifnot(
    sex_distribution %in% c("unimportant", "all one sex", "balanced")
  )
  # litter_mean
  if(!(litter_mean > 0)){
    stop("Average number of pups per litter should be a positive number.\n")
  }
  
  
  
  ################# calculation  ################
  
  if(sex_distribution == "unimportant"){
    breesetup <- breed_genotype(
      confidence_p = confidence_p, 
      fertility_p = fertility_p, 
      genotypes_p = genotypes_p,
      genotypes_N = genotypes_N, 
      litter_mean = litter_mean, 
      method = "poisson")
  }
  if(sex_distribution == "all one sex"){
    breesetup <- breed_genotype(
      confidence_p = confidence_p, 
      fertility_p = fertility_p, 
      genotypes_p = c(genotypes_p/2, 0.5),
      genotypes_N = c(genotypes_N, 0), 
      litter_mean = litter_mean, 
      method = "poisson")
  }
  if(sex_distribution == "balanced"){
    if( try( !all(is.wholenumber(genotypes_N/2) ) )){
      stop(
        message = "For balanced experiment setup, all genotype_N values should be even.\n")
    }
    breesetup <- breed_genotype(
      confidence_p = confidence_p, 
      fertility_p = fertility_p, 
      genotypes_p = c(genotypes_p/2, genotypes_p/2),
      genotypes_N = c(genotypes_N, genotypes_N)/2, 
      litter_mean = litter_mean, 
      method = "poisson")
  }
  
  breesetup$genotype_names = genotype_names
  breesetup$strain = strain
  breesetup$if_balanced_sex = (sex_distribution=="balanced")
  breesetup$if_onesex = (sex_distribution=="all one sex")
  breesetup$if_unimportant_sex = (sex_distribution=="unimportant")
  
  return(breesetup)
}


# @TODO same input - animals born as output: move expect_born_poisson here 