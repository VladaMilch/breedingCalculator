# constructor 
breedingMulti <- function(
  required_breedings = NULL,
  confidence_p,
  fertility_p,
  genotypes_p,
  genotypes_N,
  genotype_names = NULL,
  strain = "BALB/cJ",
  if_balanced_sex = NULL,
  if_onesex = NULL,
  if_unimportant_sex = NULL,
  litter_mean,
  doof1_obj = NULL,
  method = "poisson"
){
  
  if(is.null(genotype_names)){
    genotype_names <- paste0("genotype_", LETTERS[1:length(genotypes_p)])
  }
  
  obj <- list(  
    required_breedings = required_breedings,
    confidence_p = confidence_p,
    fertility_p = fertility_p,
    genotypes_p = genotypes_p,
    genotypes_N = genotypes_N,
    genotype_names = genotype_names,
    strain = strain,
    if_balanced_sex = if_balanced_sex,
    if_onesex = if_onesex,
    if_unimportant_sex = if_unimportant_sex,    
    litter_mean = litter_mean,
    doof1_obj = doof1_obj, 
    method = method)
  
  attr(obj, "class") <- "breedingMulti"
  return(obj)
}

# aa <- breedingMulti(
#   confidence_p = 0.9, fertility_p = 0.7, 
#   genotypes_p = c(0,1), genotypes_N = c(0,10), 
#   litter_mean = 7)

# print.breedingMulti <- function(breObj){
#   x = breObj$required_breedings
#   print(x)
# }