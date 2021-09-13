#' Calculate the number of breedings needed to guarantee a fixed number of
#' offsprings with a certain probability
#'
#' At the moment only one model is implemented (the textbook).
#'
#' @export
breed_genotype <- function(
    confidence_p,
    effective_fertility_p,
    genotypes_p = c(0.25,0.5,0.25),
    genotypes_N = c(0,0,10),
    only_Fe_Male = FALSE,
    fe_male_p = 0.5,
    litter_mean = NULL,
    litter_sd = NULL,
    binomial_p=NULL,
    offsprings_n_sample = NULL, # should be a large vector, n of offs for a single mouse
    method = "poisson" # object of breedingClass
){
    stopifnot(length(genotypes_p)==length(genotypes_N))
    stopifnot(sum(genotypes_p)==1)
    stopifnot(all(genotypes_N>=0) & sum(genotypes_N)>0)
    stopifnot(confidence_p > 0 & confidence_p < 1)
    stopifnot(effective_fertility_p > 0 & effective_fertility_p <= 1)
    
    # additional requirement: by gender
    stopifnot(is.logical(only_Fe_Male))
    stopifnot(fe_male_p > 0 & fe_male_p < 1)
    
    stopifnot(is.null(litter_mean) || litter_mean > 0)
    stopifnot(is.null(litter_sd) || litter_sd > 0)
    stopifnot(is.null(binomial_p) || (binomial_p > 0 & binomial_p <=1))
    
    stopifnot(method %in% c("festing", "binomial", "empirical", "poisson"))
    
    
    # object for the distribution of offsprings for 1 mother
    if(method=="poisson"){
        doof1 <- generate_poisson_doof1(
            litter_mean = litter_mean, 
            effective_fertility_p = effective_fertility_p)
    }
    if(method=="binomial"){
        if(is.null(binomial_p))
        {
          binomial_p=0.5
          warning("No input in binomial_p => Binomial model fitted with p=0.5")
        }
        doof1 <- generate_binomial_doof1(
            litter_mean = litter_mean, 
            binomial_p = binomial_p, 
            effective_fertility_p = effective_fertility_p)
    }
    if(method=="empirical"){
        if(is.null(offsprings_n_sample)){
            errorCondition(message="Empirical sample for the number of 
            offsprings produced by 1 mouse is needed to use the 
            method=empirical")
      }
        doof1 <- generate_empirical_doof1(
            effective_fertility_p = effective_fertility_p,
            offsprings_n_sample =  offsprings_n_sample)
    }
    if(method=="festing"){
        errorCondition(
          message="No genotype-specific calculation for method festing")
    }
    

    rebre <- conv_k_search(  
      confidence_p=confidence_p,
      doof1=doof1,
      genotypes_N=genotypes_N,
      genotypes_p=genotypes_p)    
    
    
    ################# class  ################
    
    breesetup <- breedingMulti(
      required_breedings = rebre,
      confidence_p = confidence_p, 
      fertility_p = effective_fertility_p, 
      genotypes_p = genotypes_p, 
      genotypes_N = genotypes_N, 
      litter_mean = litter_mean, 
      doof1_obj = doof1,
      method = "poisson")
    
    ################# class  ################
    
    
    return(breesetup)

}

generate_poisson_doof1 <- function(litter_mean, 
                                   effective_fertility_p
){
    almost_certain_positive_support <- seq(
        1,
        round(qpois(p = 1-0.1^10, lambda = litter_mean)), 
        1)
    freqs <- dpois(x = almost_certain_positive_support, 
                   lambda = litter_mean)
    freqs <- freqs/sum(freqs)
    supp1 = as.numeric(c(0, almost_certain_positive_support))
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    stopifnot(all.equal( sum(prob1), 1 ) )
    # distribution of offsprings for 1 mother
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    return(doof1)
}


generate_binomial_doof1 <- function(
    litter_mean, 
    binomial_p, 
    effective_fertility_p
){
    binomialSize = round(litter_mean/binomial_p)
    positive_support <- seq(1,round(litter_mean/binomial_p), 1)
    freqs <- dbinom(
        x = positive_support, # only positive values
        size = binomialSize, 
        prob = binomial_p)
    freqs <- freqs/sum(freqs)
    supp1 = as.numeric(c(0, positive_support)) # full support
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    # distribution of offsprings for 1 mother
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    return(doof1)
}

generate_empirical_doof1 <- function(
    effective_fertility_p = effective_fertility_p, 
    offsprings_n_sample = offsprings_n_sample
){
    stopifnot(all(offsprings_n_sample >= 0))
    stopifnot(all(is.wholenumber(offsprings_n_sample)))
  
    offsprings_n_sample <- offsprings_n_sample[which(offsprings_n_sample!=0)]
    if(length(offsprings_n_sample) < 100){
        warning("Empirical sample too small? Less than 100 positive values.")
    }

    freqs <- table(offsprings_n_sample)/length(offsprings_n_sample)
    supp1 = as.numeric(c(0, names(freqs)))
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    stopifnot(sum(prob1)==1)
    
    # distribution of offsprings for 1 mother
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    return(doof1)
    
}
