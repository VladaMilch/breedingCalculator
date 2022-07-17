#' Minimal number of breedings: Poisson 
#' 
#' Calculates the minimal number of animals to be bred in order
#' to achieve a given number of offsprings with a certain confidence 
#' under the following assumptions:
#' Poisson-distributed number of offsprings per one animal,
#' Binomially distributed number of fertile animals
#'
#' @param confidence_p desired confidence level (probability to achieve 
#' n_needed offsprings), a number between 0 and 1, excluding 1
#' @param fertility_p how likely is an animal to be fertile, a number between 0 and 1, including 1
#' @param n_needed desired total number of offsprings 
#' @param litter_mean average number of offsprings one fertile animal produces
#'
#' @return returns one value: The minimal number of breedings
#'
calculate_needed_breedings_poisson <-   function(
    confidence_p,
    fertility_p,
    n_needed,
    litter_mean,
    search_strarting_point = 1
){
    freqs_r <- dpois(x = seq(1,round(4*litter_mean), 1), lambda = litter_mean)
    freqs <- freqs_r/sum(freqs_r)
    supp1 = as.numeric(c(0, seq(1,round(4*litter_mean))))
    prob1 <- c(1-fertility_p, fertility_p*freqs)
    stopifnot(   abs(1-sum(prob1)) < 0.1^5   )
    
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    res <- conv_k_search(
      confidence_p=confidence_p, 
      doof1 = doof1, 
      genotypes_N = c(n_needed,0), 
      genotypes_p = c(1,0))
    return(res)
    # doofN_quantile = 0
    # k=1
    # while(doofN_quantile < n_needed){
    #   doofN_quantile <- distr::convpow(doof1, N=k)@q(1-confidence_p)
    #   k <- k+1
    # }
    # return(k-1)
}
  
