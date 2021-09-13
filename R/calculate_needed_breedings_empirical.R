calculate_needed_breedings_empirical <- function(
    confidence_p = confidence_p, 
    effective_fertility_p = effective_fertility_p, 
    n_needed = n_needed, 
    offsprings_n_sample = offsprings_n_sample
){
    offsprings_n_sample <- offsprings_n_sample[which(offsprings_n_sample!=0)]
    freqs <- table(offsprings_n_sample)/length(offsprings_n_sample)
    supp1 = as.numeric(c(0, names(freqs)))
    prob1 <- c(1-effective_fertility_p, effective_fertility_p*freqs)
    
    tol = 0.1^5
    stopifnot( abs( sum(prob1)-1) < tol )
    
    #search_interval <- seq(1,10)
    doof1 <- distr::DiscreteDistribution(supp = supp1, prob = prob1)
    doofN_quantile = 0
    k=1
    # optim( FUN = function(k){
    #     doofN_quantile <- distr::convpow(doof1, N=k)@q(1-confidence_p)
    #     abs(doofN_quantile - n_needed)
    # })
    while(doofN_quantile < n_needed){
        #doofN_quantile <- sapply(search_interval, FUN = function(k){distr::convpow(doof1, N=k)@q(confidence_p)})
        doofN_quantile <- distr::convpow(doof1, N=k)@q(1-confidence_p)
        k <- k+1
    }
    return(k-1)
}

















# 
# library("DEoptim")
# ?opti
# calculate_needed_breedings_empirical(confidence_p = 0.9, 
#                                      effective_fertility_p = 0.7, 
#                                      n_needed = 100, 
#                                      offsprings_n_sample = c(1,1,1,2,3,4,5,6,7,8,8,8,8))
