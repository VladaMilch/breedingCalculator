# fit binomial distribution such that 
# the mean fits, and then add to the zeroes such that
# the fertility fits

# one 'fertile' mouse gives birth to np offstrings on average
# let's say we choose the Binomial distribution with the highest possible 
# variance (np(1-p)), achieved when p=0.5
# then, n=size = 2*round(m)

calculate_needed_breedings_binomial <- function(
    confidence_p,
    fertility_p,
    n_needed,
    litter_mean,
    binomial_p=0.5
){
    search_interval <- c(1:n_needed)
    confidence_4_K <- function(K, 
                               needed_offs, 
                               fertility, 
                               litter_mean,
                               single_mouse_p=binomial_p){
        k_values <- seq(0, K, 1)
        sum(pbinom(
            q = needed_offs-1, 
            size = k_values*litter_mean/single_mouse_p, 
            prob = single_mouse_p, 
            lower.tail = F)*
                dbinom(
                    k_values, 
                    size = K, 
                    prob = fertility-dbinom(0, 
                                            size = litter_mean/single_mouse_p, 
                                            prob = single_mouse_p)))
    }
    
    Nbreedings <- search_interval[
        min(which(sapply(search_interval, FUN = function(x){
            confidence_4_K(K = x, 
                       needed_offs = n_needed, 
                       fertility = fertility_p, 
                       litter_mean = litter_mean)   >=     confidence_p
    })))]
    return(Nbreedings)

} 

#calculate_needed_breedings_binomial(confidence_p = 0.95, fertility_p = 0.6, n_needed = 50, litter_mean = 7)




# fert = 0.6
# litter_mean = 7
# 
# barplot(dbinom(x = c(0:50), size = 50, prob = 0.5))
# 
# doffsprings <- function(value, litter_mean, fertility){
#     stopifnot(all(value >=0))
#     n = round(litter_mean)*2
#     normalization_coeff <- fertility/(1-dbinom(x=0, size = n, prob = 0.5))
#     res = ifelse(value==0, 1-fertility, dbinom(x=value, size = n, prob = 0.5)*normalization_coeff)
#     return(res)
# }

