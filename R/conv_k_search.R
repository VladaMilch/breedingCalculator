# search of the min k such that conv(k, doof1) produces n_needed 
# with confidence >= required confidence
conv_k_search <- function(
  confidence_p,
  doof1,
  genotypes_N,
  genotypes_p,
  largest_step_size = 32 # must be 2^
){
  # choose the starting k wisely: Needed for large breedings
  mm = max(median(doof1@r(10000)), 1) #sometimes median=0...
  # k = max(1, 
  #         floor(sum(genotypes_N)/mm) - largest_step_size)
  k=1
  step_size = largest_step_size

  # 1 breeding is enough (k=1)
  confi_low <- calculate_confi(k=k, 
                               doof1=doof1, 
                               genotypes_N=genotypes_N, 
                               genotypes_p=genotypes_p)
  if(confi_low >= confidence_p)
  {
    return(k)
  }
  
  # more than 1 breedings needed (k>1)
  while(confi_low < confidence_p){
    confi_hig <- calculate_confi(k=k+step_size, 
                                   doof1=doof1, 
                                   genotypes_N=genotypes_N, 
                                   genotypes_p=genotypes_p)
    
    if(confi_hig < confidence_p){
        # keep jumping
        confi_low <- confi_hig
        k <- k+step_size
    }else if(step_size > 1){
        # confi_low stays as it is
        # confi_high by half step
        step_size <- max(step_size/2,1)
        # cat("step size decreased")
    }else if(step_size==1){
      # take k corresponding to confi_high
      break
    }
  }
  res = k+step_size
  aa = calculate_confi(k=res, 
                       doof1=doof1, 
                       genotypes_N=genotypes_N, 
                       genotypes_p=genotypes_p)
  bb = calculate_confi(k=res-1, 
                       doof1=doof1, 
                       genotypes_N=genotypes_N, 
                       genotypes_p=genotypes_p)
  stopifnot( (aa >= confidence_p & bb <confidence_p) || aa >= 1)
  return(res)
}

calculate_confi <- function(
  k, # breedings
  doof1, # offspring distribution for 1 mouse
  genotypes_N, # required Ns
  genotypes_p # required Ps
){
  doofK <- distr::convpow(doof1, N=k)
  
  if(sum(genotypes_N-1) >= max(doofK@support))
  { # if required more than theoretically can be born
    # confidence = 0
    return(0)
  }
  
  # input n for the multinomial (total trials)
  total_offs_seq <- seq(
    from = max(sum(genotypes_N-1),0), 
    to = max(doofK@support), 
    by = 1)
  # 
  p_genotypes_by_total <- pmultinom::pmultinom(
    size = total_offs_seq, 
    lower = genotypes_N-1, # because ksi > lower (not >=)
    probs = genotypes_p, 
    method = "exact")
  d_total <- distr::d(doofK)(total_offs_seq) # pmf needed born of total
  confi <- sum(p_genotypes_by_total*d_total) # sum_total (p_total * p_needed_born)
  return(confi)
}

