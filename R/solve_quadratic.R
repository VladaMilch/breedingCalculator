# solve ax^2+bx+c=0
solve_quadratic <- function(a,b,c){
  if(delta(a,b,c) > 0){ # D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    res = c(x_1,x_2)
    return(res)
  }
  else if(delta(a,b,c) == 0){ # D=0
    res = -b/(2*a)
    return(res)
  }
  else{return(NULL)} # D<0
}

# D
delta<-function(a,b,c){
  b^2-4*a*c
}