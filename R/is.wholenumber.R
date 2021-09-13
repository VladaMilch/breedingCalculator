# checks if the number is whole with a certain tolerance
# is.integer() does not fit for this purpose
is.wholenumber <-function(x, 
                          tol = .Machine$double.eps^0.5
){
    abs(x - round(x)) < tol
}
