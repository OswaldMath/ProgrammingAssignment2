## These functions create a special object that stores a matrix and 
## caches its inverse to avoid redundant computations.

## This function creates a special "matrix" object, which is a list containing
## functions to set and get the matrix, as well as set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  new_matrix <- function(new_m){
    x <<- new_m
    inv <<- NULL
  }
  
  ver_matrix <- function() x
  
  calc_inv <- function(valor_inv) inv <<- valor_inv
  
  ver_inv <- function() inv
  
  list( new_matrix = new_matrix,
        ver_matrix = ver_matrix,
        calc_inv   = calc_inv,
        ver_inv    = ver_inv
  )
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$ver_inv()
  
  if (!is.null(inv)){
    print("El valor almacenado de la inversa es:")
    return(inv)
  }
  
  obt_matrix <- x$ver_matrix()
  inv <- solve(obt_matrix, ...)
  x$calc_inv(inv)
  inv
}
