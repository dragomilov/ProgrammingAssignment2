## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set_matrix <- function(y) {
        x <<- y
        inv <<- NULL ## set NULL to cache when reset the x matrix
    }
    get_matrix <- function() x
    set_inverse <- function(s) inv <<- s
    get_inverse <- function() inv
    list(	set = set_matrix, 
					get = get_matrix,
					set_inverse = set_inverse,
					get_inverse = get_inverse
				)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## The inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()	
    if(!is.null(inv)) {
        message("getting inversed matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inverse(inv)
    inv
}
