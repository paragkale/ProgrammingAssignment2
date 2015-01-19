## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(my_mat = matrix()) {
    # Set the cache to NULL
    inv <- NULL 
    
    # when the values are set through the set function
    # reset the cached value for the inverse
    set <- function(y) {
        my_mat <<- y
        inv <<- NULL
    }
    
    # 'override' the get function
    get <- function() my_mat
    
    # compute the inverse of the underlying matrix
    setinv <- function(solve) inv <<- solve
    
    # return the cached inverse
    getinv <- function() inv
    
    # havent understood, why we have to do this?
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# take the special matrix and compute the inverse 
# if not already computed
cacheSolve <- function(inp_special_matrix, ...) {
    ## Return a matrix that is the inverse of 'inp_special_matrix'
    inv_temp <- inp_special_matrix$getinv()
    
    # if inverse is already computed return it
    if(!is.null(inv_temp)) {
        message("getting cached data")
        return(inv_temp)
    }
    
    # inverse wasnt computed so compute and cache it
    data <- inp_special_matrix$get()
    inv_temp <- solve(data,...)
    inp_special_matrix$setinv(inv_temp)
    inv_temp
}

