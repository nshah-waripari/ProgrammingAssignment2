## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y){
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mat <<- inverse
        getinverse <- function() mat
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## this function assumes the given matrix is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getinverse()
        if(!is.null(inv_mat)){
                message("getting cached data")
                return(inv_mat)
        }
        data <-x$get()
        inv_mat <- solve(data)
        x$setinverse(inv_mat)
        inv_mat
}
