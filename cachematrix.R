## Put comments here that give an overall description of what your
## functions do

## This function creates the list of 4 "basic" function get, set, getSolve 
## and setSolve enabling to use the "special" matrix and its cache solve

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## This function is returning the 'inverse' of a "special" matrix 
## If already stored Then value stored in cache,
## Else calculating, storing in cache and returning it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached solve")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
