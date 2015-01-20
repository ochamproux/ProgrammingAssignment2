## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## this function creates the list of 4 "basic" function get, set, getSolve 
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


## Write a short comment describing this function
##this function is giving the cache of a "special" matrix if already stored
##and calculating + storing it if not available

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
