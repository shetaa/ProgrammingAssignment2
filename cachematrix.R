## makeCacheMatrix is used for initialization (like a constructor)
## cacheSolve returns the inverse of the matrix 
##      from the cache if available or computes and returns it


## 
##  Function makeCacheMatrix()
## 
## in this funciton the matrix is initialized and the scope variable are set
##  which helps track if the inverse matrix is available in the cache or not

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve= setsolve,
         getsolve= getsolve)
}

## 
##  Function cacheSolve()
## 
## cacheSolve taken a square inversible matrix as input
## 
## x$getsolve() indicates if the inverse of x(i.e. solve(x) ) 
##      is already in cache or not
## if yes (i.e. return values is null ) then return result from cache x$get() 
## if no (i.e. its null ) then calculate the inverse using solve(), 
###    store it in cache and then reutrn it

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        ## Return the cached inverse of matrix 'x'
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    ## Return the computed inverse of matrix 'x'
    s
}

##
## Sample Runs
##
# > source("cachematrix.R")
# > mat<-makeCacheMatrix(matrix(floor(runif(16000000,1,3200)),4000,4000))
# > str(cacheSolve(mat))
# num [1:4000, 1:4000] -1.81e-05 7.68e-06 4.38e-06 1.33e-05 1.41e-06 ...
# > str(cacheSolve(mat))
# getting cached data
# num [1:4000, 1:4000] -1.81e-05 7.68e-06 4.38e-06 1.33e-05 1.41e-06 ...
# > mat<-makeCacheMatrix(matrix(floor(runif(16000000,60,3200)),4000,4000))
# > str(cacheSolve(mat))
# num [1:4000, 1:4000] 3.47e-05 -9.12e-05 9.30e-06 2.32e-05 -2.33e-05 ...
# > str(cacheSolve(mat))
# getting cached data
# num [1:4000, 1:4000] 3.47e-05 -9.12e-05 9.30e-06 2.32e-05 -2.33e-05 ...
# > 
    
    
    
    
    












