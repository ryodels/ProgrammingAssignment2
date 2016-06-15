## The two function allow to calculate the inverse of a given
## matrix and cache its result for future use.

## The function makeCacheMatrix defines an special 
## object, which is a list of functions that allows to 'SET' 
#  the matrix to be inverted, 'GET' its value, cache (setInverse) 
## and retrieve (getInverse) its inverse.
## Additionally, to be able to identify if the cached value comes
## from the same matrix (in case it was defined as an object),
## the matrix 'name' can also be retrieved.

makeCacheMatrix <- function(x = matrix()) {
    nameof <- deparse(substitute(x))
    name <- function() nameof
    Inverse <- NULL
    set <- function(y) {
        x <<- y
        Inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) Inverse <<- solve
    getInverse <- function() Inverse
    list(name=name, set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the special 
## matrix created by the above function. However it first checks
## to see if the inverse has been already calculated.  If so, it 
## gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse and sets its value in the 
## through `setmean` function.
## To avoid cacheSolve being called without updating the value 
## of the matrix of interest (something that would lead to have 
## an incorrect cached value of the inverse), a test is performed.
## If the value of the matrix supplied through makeCacheMatrix
## has changed, then its value is updated using 'set' function
## before further computations. If the matrix was supplied
## directly, without creating a named object, this test is skipped
## since there is no way to change the matrix without invoking
## makeCacheMatrix first.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matName <- get0( x$name() )
    if ( !is.null(matName) & 
         !identical(x$get(), matName) ) {
        x$set(matName)
        warning(paste("Matrix ", x$name() , 
                      " has changed. Calcutations has been updated",
                      sep=""))
    }
    
    Inverse <- x$getInverse()
    if(!is.null(Inverse)) {
        message("getting cached data")
        return(Inverse)
    }
    data <- x$get()
    Inverse <- solve(data, ...)
    x$setInverse(Inverse)
    Inverse
}
