## The makeCacheMatrix function creates an instance of a square (invertible) matrix
## and providesvarious methods to set and return the elements of that matrix object
##as well as save and return the inverse of that matrix object.
 
## The cacheSolve function uses the public functions of makeCacheMatrix function to calculate
## and return the inverse of a matrix provided by the user. If the inverse has already been
## calculated, then this function returns its cached value instead of recalculating the inverse.


# makeCacheMatrix takes a matrix saved in the private variable 'x'
# and returns a list of public functions used to set and get the values of a matrix
# as well as calculate, save(cache), and return the inverse of matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the inverse to NULL
    i <- NULL
    
    # set new values of matrix and clear cached inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # function to return matrix elements
    get <- function() x
    
    # calculate the inverse of matrix
    setinverse <- function(solve) i <<- solve
    
    # return the inverse of matrix; will be NULL if cacheSolve has
    # not been called or new matrix values have been set 
    getinverse <- function() i
    
    # return value of makeCacheMatrix is a list of functions that are public
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


# cacheSolve takes an instance of a matrix created by makeCacheMatrix
# and returns either the cached inverse of that matrix if the matrix
# has not changed and the inverse was already calculated, or returns the
# freshly calculated inverse and also caches it for future calls
cacheSolve <- function(x, ...) {
    
    # get the inverse of matrix 'x'
    i <- x$getinverse()
    
    # if the inverse of matrix has been calculated and saved,
    # and the matrix elements have not changed,
    # return the cached value of inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # the following lines are executed only if the inverse
    # value is NULL
    
    # get the matrix elements of 'x' and assign to 'data'
    data <- x$get()
    
    # calculated the inverse of matrix
    i <- solve(data, ...)
    
    # save (cache) the calculated inverse so it does
    # not have to be recalculated every time cacheSolve is called
    x$setinverse(i)
    
    # return the cached inverse of matrix 'x'
    i
}
