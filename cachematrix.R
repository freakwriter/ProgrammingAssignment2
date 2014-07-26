## This set of functions will cache a matrix object and can compute
## its inverse; will continue to use cached values unless object
## has been changed. makeCacheMatrix handles the initial function set-up
## while cacheSolve creates inverse and handles checking the cache

## makeCacheMatrix function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ##set v to NULL in case it already exists.
    v <- NULL
    ## set function to set a new value for the underlying matrix
    # this invalidates the cached matrix, v
    set <- function(y) {
        ## Use <<- to modify in this environment only; set x to y
        x <<- y
        ##reset v to NULL since cached value is now invalid
        v <<- NULL
    }
    ## create get function, which will return value of x
    get <- function() x
    ## create setinverse function which can be called on matrix
    ## will return the value of v, where v is assigned value from
    ##the setinverse function call
    setinverse <- function(z) v <<- z
    ## create getinverse function which can be called on matrix;
    ## it will return the value assigned to v
    getinverse <- function() v
    ## establish the list of functions that can now be called
    ##in the public environment. All variables not listed here
    ## remain internal to makeCacheMatrix and cannot be called explicitly.
    ## Below list can be called using v$list_name; e.g. v$setinverse()
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has
## not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## assign variable v for makeCacheMatrix the getinverse function
    v <- x$getinverse()
    ## now check if v is null; if it is not, data has been cached
    ## simply return the already calculated value
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    ## if v's not null, data has not been cached
    ## use get function from makeCacheMatrix
    data <- x$get()
    ## calculate and assign inverse of matrix to variable v
    v <- solve(data, ...)
    x$setinverse(v)
    v
}