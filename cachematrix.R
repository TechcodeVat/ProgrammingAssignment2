## Takes in the matrix as input and gives the inverse of it as output
## If the matrix is repeated it code uses the cache memory to extract the inverse hence saving time.

## Takes matrix and gives inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)


}


## Checks if the matrix is repeated or not; 
##if repeated then pulls the inverse from cache memory or calls the earlier function to give inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached result")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}
