## Functions makeCacheMatrix and cacheSolve matrix demonstrate the utility of caching
## R vectors if they are computed repeatedly to reduce computation cycles


## makeCacheMatrix function a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverse) im <<- inverse
        getInverseMatrix <- function() im
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getInverseMatrix()
        if(!is.null(im)) {
                message("getting cached matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setInverseMatrix(im)
        im
}