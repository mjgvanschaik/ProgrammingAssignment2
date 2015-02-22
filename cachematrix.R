# ##############################################################################
## makeCacheMatrix
# Creates a special "matrix object" in R, which is actually a list. This list
# contains 4 functions.
#
# Args:
#   x: a matrix which should have an inverse. 

# Returns:
#   A list with 4 functions: $set, $get, $setsolve, $getsolve.
# 
# ##############################################################################
# cacheSolve
# Returns a matrix that is the inverse of 'x'.
#
# Args:
#   x: a 'list' object created using the makeCacheMatrix function. 
#
# Returns:
#   If this is the first time cacheSolve() is used on the object, the inverse 
#   matrix will be computed (this can take a while) and returned. If this is
#   not the first time cacheSolve() is used on that object, it will state a 
#   message and return the inverse matrix from cache (this will be very fast).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

# ##############################################################################
## Demonstration of its use:
# Randomly generate a matrix with numbers. Set 'size' to specify size.
size <- 10
mymatrix <- makeCacheMatrix(matrix(rnorm(size^2), ncol=size)) 

# Show the randomly generated matrix.
mymatrix$get() 

# find inverse matrix.
cacheSolve(mymatrix)  

# Second time we ask, it's pulled from cache instead of computed again. 
cacheSolve(mymatrix) 

# multiplying original matrix with its inverse results in Identity Matrix.
# round() is used to avoid scientific notation. 
round(mymatrix$get() %*% cacheSolve(mymatrix)) 
