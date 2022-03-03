makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

matrix<- makeCacheMatrix(matrix(2:5, 2,2))
matrix$getinverse()

cacheSolve(matrix)

matrix1<- makeCacheMatrix(matrix(c(6,7,3,8,1,6),2,2))
matrix1$get()

matrix1$getinverse()

cacheSolve(matrix1)
matrix1$getinverse()
