makeCacheMatrix <- function(x = matrix()) ##his function creates a special "matrix" object that can cache its inverse.
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse ##stores the inverse to be called by other functions
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
{
        m <- x$getinverse()
        if(!is.null(m)) ##checks if the inverse has already been calculated
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}