## esta codigo permite almacenar en cache la inversa de una matrix 
## por medio de cuatro funciones internas de la función makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(x){
                x <<- x
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set , get = get,
             setinverse = setinverse, getinverse = getinverse)
        return(x)

}

## Este codigo toma una matrix y calcula su inversa, si su inversa 
## ya ha sido calculada y esta encache la recupera.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
