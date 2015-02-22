## These functions manage the caching and retrieval of the inverse 
## of provided matrices 

## makeCacheMatrix will take a provided matrix and store it in a cached
## copy, and then it will compute the inverse of the provided matrix and
## store it also in the object for future retrieval (using cacheSolve).

makeCacheMatrix <- function(x = matrix()) {
    volteada <- NULL
    set <- function(y) {
        x <<- y
        volteada <<- NULL
    }
    get <- function() x
    setvolteada <- function(solve) volteada <<- solve
    getvolteada <- function() volteada
    list(set = set,
		 get = get,
		 setvolteada = setvolteada,
		 getvolteada = getvolteada)
}


## cacheSolve first verifies that it has the inverse of the provided matrix 
## (created with makeCacheMatrix). If the inverse doesn't exists, 
## cacheSolve will compute the inverse, but if the inverse already exists,
## cacheSolve will retrieve the inverse of the matrix from the cached
## object.

cacheSolve <- function(x, ...) {
	matriz <- x$getvolteada()
		if(!is.null(matriz)) {
			message("getting cached data")
			return(matriz)
		}
		data <- x$get()
		matriz <- solve(data, ...)
		x$setvolteada(matriz)
		matriz

}
