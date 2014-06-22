
makeCacheMatrix <- function(x = matrix()) {
        ## set initial matrix value
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## get the initial value
        get <- function() x
        ## set the inverse matrix method
        setsolve <- function(solve) s <<- solve
        ## get the inverse matrix method
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
