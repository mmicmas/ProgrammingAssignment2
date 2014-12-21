## The followning method creates matrix that will be cached. Cache is emptied when set is called
## Returns the list containsing method of set / get Inversed matrix 
makeCacheMatrix <- function(x = matrix()) {
  
    inversed <- NULL

    set <- function(newMatrix) {
        x <<- newMatrix
        inversed <<- NULL 
    }

    get <- function() x

    # set inverse of matrix
    setInversed <- function(inverse) inversed <<- inverse

    # get inverse of matrix
    getInversed <- function() inversed

    list(set = set, get = get,
        setInversed = setInversed,
        getInversed = getInversed)
}


## Inversing the matrix is heavy and time consuming operation. Therefore, if possible, it is cached by the methof above
## which does the inversion. The following method uses the methods from the makeCacheMatrix. It uses the scoped vars between two methods
cacheSolve <- function(x, ...) {
    inversed <- x$getInversed()    
    
    if(!is.null(inversed)) {
        ##get cached and return
        return(inversed)
    }

    data <- x$get()

	## no cahce was available, so do it now and return computed matrix
    inversed <- solve(data, ...)	
    x$setInversed(inversed)
    inversed
}