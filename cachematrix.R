## This function will:
#Set the value of a matrix
#Get the value of that matrix
#Create the inverse of that matrix
#Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, 	
        	 get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

##This function will:
#Return a matrix that is the inverse of the input 'x'
#If a cached copy exists, return it

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("fetching cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
