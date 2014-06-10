## As the assignment requires, 
## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
## cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.



makeCacheMatrix <- function(x) {
        
        ## This function is a closure function which creates a special Matrix object that can cache its inverse
        ## The input to the function is a Matrix        
        
        ## It is assumed that the matrix that is passed to the function is inversable. i.e. it is a N x N matrix with equal
        ## number of rows and columns. No error handling subroutine is implemented to handle this.
        
        ## the output of this function is a list containing pointers to the following functioins
        ## Function 1: get() : Returns the Matrix
        ## Function 2: set() : Sets the value of the matrix from formal parameter passed
        ## Function 3: getInverse() : Returns the value of Inverse of a matrix
        ## Function 4: setInverse() : Sets the value of Inverse of a matrix
        
        ## Initialize the object (supposed to hold hte inverse of a matrix )
        m <- NULL
        
        ## Worker function that returns the value of the created matrix
        get <- function() x
        
        ## Worker function that sets that creates the matrix and sets cache value to that matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Worker function that returns the inverse of the matrix
        getInverse <- function() m
        
        ## Worker function that computes and sets the invers of the matrix
        setInverse <- function(solve) m <<- solve
        
        ## This is the return value of this closure function which is a list of functions defined above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x,...) {
        
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function above. 
        ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will 
        ## retrieve the inverse from the cache and return that value.
        
        ## This function takes a matrix object as input. Ideally the Matrix object is created by makeCache Matrix.
        ## Using the Matri handle as object it calls the functions of makeCacheMatrix as appropriate and sets the value 
        ## of 'm' which finally contains the inverse of the matrix
        
        ## Set the value of 'm' object to the inverse of the matrix created by makeCacheMatrix
        m <- x$getInverse()
        
        ## If the value of matrix inverse is already in cache, return the cache value instead of computing it again
        if(!is.null(m)) {
                message("Getting Cached Data")
                return(m)
        }
        
        ## Get the value of Matrix created by makeCacheMatrix function
        data <- x$get()
        
        ## Compute the inverse of the Matrix and store it in object 'm'
        m <- solve(data)
        
        ## Set the newly computed inverse matrix value and also store it in cache
        x$setInverse(m)
        
        # Return the inverse of the matrix
        m
}