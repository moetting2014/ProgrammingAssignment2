## Below are 2 functions. The first function, 'makeCacheMatrix', accepts as an input a matrix and 
## creates a list of 4 elements that store various aspects of the inputted matrix. 
## The second function, 'cacheSolve', receives as inputs, the various aspects of the inputted 
## matrix. According to the value that is stored in a particular aspect, this function will either
## (1) compute the inverse of the inputted matrix and then cache it, or (2) retrieve the cached valued 
## of the already computed inverse of the inputted matrix.

# Function to accept a Matrix as an input
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## 'SET' is an element of a list
        ## It will allow the user to input a Matrix
        set <- function(y){
            x <<- y
            m <<- NULL
        }
        
        ## 'GET' is an element of a list
        ## It will be the location of the Matrix 
        ## that a user has inputted
        get <- function() x
        
        ## 'SETINVERSE' is an element of a list
        ## It will compute the inverse of a matrix and 
        ## store it in the global variable 'm'
        setinverse <- function(solve) m <<- solve(x)
        
        ## 'GETINVERSE' is an element of a list
        ## It will be the location of the inverse of the Matrix
        ## that the user inputted 
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to compute the Inverse of a Matrix and cache it
cacheSolve <- function(x, ...){
    
    ## 'getinverse' is read from the makeMatrix function
    ## it will return either a NULL or inverse matrix value
    ## and store that value in the local variable 'm'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ## If 'm' is NULL, then the inputted matrix is stored
    ## in the local variable 'data', and the inverse of the 
    ## matrix is computed and stored in the local variable 'm'
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
