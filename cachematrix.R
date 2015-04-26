## These two functions, makeCacheMatrix and cacheSolve, 
## will cache the inverse of a matrix


##  makeCacheMatrix function will create a special "matrix" object
##  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {        
 
        z <- NULL  # initialize z, this is where the inversed data will be stored
        
        set <- function(y)  {    
                
                x <<- y
                z <<- NULL  #initialize z to null, in case data had been cached
                           
        }
        
        get <- function() x  #get matrix
        
        setInverse <- function(solve)   z <<- solve  #set inverse matrix
        getInverse <- function() z  #return the inverse matrix
        
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)  #create list of functions
       
        
}

##  Compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...)  {

    
        z <- x$getInverse()                     # check x matrix's cache
        
        if(!is.null(z))  {                      # if there is a cache, inverse has been previously calculated
                message("getting cached data")  # send message - data is cached
                return(z)                       # return cached data
                
        }
        
        data <- x$get()                         # get the matrix used by makeCacheMatrix
        z <- solve(data, ...)                   # calculate the inverse of the matrix
        x$setInverse(z)                         # store the inverse matrix in cache
                              
}

