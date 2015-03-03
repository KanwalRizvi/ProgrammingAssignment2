## The following two functions calculate the inverse of a matrix and then store
## the inverse of the matrix so that the calculation does not need to be done again

## This function does no calculation but simply stores and provides values of variables

makeCacheMatrix <- function(x = matrix()) {
        #saving a variable name with no value
        m <- NULL
        
        # set is a funct, takes input var y and appoints it's value to x & Null to m in another env
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get function takes no input but returns x
        get <- function() x
        
        #sets takes value and sets m to that value 
        setinverse <- function(inverse) m <<- inverse
        
        #get inverse returns the value of m
        getinverse <- function() m
        
        #this function returns a list of set :  appoints x and m, get: x, setinverse: appoints m, getinverse: m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## This function calculates the inverse of the matrix and stores it so it can be retrived

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        #if the value of m stored is not null
        if(!is.null(m)) {
                
                #msg
                message("getting cached data")
                
                #returns stored value of m as output of function
                return(m)
        }
        
        #returns the matrix x
        data <- x$get()
        
        #calculating inverse and assigning value to m
        m <- solve(data, ...)
        
        #setting inverse value to m
        x$setinverse(m)
        
        #return m as output
        m
}
