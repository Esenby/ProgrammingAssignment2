## The first function takes an invertible matrix and creates a list ("vector") of functions
## which set and retrieve the value of the matrix and its inverse for further reference.
## On the first call of the second function the "stored" value (in the parent environment)of the matrix inverse 
## will be found empty, so the inverse will be calculated. 
## On subsequent calls of the second function the value set will be retrieved instead.

## makeCacheMatrix() - takes the matrix "x" in question, sets the inverse to zero and stores these values in the parent environment, 
##                   - returns the list of assignment and retrieval functions to be used in cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL                                     # Sets initial value of the inverse matrix.
        set<-function(y){                           # Defines a function for assigning parent environment values,
            x<<-y
            m<<-NULL
            }
        get<-function() x                           # defines a function for retrieving matrix, x,
        setinverse<-function(solve) m<<- solve      # defines a function for storing parent environment value of inverse of x,
        getinverse<-function() m                    # defines a function for retrieving inverse, m, of matrix, x, and
    list(set=set, get=get,                          # returns a list of function for the matrix, x
         setinverse=setinverse, getinverse=getinverse)
    }

## CacheSolve() returns the inverse, m, of a matrix 

cacheSolve <- function(x, ...) {
        m<-x$getinverse()                           # Retrieves the stored "inverse" m from the list makeCacheMatric(x),
        if(!is.null(m)){                            # tests whether the stored inverse is NULL, and 
            message("getting cached data")          # if not, sends a message and            
            return(m)                               # returns the value the stored inverse matrix, m.    
            }
        data<-x$get()                               # Otherwise retrieves the value of the stored matrix, X, from the list of functions passed,
    m<-solve(data, ...)                             # evaluates its inverse, m,
    x$setinverse(m)                                 # assigns this value to the parent environment variable, m, by means of the function x$getinverse()...
                                                    # ...passed from the function makeCacheMatrix(), and
    m                                               # returns the value of m.
}