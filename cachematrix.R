## The first function takes an invertible matrix and creates a list ("vector") of functions
## which set and retrieve the value of the matrix and its inverse for further reference.
## On the first call of the second function the "stored" or "global" value of the matrix inverse
## will be found empty, so the inverse will be calcultated. 
## On subsequent calls of the second function the value set will be retrieved instead.

## makeCacheMatrix() - takes the matrix "x" in question, sets the inverse to zero and stores these values in the parent environment, 
##                   - returns the list of assignemnt and retrieval functions to be used in cacheSovle()
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL                                     # sets initial value of the inverse matrix
        set<-function(y){                           # defines a function for assigning global values
            x<<-y
            m<<-NULL
            }
        get<-function() x                           # defines a function for retrieving matrix, x
        setinverse<-function(solve) m<<- solve      # defines a function for storing global value of inverse of x
        getinverse<-function() m                    # defines a function for retrieving inverse, m, of matrix, x
    list(set=set, get=get,                          # returns a list of function for the matrix, x
         setinverse=setinverse, getinverse=getinverse)
    }

## CacheSolve() returns the inverse, m, of a matrix 

cacheSolve <- function(x, ...) {
        m<-x$getinverse()                           # retrieves the stored "inverse" m stored in the list makeCacheMatric(x)
        if(!is.null(m)){                            # tests whether the stored inverse is NULL, and 
            message("getting cached data")          # if not, returns it, with the message, the required inverse.           
            return(m)
            }
        data<-x$get()                               # otherwise retrieves the value of the stored matrix, X, from the list of functions passed
    m<-solve(data, ...)                             # evaluates its inverse, m
    x$setinverse(m)                                 # assigns this value to the global variable, m, by means of the function x$getinverse() 
                                                    # passed from the function makeCacheMatrix()
    m                                               # returns the value of m
}