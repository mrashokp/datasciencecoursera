## These functions will calculate an inverse of a matrix if it is not already stored in the cache. 

## This function will create a list of four functions:
#1. 'setmatrix' will create the function to set the value of matrix and inverse(inv for short)
#2. 'getmatrix' will create a function to get the matrix 
#3. 'setinv' will create a function to set/calculate the inverse of this matrix
#4. 'getinv' will create a function to get the value of inverse.
#Finally, all the results are stored as a list.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        setmatrix<-function(y){
                x<<-y
                inv<<-NULL
        }
        getmatrix<-function() x
        setinv<-function(solve) inv<<- solve
        getinv<-function() inv
        list(setmatrix=setmatrix, getmatrix=getmatrix,
             setinv=setinv,
             getinv=getinv)
}


## This function takes the above function as it's argument and checks whether the inverse value is stored in cache. If stored, gets the cached value and displays; if not, calculates it and displays.

cacheSolve <- function(x=makeCacheMatrix, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix<-x$getmatrix()
        inv<-solve(matrix,...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
