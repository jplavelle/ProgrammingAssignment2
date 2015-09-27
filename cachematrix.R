## The makeCacheMatrix function and cacheSolve function work together to
## reduce the amount of time and processing power required to produce the
## inverse of a matrix. Rather than computing the inverse of the same matrix
## multiple times, this pair of functions checks if the inverse for the matrix
## has already been computed. If so, the value is retrieved from the cache 
## instead of running a new set of calculations. If not, the inverse matrix is
## calculated, and the result is saved in the cache and returned to the user.

## The makeCacheMatrix function contains four functions that the cacheSolve
## function calls upon as needed to either return an existing value or, if one
## does not exist, take the calcuated value passed to it from cacheSolve and
## save it in the cache.

makeCacheMatrix <- function(x = matrix()) {
        Minv<-NULL
        set<-function(y){
                x<<-y
                Minv<<-NULL
        }
        get<-function()x
        setmatrix<-function(inv)Minv<<-inv
        getmatrix<-function()Minv
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## The user calls the cacheSolve function and enters a matrix as the argument.
## This function beings by creating an object 'z' that enables subsetting of
## the makeCacheMatrix function. The cacheSolve function then calls the
## relevant subsets to check if a value exists in the cache, return any
## existing value, and, if no value is already saved, calculate the inverse and
## save the result to cache while returning the value to the user.


cacheSolve <- function(x, ...) {
        z<-makeCacheMatrix()
        Minv<-z$getmatrix()
        if(!is.null(Minv)){
                message("Retrieving cached data.")
                return(Minv)
        }
        data<-z$get()
        Minv<-solve(x,...)
        z$setmatrix(Minv)
        Minv
}