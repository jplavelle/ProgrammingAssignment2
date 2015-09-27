## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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