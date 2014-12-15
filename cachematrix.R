## Function that takes a matrix as its argument
## and returns a list of functions that allow
## access to the matrix values and its inverse

makeCacheMatrix <- function(x = matrix()) {##x is a numeric matrix
        inv<-NULL ##sets the inverse to NULL
        set<-function(y){ ##defines the 'set' function in the list
            x<<-y
            inv<<-NULL
        }
        get<-function() x ##get function returns the values x
        setinv<-function(inverse) inv<<-inverse ##sets inverse, called by the cachesolve function
        getinv<-function() inv ##returns the stored inverse matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv) ##returns the list of functions
    }
}


##Function to invert the cached matrix, given the matrix in 'list' form

cachesolve<-function(x,...){
    inv<-x$getinv() ##retrieves the inverse from the cached matrix
    if(!is.null(inv)){ ##if already calculated, returns the stored value
        message("getting cached result")
        return(inv)
    }
    data<-x$get() ##retrieves the matrix
    inv<-solve(data) ##inverts it
    x$setinv(inv) ##sets the inverse matrix in the cache
    inv ##returns the inverse matrix
}
