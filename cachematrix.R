#The following two functions are used to cache a matrix and compute it's inverse. 
#If the original matrix has already been inverted it won't be computed again but
#the result wil be taken out of the cache.

#The function makeCacheMatrix is constructed to cache a matrix and it's inverse matrix. 
#For this reason the function departs in four subfunctions (set, get, setinverse, getinverse). 
#The makeCachefunctions takes as argument an invertable matrix. The tasks of the four 
#subfunctions are the following: 
#set sets the value of the original matrix, get caches this matrix. The same is true for
#setinverse and getinverse. All this informations are then stored in a list.

makeCacheMatrix <- function(x=matrix()) {
        inv <- NULL
        set <- function(y) {
                inv <<- NULL
                x <<- y
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#The function cacheSolve invertes a matrix. If the inversion has already been computed, 
#the function will print "getting cached data" and return the cached inverted matrix. 
#Otherwise it computes the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                print("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse <- inv
        return(inv)
}