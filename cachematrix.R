## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <-NULL
        
        set<-function(y){
                x <<- y
                invmatrix <<- NULL
        }
        
        get<-function() x
        
        setinversematrix<-function(mat1) invmatrix <<- mat1 
        
        getinversematrix<-function() invmatrix
        
        list(set=set, get=get, setinversematrix=setinversematrix, getinversematrix = getinversematrix )
}


## Creates the inverse of the provided matrix using solve()
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <-x$getinversematrix()
        if (!is.null(invmat)){
                print("getting cached copy")
                return (invmat)
        }
        data <- x$get()
        invmat<- solve(data)
        
        x$setinversematrix(invmat)
        invmat
}

# sample execution steps

# a1 <- c(3, 2, 8)
# a2 <- c(6, 3, 2)
# a3 <- c(5, 2, 4)
# samplemat <- rbind(a1, a2, a3)
# 
# mymat<- makeCacheMatrix(samplemat)
# 
# cacheSolve(mymat)

# a1         a2         a3
# [1,] -0.2857143 -0.2857143  0.7142857
# [2,]  0.5000000  1.0000000 -1.5000000
# [3,]  0.1071429 -0.1428571  0.1071429
