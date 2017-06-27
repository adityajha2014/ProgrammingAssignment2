## makeCacheMatrix is the function to store the matrix and the cached inverse so that
## we do not have to recalculate it again and again if the matrix in question is same.
## cacheSolve function is defined to retrive the value to cached inverse of matrix.
## It first check if the inverse of matrix is cached or not. If it is, it returns the cached 
## value, if not, the it calculate the inverse and set it as cache for further use.

## Function to create matrix object

makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
    get.matrix<-function()return(x)
    set.matrix<-function(y)
        x<<-y
    get.inverse<-function()return(inverse)
    set.inverse<-function(inv)
        inverse<<-inv
    return(list(get.matrix=get.matrix, set.matrix=set.matrix, get.inverse= get.inverse, set.inverse=set.inverse))

}


## Function to retrive the cached inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 temp<-x$get.inverse
    if(!is.null(temp)){
        message("Getting cached inverse of matrix: ")
        return(temp)
    }
    mat<-x$get.matrix()
    inv<-solve(mat)
    x$set.inverse(inv)
    return(inv)
}
