
makeCacheMatrix <- function(x = martix()){
        inv <- NULL
        set <- function(y){
                x <<- y     #define the value of "set"
                inv <<- NULL  #reset the value of "setinvs"
        }
        get <- function() x #print the value of "set"
        setinvs <- function(inverse) inv <<- inverse #define the value of "setinvs"
        getinvs <- function () inv  #print the value of inv
        
        list(set = set, get = get, set_invs = setinvs, get_invs = getinvs)
}

cacheSolve <- function(x,...){
        inv <- x$get_invs()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_invs(inv)
        inv
}


#testing

testmatrix_1 <- matrix(c(2,1,5,3),2,2)
det(testmatrix_1)

testmatrix_2 <- matrix( c(5, 1, 0,
                          3,-1, 2,
                          4, 0,-1), nrow=3, byrow=TRUE)
det(testmatrix_2)


my_matrix <- makeCacheMatrix(testmatrix_2)

my_matrix$set(testmatrix_1)
my_matrix$get()
my_matrix$set_invs()
my_matrix$get_invs()

cacheSolve(my_matrix)
