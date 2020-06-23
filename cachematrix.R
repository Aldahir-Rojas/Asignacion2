## Put comments here that give an overall description of what your
## functions do

## Esta funcion crea una matriz y las funciones para obtenerla junto
#con su inversa

makeCacheMatrix <- function(x = matrix()) {
  
        inv<-NULL
        set<-function(y){
          x<<-y
          inv<<-NULL
        }
        get<-function() x
        setinv<-function(solve) inv<<-solve
        getinv<- function() inv
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##Esta funcion calcula la inversa de la matriz si esque esta no se
#ha calculado antes

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      if(!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
      data<-x$get()
      inv<-solve(data)
      x$setinv(inv)
      inv
  
}

m<-matrix(c(1,1,0,1,0,1,0,1,0),ncol = 3,nrow = 3)
a<-makeCacheMatrix(m)
cacheSolve(a)


