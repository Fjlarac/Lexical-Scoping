## Put comments here that give an overall description of what your
## functions do

## La funcion a cotinuaci?n asigna en la memoria chache la solcuion de la matriz inversa que sea calculada
## mejorando el rendimiento del programa.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL # creacion varaible flotante
  
  conjunto <- function(y) { # acciones de X de obejtos externos de la funci?n
    x <<- y
    inv <<- NULL
  }
  # Asignasion de objetos para  guardar en cache
  Inverso <- function() x 
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv
  list(conjunto = conjunto,
       Inverso = Inverso,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Función para calcular la mariz inverza u guardar en cache al llamar la función anterior

cacheSolve <- function(x, ...) 
{
  # funcion que calcula la matriz inversa
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
