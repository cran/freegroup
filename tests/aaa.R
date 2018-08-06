## define some checker functions, call them at the end.  They should
## all return TRUE if the package works.  Function checker1() has one
## argument, checker2() two, and checker3() has three.  These
## functions also check that the vectorization is OK; so for example
## in checker2(), if is of length 10 and 'y' is of length 1 [e.g. x <-
## rfree(10,2,2) and y <- rfree(10,2,2)], then all three of
## checker2(x,y), checker2(x,y[1]), checker2(x[1],y) should return
## TRUE.

library(freegroup)


checker1 <- function(x){
  stopifnot(x==x)

  stopifnot(x == x + as.free(0))  
  stopifnot(x == -(-x))
  stopifnot(x == +(+x))

  stopifnot(x+x-x == x)

  stopifnot(is.id(x-x))

  stopifnot(0*x == as.free(0))
  stopifnot(1*x == x)
  stopifnot(2*x == x+x)
  stopifnot(3*x == x+x+x)
  stopifnot(4*x == x+x+x+x)
  stopifnot(5*x == x+x+x+x+x)
  stopifnot(6*x == x+x+x+x+x+x)

  stopifnot(x^x==x)
  stopifnot(x^id() == x)

  ## verify idempotence of abelianize():
  stopifnot(abelianize(abelianize(x)) == abelianize(x))
  stopifnot(abelianize(abelianize(sum(x))) == abelianize(sum(abelianize(x))))

  stopifnot(abelianize(x) == abelianize(backwards(x)))
  stopifnot(abelianize(x+x) == abelianize(backwards(x)+backwards(x)))
  stopifnot(abelianize(x+backwards(x)) == abelianize(backwards(x)+x))

  stopifnot(abelianize(x*2) == abelianize(backwards(x)*2))
  stopifnot(abelianize(x*3) == abelianize(backwards(x)*3))
  stopifnot(abelianize(x*4) == abelianize(backwards(x)*4))
  stopifnot(abelianize(x*5) == abelianize(backwards(x)*5))

  stopifnot(abelianize(x*3) == abelianize(backwards(x) + x + x))
  stopifnot(abelianize(x*3) == abelianize(x + backwards(x) + x))
  stopifnot(abelianize(x*3) == abelianize(x + x + backwards(x)))
  stopifnot(abelianize(x*3) == abelianize(backwards(x) + backwards(x) + x))

  
  stopifnot(is.id(sum(x,rev(-x))))
  stopifnot(is.id(cumsum(c(x,-rev(x)))[2*length(x)]))
  stopifnot(cumsum(x)[length(x)] == sum(x))

  return(TRUE)
}


checker2 <- function(x,y){
  stopifnot(x^y == -y+x+y)
  stopifnot(x+y == x-(-y))

  stopifnot((x^y)^(-y) == x)

  stopifnot(abelianize(x) == abelianize(x^y))
  stopifnot(abelianize(x+y) == abelianize(y+x))
  stopifnot(abelianize(x+y) == abelianize(x+abelianize(y)))

  stopifnot((x^y)^(-y) == x)

  stopifnot(abelianize(x+y) == abelianize(abelianize(x)+y))
  

  stopifnot(sum(x,y) == sum(sum(x),sum(y)))

  stopifnot(sum(x^y[1]) == sum(x)^y[1])


  return(TRUE)
}

checker3 <- function(x,y,z){
  stopifnot(x+(y+z) == (x+y)+z) # associativity
  stopifnot(x^(y+z) == (x^y)^z) 
  stopifnot(x^z + y^z == (x+y)^z)

  abelianize(x^z - x^y) %>% abelianize %>% is.id %>% stopifnot
  stopifnot(sum(x,y,z) == sum(sum(x),sum(y),sum(z)))

  return(TRUE)
}

check_abelianize <- function(n){
  stopifnot(abelianize(sum(abelianize(alpha(sample(n))))) == abc(n))
  return(TRUE)
}



for(i in 1:10){
    x <- rfree(10,3,2)
    y <- rfree(10,3,2)
    z <- rfree(10,3,2)
    
    checker1(x)
    checker2(x,y)
    checker3(x,y,z)
}


sapply(1:10,check_abelianize)


## The following checks verify that various bugs are truly fixed:
as.free('a') * 0
as.free('a') * (-10:10)
stopifnot(sapply(0:10,function(n){is.id(alpha(n)+alpha(-n))}))
stopifnot(sapply(0:10,function(n){is.id(abc(n)+abc(-n))}))

