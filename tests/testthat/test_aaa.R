## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.  These functions also check that the
## vectorization is OK; so for example in checker2(), if 'x' is of
## length 10 and 'y' is of length 1 [e.g. x <- rfree(10,2,2) and y <-
## rfree(10,2,2)], then all three of checker2(x,y), checker2(x,y[1]),
## checker2(x[1],y) should return TRUE.



test_that("Test suite aaa.R",{

checker1 <- function(x,do_expensive_test = TRUE){

  expect_true(is.free(as.free(x)))

  expect_true(all(x==x))
  expect_false(any(x!=x))
  expect_error(!x)
  expect_error(x*x)
  expect_error(1+x)
  expect_error(x+1)
    
  expect_true(all((x == x + as.free(0))))
  expect_true(all((x == -(-x))))
  expect_true(all((x == +(+x))))

  expect_true(all(x+x-x == x))

  expect_true(all(is.id(x-x)))

  expect_true(all(0*x == as.free(0)))
  expect_true(all(1*x == x))
  expect_true(all(2*x == x+x))
  expect_true(all(3*x == x+x+x))
  expect_true(all(4*x == x+x+x+x))
  expect_true(all(5*x == x+x+x+x+x))
  expect_true(all(6*x == x+x+x+x+x+x))

  expect_true(all(x^x==x))
  expect_true(all(x^id() == x))

  ## verify idempotence of abelianize():
  expect_true(all(abelianize(abelianize(x)) == abelianize(x)))
  expect_true(all(abelianize(abelianize(sum(x))) == abelianize(sum(abelianize(x)))))

  expect_true(all(abelianize(x) == abelianize(backwards(x))))
  expect_true(all(abelianize(x+x) == abelianize(backwards(x)+backwards(x))))
  expect_true(all(abelianize(x+backwards(x)) == abelianize(backwards(x)+x)))

  expect_true(all(abelianize(x*2) == abelianize(backwards(x)*2)))
  expect_true(all(abelianize(x*3) == abelianize(backwards(x)*3)))
  expect_true(all(abelianize(x*4) == abelianize(backwards(x)*4)))
  expect_true(all(abelianize(x*5) == abelianize(backwards(x)*5)))

  expect_true(all(abelianize(x*3) == abelianize(backwards(x) + x + x)))
  expect_true(all(abelianize(x*3) == abelianize(x + backwards(x) + x)))
  expect_true(all(abelianize(x*3) == abelianize(x + x + backwards(x))))
  expect_true(all(abelianize(x*3) == abelianize(backwards(x) + backwards(x) + x)))

  
  expect_true(all(is.id(sum(x,rev(-x)))))
  expect_true(all(is.id(cumsum(c(x,-rev(x)))[2*length(x)])))
  expect_true(all(cumsum(x)[length(x)] == sum(x)))

  allsymbols <- getlet(sum(x))
  if(length(allsymbols)>0){ # guard against identity, always problematic
      a <- allsymbols[1]
      expect_true(all(size(flip(x,a))   == size(x)))
      expect_true(all(total(flip(x,a))  == total(x)))
      expect_true(all(number(flip(x,a)) == number(x)))
  }
  if(length(allsymbols)>=2){ # two or more symbols needed
      b <- allsymbols[2]
      expect_true(all(size(subs(x,a,b))   <= size(x)))
      expect_true(all(total(subs(x,a,b))  <= total(x)))
      expect_true(all(number(subs(x,a,b)) <= number(x)))
  }

  expect_true(all(size(as.cyclically_reduced(x)) <= size(x)))
  expect_true(all(total(as.cyclically_reduced(x)) <= total(x)))
  expect_true(all(number(as.cyclically_reduced(x)) <= number(x)))

  expect_true(all(is.cyclically_reduced(as.cyclically_reduced(x))))

  if(do_expensive_test){
      for(i in seq_along(x)){
          o <- as.cyclically_reduced(x[i])
          expect_true(all(o %~% allconj(o)))
          expect_true(all(all(is.id(allconj(o) + allconj(-o)[shift(rev(1:total(o)))]))))
      }
  }

  expect_output(print(x))
  expect_output(print(x[FALSE]))
  expect_true(is.character(as.character(x)))
    
  return(TRUE)
}


checker2 <- function(x,y){

    expect_error(x*y)

    expect_true(all(x^y == -y+x+y))
    expect_true(all(x+y == x-(-y)))

    expect_true(all((x^y)^(-y) == x))

    expect_true(all(abelianize(x) == abelianize(x^y)))
    expect_true(all(abelianize(x+y) == abelianize(y+x)))
    expect_true(all(abelianize(x+y) == abelianize(x+abelianize(y))))

    expect_true(all((x^y)^(-y) == x))

    expect_true(all(abelianize(x+y) == abelianize(abelianize(x)+y)))
  

    expect_true(all(sum(x,y) == sum(sum(x),sum(y))))

    expect_true(all(sum(x^y[1]) == sum(x)^y[1]))


  stopifnot(  size(x+y) <= size(x)   +   size(y))
  stopifnot( total(x+y) <= total(x)  +  total(y))
  stopifnot(number(x+y) <= number(x) + number(y))

  stopifnot(  size(x+y) <= size  (abs(x) + abs(y)))
  stopifnot( total(x+y) <= total (abs(x) + abs(y)))
  stopifnot(number(x+y) <= number(abs(x) + abs(y)))

  stopifnot(is.conjugate(x,x^y))

  return(TRUE)
}

checker3 <- function(x,y,z){
  stopifnot(x+(y+z) == (x+y)+z) # associativity
  stopifnot(x^(y+z) == (x^y)^z) 
  stopifnot(x^z + y^z == (x+y)^z)

  abelianize(x^z - x^y) |> abelianize() |> is.id() |> stopifnot()
  stopifnot(sum(x,y,z) == sum(sum(x),sum(y),sum(z)))

  ## Hall-Witt:
  stopifnot(all(is.id(.[.[x,-y],z]^y + .[.[y,-z],x]^z + .[.[z,-x],y]^x)))
  
  return(TRUE)
}




for(i in 1:2){
    x <- rfree(10,6,3)
    y <- rfree(10,6,3)
    z <- rfree(10,6,3)
    
    checker1(x)
    checker2(x,y)
    checker3(x,y,z)
}

checker1(rfreee(), FALSE)
checker1(rfreeee(), FALSE)

stopifnot(checker1(id()))
stopifnot(checker1(as.free('a')))
stopifnot(checker1(as.free('ab')))

expect_true(is.free(as.free(rbind(c(1,2,1),c(2,1,-1)))))
expect_true(all(as.free(list("a","ab","abc")) == abc(1:3)))
expect_true(all(list_to_free(sapply(1:5,seq_len))==abc(1:5)))
expect_error(list_to_free(list(c,c)))

expect_true(is.free(as.free(unclass(rfree(4,4)))))
expect_error(as.free(function(...){}))
expect_output(print(as.free(0)))
expect_output(print(as.character_free(matrix(1:6,2,3),latex=TRUE)))
expect_error(c(rfree(4,4),rfree(4,4),1))

})
