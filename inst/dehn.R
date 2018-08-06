library(magrittr)
'
> dput(x)
c(1, 5, 9, 13, 17)
> f <- function(n){c(x[process(seq(from=n,by=1,len=3),5)],-x[process(seq(from=n-1,by=-1,len=2),5)])}
> t(sapply(1:5,f))
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    5    9  -17  -13
[2,]    5    9   13   -1  -17
[3,]    9   13   17   -5   -1
[4,]   13   17    1   -9   -5
[5,]   17    1    5  -13   -9
>



> vec_to_matrix(c(1,1,1,-2,-2,1,1,4,5))
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
[1,]    1    1    1    2    2    1    1    4    5
[2,]    1    1    1   -1   -1    1    1    1    1
>

> x <- 100 + 1:19
> A <- matrix(0,13,7)
> t(matrix(x[row(A)+col(A)-1],nrow=13))
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
[1,]  101  102  103  104  105  106  107  108  109   110   111   112   113
[2,]  102  103  104  105  106  107  108  109  110   111   112   113   114
[3,]  103  104  105  106  107  108  109  110  111   112   113   114   115
[4,]  104  105  106  107  108  109  110  111  112   113   114   115   116
[5,]  105  106  107  108  109  110  111  112  113   114   115   116   117
[6,]  106  107  108  109  110  111  112  113  114   115   116   117   118
[7,]  107  108  109  110  111  112  113  114  115   116   117   118   119
> 
'

library(magic)
library(freegroup)


`.dehn` <- function(x,r){ #  x is a 2r>n; in each row, first 'r' columns equal
                                        #  to last 'n-r' columns.
    l <- length(x)
    f <- function(n){
        c(
            +x[process(seq(from=n  ,by=+1,len=r  ),l)],
            -x[process(seq(from=n-1,by=-1,len=l-r),l)]
        )
    }
    
    t(sapply(seq_along(x),f))
}


                                        # dehn2(c(1,-3,-3,-3,2,2,1,1,1,1,7),8)

`dehn` <- function(x,r){ #  x is  2r>n; in each row, first 'r' columns equal
                                        #  to last 'n-r' columns.
    l <- length(x)
    f <- function(n,x){c(
                         +x[process(seq(from=n  ,by=+1,len=r  ),l)],
                         -x[process(seq(from=n-1,by=-1,len=l-r),l)]

                       )}
        t(cbind(
            sapply(seq_len(l),f,x),
            sapply(seq_len(l),f,rev(-x))
        ))
}

## x1 <- c(1,-2,-2,-2,4,5)
## substrings(x1,4)
`substrings` <- function(v,r){ # substrings of 'v', of length 'r'
    jj <- length(v)-r+1
    A <- matrix(0,jj,r)
    matrix(v[row(A)+col(A)-1],nrow=jj)
}

                                        # somehow we must match rows of substrings(...) with rows of
                                        # dehn2(...)$expression (and replace the expression with equivalent)


`.dehn2` <- function(x,r){ #  x is a 2r>n; in each row, first 'r' columns equal
                                        #  to last 'n-r' columns.
    l <- length(x)
    f_expression <- function(n){ +x[process(seq(from=n  ,by=+1,len=r  ),l)]  }
    f_equivalent <- function(n){ -x[process(seq(from=n-1,by=-1,len=l-r),l)] }
    
    expression <- t(sapply(seq_along(x),f_expression,simplify=TRUE))
    equivalent <- t(sapply(seq_along(x),f_equivalent,simplify=TRUE))
    if(nrow(equivalent)==1){ equivalent <- t(equivalent) }

    list(expression=expression,equivalent=equivalent)
}

## we will define 'original' and try and reduce it with 'relator'



remove_adjacent_inverses <- function(x){
    jj <- which(rowSums(cbind(x,shift(x)))==0)
    if(length(jj)>0){
        return(x[-c(jj,jj-1)])
    } else {
        return(x)
    }
}

comb <- function(x){
    xold <- c(x,1)
    while(any()){}

}

`use_relator_n` <- function(orig,relator,n){
  

  ## cycles through all substrings of the presentation element
  ## 'relator', searching for an opportunity to replace part of 'orig'
  ## with its equivalent.  If nothing is found, return 'orig'
  ## unchanged.

  ## If you want to shorten 'orig', then make sure that 'n' is
  ## sufficiently large because otherwise this function will blindly
  ## replace a short substring of 'orig' with a *longer* equivalent.
  ## To guarantee only shortening substitutions are made, 'n' needs to
  ## be longer than half relator: 2*n>length(relator)

  ## R> orig <- c(1,2,2,2,4,5,1)
  ## R> relator <- c(-4,-2,-2,4,7)
  ## R> simplify(orig,relator,3)


    s <- seq_len(n)
    o <- dehn(relator,n)
    p <- substrings(orig,n)
  print(o)
  found <- FALSE
  for(i in seq_len(nrow(o))){
    for(j in seq_len(nrow(p))){
      
      if(all(o[i,s] == p[j,s])){

        ## what we *want* to do is this.  If, say, i=8,j=3,
        ## and n=3 then we want to say
        ## "orig[3:5] <- dehn(relator, 3)[8, 4:5]" ...but this
        ## doesnt work (for good reasons)
        ## so we are  going to have to build up the thing from bits:




            bit1 <- orig[seq_len(j-1)]
            if(n<length(relator)){
              bit2 <- o[i,seq.int(from=n+1,to=length(relator))]
            } else {
              bit2 <- NULL
            }
            bit3 <- orig[seq.int(from=j+n,to=length(orig))]
        if(j+n <= length(orig)){
          bit3 <-     orig[seq.int(from=j+n,to=length(orig))]
        } else {
          bit3 <-  NULL
        }
        return(c(bit1,bit2,bit3))
        
      }
    }
  }
  return(orig)   # original string returned, no change.  Not sure about this.
}

orig <- c(1,2,2,2,4,5,1)
orig1 <- c(66,66,66,66,1,2,2,2,4,5,1)
relator <- c(-4,-2,-2,4,7)

## In the above, 'orig' is the original string and 'relator' is an
## element of the presentation.  The original string 'orig' =
## c(1,2,2,2,4,5,1) = 1222451 = ab^3dea in algebraic notation; and
## 'relator' = c(-4,-2,-2,4,7) = d^{-1}b^{-2} dg=Id.  Thus relator=Id
## gives 244=47.  We can therefore re-write orig as 1222451 =
## 12.224.51 = 12.47.51=124751 = abdgea, which is shorter.

jj <- use_relator_n(orig,relator,3)

## R> 

use_relator <- function(orig,relator){
  ## all sensible values of 'n', tried one at a time.  Returns the
  ## first word found that is shorter than 'orig', if it exists: that
  ## is, it "uses" the presentation element 'relator' at most once (ie
  ## not at all if it doesn't find a shortening).


  lp <- length(relator)
  for(n in seq(from=ceiling(lp/2 + 0.01), to=lp)){
    orig.possibly.shorter <- use_relator_n(orig,relator,n)
    if(length(orig.possibly.shorter) < length(orig)){  # found a shortening!
      return(orig.possibly.shorter)  # actually, if we are here then
                                     # orig.possibly.shorter is
                                     # *definitely* shorter-not just
                                     # possibly shorter; use_relator()
                                     # exits here
    }
  } # for(n) loop closes
  return(orig)     # possibly the same length as orig (but hopefully shorter)
}

use_single_relator <- function(orig,relator){   ## repeatedly
                                           ## shortens 'orig' using
                                           ## 'relator' until it
                                           ## cannot be shortened any
                                           ## further.

  print(orig)
  print(relator)
  perhaps_further_shortening_possible <- TRUE
  while(perhaps_further_shortening_possible){
    orig.possibly.shorter <- remove_adjacent_inverses(use_relator(orig,relator))
    if(length(orig.possibly.shorter) == length(orig)){  # found no shortening!
      perhaps_further_shortening_possible <- FALSE # no shortening possible, close while() loop
    }
    orig <- orig.possibly.shorter
  } 
  return(orig)  # hopefully shorter, but possibly same length as, function argument orig
}

`comb_relatorlist` <- function(orig, relatorlist){
  ## Function comb_relatorlist() goes through a whole list of
  ## presentation elements; skeleton similar to that of
  ## comb_single_relator()

  while(TRUE){
    orig.possibly.shorter <- orig
    for(i in seq_along(relatorlist)){
      orig.possibly.shorter %<>% use_single_relator(relatorlist[[i]])  # maybe shorten more than once
    }
    if(length(orig.possibly.shorter) == length(orig)){  # no shortening found after going through all relatorlist
      return(orig)  # function exits
    } else {  # found a shortening;
      orig <- orig.possibly.shorter  # Here, 'orig.possibly.shorter' is *definitely* shorter than orig
    }
  }  # iterate the while() loop once more, hoping to find further shortening
  stop("this cannot happen")
} 

is_simply_reduced <- function(...){}

## presentation for quaternion group:
rl <- list(c(-1,-1), c(2,2,-1), c(3,3,-1),c(4,4,-1),c(2,3,4,-1))


thing <- c(1,3,3,3,3,3,3,4,3,2,4,4,4,4)
jj <- comb_relatorlist(thing,rl)
