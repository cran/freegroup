`as.free` <- function(x){  # x is a list
    if(is.free(x)){
      return(x)
    } else if(is.matrix(x)){
        return(free(x))
    } else if(is.character(x)){
        return(char_to_free(x))
    } else if(identical(x,0)|identical(x,0L)){
        return(free(matrix(0,2,0)))
    } else if (is.list(x)){
        return(free(list_to_free(x)))
    } else if(is.vector(x)){    
        return(free(vec_to_matrix(x)))
    } else {
        return(lapply(x,free))
    }
}

`is.free` <- function(x){inherits(x,"free")}

`free` <- function(x){ # x is a list, each element is a two-row matrix
    if(is.matrix(x)){x <- list(x)}
    stopifnot(all(unlist(lapply(x,is_proper))))
    x <- reduce(x)
    class(x) <- "free"  # This is the *only* place the class is set
    return(x)
}

`char_to_matrix` <- function(x){
  if(nchar(x)>0){
      jj <- as.numeric(charToRaw(x))-96
      s <- rep(1,length(jj))
      s[jj<0] <- -1
      jj[jj<0] <- jj[jj<0] + 32
      return(rbind(jj,s))
  } else {   # x=''
      return(matrix(0,2,0))
  }
}

`char_to_free` <- function(x){
    jj <- sapply(x,char_to_matrix,simplify=FALSE)
    names(jj) <- names(x)
    return(free(jj))
}

`list_to_free` <- function(x){
  stopifnot(is.list(x))
  if(all(unlist(lapply(x,is.matrix)))){
    return(free(x))
  } else if (all(unlist(lapply(x,is.character)))){
    return(free(lapply(x,char_to_matrix)))
  } else if (all(unlist(lapply(x,is.vector)))){
    return(free(lapply(x,vec_to_matrix)))
  } else {
    stop("list_to_free() is confused; it requires a list of matrices or a list of vectors")
  }
}

`vec_to_matrix` <- function(x){  # takes a *vector* like c(1,2,-1,-1,2); returns a matrix
    if(all(x==0)){
        return(matrix(NA,2,0))
    } else {
        x <- x[x!=0]
       return(rbind(abs(x),sign(x)))
    }
}

setGeneric("tietze",function(x){standardGeneric("tietze")})
`tietze` <- function(x){UseMethod("tietze")}

`tietze.matrix` <- function(x){
    c(apply(x,2,function(r){rep(sign(r[2])*r[1],abs(r[2]))}),recursive=TRUE)
}

`tietze.free` <- function(x){
  lapply(x,tietze.matrix)
}

`print.free` <- function(x, ...){
    if(length(x)==0){
      print(NULL)
    } else {      
      print(noquote(unlist(lapply(x,as.character_free,...))))
    }
    return(invisible(x))
}

`as.character_free` <- function(m,latex=getOption("latex")){ # takes a matrix

    symbols <- getOption("freegroup_symbols")
    if(is.null(symbols)){symbols <- letters}
    if(ncol(m)==0){return("0")}
    if(isTRUE(latex)){
        open <- "{"
        close <- "}"
    } else {
        open <- ""
        close <- ""
    }
    f <- function(x){
        ifelse(
            x[2]==1,
            symbols[x[1]],
            paste(symbols[x[1]],"^",open,x[2],close,sep="")
        )
    }
    paste(apply(m,2,f),collapse=".")
}

`remove_zero_powers` <- function(a){a[,a[2,,drop=FALSE]!=0,drop=FALSE]}

`is_proper` <- function(a){
  return(
      is.matrix(a)     &&
      nrow(a) == 2     &&
      all(a==round(a)) &&
      all(a[1,] > 0)
  )
}

`is_reduced` <- function(a){ all(a[2,]!=0) && all(diff(a[1,]) != 0) }
   
`.gsr` <- function(a){  # gsr == Get Start Repeats
    cumsum(rle(a)$lengths)
}

`.gse` <- function(a){   # gse == Get Start & End [of a run]
    cbind(
        1L+length(a)-rev(.gsr(rev(a))),
        .gsr(a)
    )
}

`consolidate` <- function(a){
    jj <- .gse(a[1,])
    f <- function(i){sum(a[2,seq(from=jj[i,1],to=jj[i,2])])}
    rbind(
        rle(a[1,])$values,
        sapply(seq_len(nrow(jj)), f)
    )
}

`reduce` <- function(a){
    if(is.list(a)){
        return(lapply(a,reduce))
    } else {
        while(!is_reduced(a)){
#            a %<>% consolidate %<>% remove_zero_powers
            a <- remove_zero_powers(consolidate(a))
        }
        return(a)
    }
}

`c.free` <- function(...){
    if(!all(unlist(lapply(list(...),is.free)))){
        stop("all arguments must be the same class")
    } else {
        return(free(unlist(list(...),recursive=FALSE)))
    }
}

`[.free` <- function(x,...){ free(unclass(x)[...])}

`[<-.free` <- function(x, index, value){
    out <- unclass(x)
    out[index] <- value
    return(free(out))
}

`rfree` <- function(n=7,size=4,number=size,powers=seq(from=-size,to=size)){
  out <- list()
  for(i in seq_len(n)){
    out[[i]] <- 
      rbind(
          sample(number,size,replace=TRUE),
          sample(powers,size,replace=TRUE)
      )
  }
  return(free(out))
}

`rfreee` <- function(n=30, size=8, number=size, powers=seq(from=-size, to=size)){
    rfree(n=n, size=size, number=size, powers=seq(from=-size, to=size))
}

`rfreeee` <- function(n=40,size=25, number=size,powers=seq(from=-size, to=size)){
    rfree(n=n, size=size, number=size, powers=seq(from=-size, to=size))
}

`abc` <- function(v){
    free(sapply(v,
                function(o){
                    if(o>0){
                        return(rbind(seq_len(o),1))
                    } else if (o==0){
                        return(rbind(1,0))
                    } else {
                        return(rbind(rev(seq_len(-o)),-1))
                    }
                },
                simplify=FALSE))
}

`alpha` <- function(v){
    free(sapply(v,
                function(x){
                    if(x!=0){
                        return(rbind(abs(x),sign(x)))
                    } else {
                        return(rbind(1,0))
                    }
                },
                simplify=FALSE))
}

`is.id` <- function(x){ UseMethod("is.id",x) }
`is.id.free` <- function(x){x==as.free(0)}

`id` <- function(n){free(rep(list(matrix(1,2,0)),n))}

`.is_cyc_reduced` <- function(a){  # low-level, works with matrices
  n <- ncol(a)
  if(n>0){
      if(a[1,1] != a[1,n]){
          return(TRUE)
      } else {
          return(prod(a[2,c(1,n)])>0)
      }
  } else { 
      return(TRUE)
  }
}

`is.cyclically_reduced` <- function(a){unlist(lapply(unclass(a), .is_cyc_reduced))}
#  a %>% unclass %>% lapply(.is_cyc_reduced) %>% unlist

`cyclically_reduce_tietze` <- function(p){  # p is in reduced tietze form
  if(length(unique(p))<2){  # either empty, or only one distinct symbol
    return(p)  
  }
  n <- floor(length(p)/2)
  jj <- (p[seq_len(n)] + p[seq(from=length(p),by= -1, len=n)]) != 0 # zero means cancellable
  if(all(jj)){ # nothing to cancel
    out <- p
  } else if(any(jj)){  # potential cancellations
    i <- min(which(jj))  # there will be i-1 cancellations
    out <- p[seq(from=i,to=length(p)-i+1)]  ## select the middle bit 
  } else {
    ## At this point, all(jj==0); everything from 1-n cancels
    if(length(p)%%2){ # argument p has odd length
     out <- p[n+1]   # return the central element
    } else {  # p has even length
      out <- NULL # return the identity
    }
  }
  return(out)
}

`as.cyclically_reduced` <- function(a){
  f <- function(p){ vec_to_matrix(cyclically_reduce_tietze(p))}
  return(free(lapply(tietze(a), f)))
}

`cyclically_reduce` <- as.cyclically_reduced


`is.conjugate_single` <- function(u,v){

  ## this is a low-level helper function, takes two integer vectors
  ## (words in Tietze form)

  if( (length(u)==0) & (length(v)==0)){return(TRUE)}
  if(length(u) != length(v)){ return(FALSE) }
  ##  at this point, both have identical nonzero length
  out <-
    apply(
        kronecker(t(u),1L+u*0L) ==
        magic::circulant(v,doseq=FALSE), 1,all)
  
  return(any(out))
}

"is.conjugate" <- function(x,y){UseMethod("is.conjugate")}

`is.conjugate.free` <- function(x,y){  # this is the user-friendly function
  jj <-  cbind(seq_along(x),seq_along(y))
  f <- function(v){
    is.conjugate_single(
        unlist(tietze(as.cyclically_reduced(x[v[1]]))),
        unlist(tietze(as.cyclically_reduced(y[v[2]])))
    )}
  apply(jj,1,f)
}

"%~%" <- function(x,y){UseMethod("%~%")}
"%~%.free" <- function(x,y){is.conjugate(x,y)}

`allconj` <- function(x){
  stopifnot(length(x)==1)
  x <- as.cyclically_reduced(x)
  if(is.id(x)){return(id())}
  out <- free(plyr::alply(magic::circulant(tietze(x)[[1]],doseq=FALSE),1,vec_to_matrix))
  names(out) <- NULL
  return(out)
}

`abelianize` <- function(x){
  free(lapply(x,
         function(o){  # takes a 2-row matrix
           jj <- unclass(by(o[2,],o[1,],sum))
           rbind(as.numeric(names(jj)),jj)
         }))
}

`is.abelian` <- function(x){x==abelianize(x)}

`sum.free` <- function(..., na.rm=FALSE){
  free(do.call("cbind",lapply(unclass(list(...)),function(x){do.call("cbind",x)})))
}

`rep.free` <- function(x, ...){
    u <- seq(length.out = length(x))
    return(x[rep(u, ...)])
}

`cumsum.free` <- function(x){
    u <- as.free(0)
    out <- rep(u,length(x))
    for(i in seq_along(x)){
        out[i] <- u <- u + x[i]
    }
    return(out)
}

`getlet` <- function(x){
  out <- lapply(x,function(m){sort(unique(m[1,]))})
  if(length(out)==1){out <- out[[1]]}
  return(out)
}

`keep` <- function(a,yes){
    yes <- getlet(as.free(yes))
#    a %<>% unclass %>% lapply(function(m){m[,(m[1,] %in% yes),drop=FALSE]}) %>% free
    free(lapply(unclass(a),function(m){m[,(m[1,] %in% yes),drop=FALSE]}))
}

`discard` <- function(a,no){
    no <- getlet(as.free(no))
    jj <- unique(c(getlet(a),recursive=TRUE))
    keep(a, jj[!jj %in% no])
}

`backwards` <- function(x){
    free(lapply(x,function(o){o[,rev(seq_len(ncol(o))),drop=FALSE]}))
  }

`subs` <- function(X,...){
    sb <- list(...)
    v <- names(sb)
    out <- X
    for (i in seq_along(sb)) {
        out <- subsu(out, v[i],sb[[i]])
    }
    return(out)
}

`subsu` <- function(X,from,to){
  from <- getlet(as.free(from))
  to <- getlet(as.free(to))
  stopifnot(length(to) == 1)
  
  s <- function(M,from,to){
    M[1,M[1,] %in% from] <- to
    return(M)
  }
    
#  a %<>% unclass %>% lapply(s,from=from,to=to) %>% free
  free(lapply(unclass(X),s,from=from,to=to))
}


`flip` <- function(X,turn){
  turn <- getlet(as.free(turn))
  
  s <- function(M,turn){
    M[2,M[1,] %in% turn] %<>% "*"(-1)
    return(M)
  }

  X %<>% unclass %>% lapply(s,turn=turn) %>% free
  return(X)  
}

`abs.free` <- function(x){
  s <- function(M){
    M[2,] %<>% abs
    return(M)
  }
  x %<>% unclass %>% lapply(s) %>% free
  return(x)
}

`size` <- function(a){unlist(lapply(a,ncol))}
`total` <- function(a){unlist(lapply(a,function(M){sum(abs(M[2,]))}))}
`number` <- function(a){unlist(lapply(a,function(M){length(table(abs(M[1,])))}))}

`bigness` <- function(a){
  out <- cbind(size=size(a),total=total(a),number=number(a))
  rownames(out) <- as.character(lapply(a,function(x){as.character_free(x)}))
  return(drop(out))
  }


`shift` <- function(x,i=1){magic::shift(x,i)}

setOldClass("free")
setMethod("[", signature(x="dot",i="free",j="ANY"),
          function(x,i,j,drop){
              j <- as.free(j)
              return(-i-j+i+j)
          })


`is.power` <- function(d, n){nrow(unique(data.frame(matrix(d, nrow=n, byrow=TRUE)))) == 1}

`is.primitive` <- function(x){
    if(is.free(x)){x <- tietze(x)}

    facs  <- x |> lapply(length) |> lapply(function(n){seq_len(n)[n %% seq_len(n) == 0] |> tail(-1)})
    out <- rep(TRUE,length(x))
    for(i in seq_along(x)){
        a <- facs[[i]]
        for(j in a){
            if(is.power(x[[i]],j)){
                out[i] <- FALSE  # the meat
                break()
            }
        }
    }
    return(out)
}
