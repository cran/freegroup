"Ops.free" <-
  function (e1, e2 = NULL) 
{
    if(nargs() == 1){  #unary operator
        if (.Generic == "+") {
            return(e1)
        } else if (.Generic == "-") {
            return(inverse(e1))
    } else {
          stop(gettextf("unary operator %s is not implemented", dQuote(.Generic)))
    }
  }

    lclass <- nchar(.Method[1]) > 0
    rclass <- nchar(.Method[2]) > 0
    
    if (lclass && rclass) {
        if (.Generic == "+") {
            return(juxtapose(e1, e2))
        } else if (.Generic == "-"){
            return(juxtapose(e1,inverse(e2)))
        } else if (.Generic == "^"){
            return(free_power(e1,e2))
        } else if (.Generic == "=="){
            return(free_equal(e1,e2))
        } else if (.Generic == "!="){
            return(!free_equal(e1,e2))
        } else if (.Generic == "*"){
            return(juxtapose(e1, e2))
        } else {
            stop(gettextf("<free> %s <free> not defined", .Generic))
        }
    } else if (lclass && !rclass){
      if(.Generic == "*"){
          return(free_repeat(e1,e2))   #e2 should be an integer
      } else {
          stop(gettextf("<free> %s <non-free> not defined", .Generic))
        }
    } else if (!lclass && rclass){
        if(.Generic == "*"){
            return(free_repeat(e2,e1))   #e1 should be an integer
        } else {
            stop(gettextf("<non-free> %s <free> not defined", .Generic))
        }
    } else if (!lclass && !rclass){
        stop("should not reach here")
    } else {
        stop("this cannot happen")
    }
}

`inverse` <- function(e1){ UseMethod("inverse",e1) }
`inverse.matrix` <- function(e1){ rbind(rev(e1[1,]), -rev(e1[2,])) }
`inverse.free` <- function(e1){ free(lapply(e1,inverse)) }

`juxtapose`  <- function(e1,e2){  #  vectorized 
    jj <- cbind(seq_along(e1),seq_along(e2))
    out <- list()
    for(i in seq_len(nrow(jj))){
        out[[i]] <- reduce(cbind(e1[[jj[i,1]]],e2[[jj[i,2]]]))
    }
    donames(free(out),e1,e2)
}

`free_power` <- function(e1,e2){
    jj <- cbind(seq_along(e1),seq_along(e2))
    out <- list()
    for(i in seq_len(nrow(jj))){
        out[[i]] <- reduce(cbind(
            inverse(e2[[jj[i,2]]]),
            e1[[jj[i,1]]],
            e2[[jj[i,2]]]
                    ))
    }
    donames(free(out),e1,e2)
}

`free_equal` <- function(e1,e2){
    stopifnot(is.free(e1),is.free(e2))
    jj <- cbind(seq_along(e1),seq_along(e2))
    out <- 
      apply(jj,1,function(x){
        return(
            all(dim(e1[[x[1]]]) == dim(e2[[x[2]]])) &&
            all(e1[[x[1]]] == e2[[x[2]]])
        )}
        )
    donames(out,e1,e2)
}

`free_repeat` <- function(e1,n){ # e1 is free, n an integer; makes vectorized "e1*n" work
    if(identical(as.integer(n),0L)){return(id(length(e1)))}
    jj <- cbind(seq_along(e1),seq_along(n))
    out <- list()
    for(i in seq_len(nrow(jj))){
        M <- e1[[jj[i,1]]]
        nn <- n[jj[i,2]]
        out[[i]] <- 
        if(nn>0){
            M[,rep(seq_len(ncol(M)),nn),drop=FALSE]
        } else if (nn==0){
            M[,FALSE,drop=FALSE]
        } else if (nn<0){
            inverse(M)[,rep(seq_len(ncol(M)),-nn),drop=FALSE]
        }
    }
    return(donames(free(out),e1,n))
}   

`donames` <- function(f,e1,e2){
  jj1 <- seq_along(e1)
  jj2 <- seq_along(e2)
  names(jj1) <- names(e1)
  names(jj2) <- names(e2)
  names(f) <- names(jj1+jj2)
  return(f)
}
