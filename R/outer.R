`permsymb_single_X` <- function(X,f){
    stopifnot(length(X) == 1)
    M <- rbind(f(X[[1]][1,,drop=FALSE]))
    p <-  X[[1]][2,,drop=FALSE]
    free(sapply(seq_len(nrow(M)),function(i){rbind(M[i,],p)},simplify=FALSE))
}

`permsymb_single_f` <- function(X,f){
    stopifnot(nrow(with(environment(f),x))==1)
    X <- unclass(X)
    for(i in seq_along(X)){
        X[[i]][1,] %<>% f
    }
    return(free(X))
}

`permsymb_vec` <- function(X,f){
    M <- with(environment(f),x)
    stopifnot(nrow(M) == length(X))

    for(i in seq_along(X)){
      X[[i]][1,] <- M[i,X[[i]][1,,drop=FALSE],drop=FALSE]
    }
    return(X)
}

`permsymb` <- function(X,f){
    if(length(X)==1){
        return(permsymb_single_X(X,f))
    } else if(nrow(with(environment(f),x))==1){
        return(permsymb_single_f(X,f))
    } else {
        return(permsymb_vec(X,f))
    }
}

`autosub_lowlevel` <- function(M,e,S){  # M is a matrix, e the
                                        # symbol to substitute, S the
                                        # substitutee

    stopifnot(length(e)==1)
    n <- ncol(S)
    out <- matrix(0L,2,(n+1)*sum(abs(M[2,M[1,]==e])) + sum(M[1,] != e))

    Splus <- S
    Sminus <- rbind(S[1,],-S[2,])[,n:1]  # negative power
    j <- 1
    for(i in seq_len(ncol(M))){
        if(M[1,i]==e){
          if(M[2,i]>0){
            S <- Splus
            E <- rbind(e,1L)
          } else {
            S <- Sminus
            E <- rbind(e,-1L)
          }
            r <- abs(M[2,i])

            out[,j:(j+(n+1)*r-1)] <- kronecker(t(rep(1,r)),cbind(E,S))
            j <- j + (n+1)*r
        } else {
            out[,j] <- M[,i]
            j <- j+1
        }
    }
    return(out)
}

`autosub` <- function(X,e,S,automorphism_warning=TRUE){
    stopifnot(length(S)==1)
    S <- S[[1]]
    if(ncol(S)==0){return(X)}
    if(is.character(e) & length(e)==1){e <- as.free(e)}
    if(is.free(e)){e <- getlet(e)}
    stopifnot(length(e)==1)
    if(automorphism_warning){
      if(any(S[1,] == e)){
        warning("substitution not an automorphism")
      }
    }
    for(i in seq_along(X)){
        X[[i]] %<>% autosub_lowlevel(e,S)
    }
    return(free(X))
}
