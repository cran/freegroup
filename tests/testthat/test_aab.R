test_that("Test suite aab.R",{

`autosub_checker`  <- function(X,S){ 
    expect_true(all(X == X |> autosub('a',S) |> autosub('a',-S)))
}

`permsymb_checker` <- function(X,p){
    expect_true(all(X == X |>
                    permsymb(permutations::as.function.permutation(p)) |>
                    permsymb(permutations::as.function.permutation(permutations::inverse.cycle(p)))
                    ))
}

# randomised tests of permsymb() removed pending
# https://github.com/RobinHankin/permutations/issues/11

for(n in 3:9){
    for(r in 5:6){
        autosub_checker(X=rfree(n,r), S=discard(rfree(1,r),'a'))
#        permsymb_checker(X=rfree(n,r), p=rperm(n,r))
    }
}

for(i in 1:3){
    permsymb_checker(X=rfree(1,4), p=permutations::as.cycle(permutations::rperm(1,8)))
    permsymb_checker(X=rfree(8,4), p=permutations::as.cycle(permutations::rperm(1,8)))

}


#permsymb_checker(X=rfree(8,4), p=as.cycle(1:9))
expect_warning(autosub(abc(1:6),"c",as.free("xxxyzc")))
expect_silent(autosub(abc(1:6),"c",as.free("")))
})

