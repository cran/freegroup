test_that("Test suite aae.R",{

    ## The following checks verify that subs() works correctly.
    checker <- function(s){
        expect_true(all(as.free(s) %>% subs(a="z") %>% subs(z="a") == as.free(s)))
        expect_true(all(as.free(s) %>% subs(a="z") == as.free(gsub("a","z",gsub("A","Z",s)))))
    }

    checker(c("","a","aa","aaa","ab","ba","baa","aab"))
    checker(c("","A","aa","aaa","Ab","ba","baa","aab"))
    checker(c("aaaabbaaa","aaacccaababac","cccccca","bbbbb"))
    checker(c("AAAbaaa","aaacccAAAbAbabA","ccccccA","Abbbbb"))
})
