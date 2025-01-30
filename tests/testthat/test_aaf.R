test_that("Test suite aaf.R",{

    ## The following checks verify that is.primitive() and is.power() work correctly.

    
expect_true(all(
    is.primitive(
        as.free(c("a" , "aaaa", "aaaab", "aabaab", "aabcaabcaabcaabc"))) ==
                c(TRUE, FALSE  ,TRUE   , FALSE   , FALSE             )
))


expect_true (is.power(c(7,8,4,7,8,4,7,8,4,7,8,4),4))
expect_false(is.power(c(7,8,4,7,8,4,7,8,4,7,8,5),4))


})
