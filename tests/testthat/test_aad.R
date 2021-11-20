test_that("Test suite aad.R",{

## The following checks verify that various bugs are truly fixed:
expect_silent(as.free('a') * 0)
expect_silent(as.free('a') * (-10:10))
expect_true(all(sapply(0:10,function(n){is.id(alpha(n)+alpha(-n))})))
expect_true(all(sapply(0:10,function(n){is.id(abc(n)+abc(-n))})))

})
