test_that("Test suite aac.R",{

check_abelianize <- function(n){
    expect_true(all(abelianize(sum(abelianize(alpha(sample(n))))) == abc(n)))
}

sapply(1:10,check_abelianize)

})
