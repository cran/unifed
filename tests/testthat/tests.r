library(unifed)

a.deviance <- function(y,mu){
    aux.y <- unifed.kappa.prime.inverse(y)
    aux.mu <- unifed.kappa.prime.inverse(mu)
    2*( y * ( aux.y - aux.mu )  - unifed.kappa(aux.y) + unifed.kappa(aux.mu)  )
}

test_that("Checking behavior of unifed.kappa.prime.inverse around zero",{    
    for( x in c(0.01,0.05,0.08)){
        expect_equal(unifed.kappa.prime.inverse(x), - unifed.kappa.prime.inverse(1- x) )
    }
})

test_that("Checking dependency of the deviance C's kappa inverse code",{
    for( x in list(
                  c(0.01,0.02),
                  c(0.05,0.9),
                  c(0.2,0.5),
                  c(0.08,0.97)                  
              )){
        expect_equal( a.deviance(x[1],x[2]) , unifed.deviance(x[1],x[2]))
        expect_equal( a.deviance(x[2],x[1]) , unifed.deviance(x[2],x[1]))
    }
})

test_that("Right domain for unifed.kappa.prime.inverse.one",{
    expect_error(unifed.kappa.prime.inverse(1.01))
    expect_error(unifed.kappa.prime.inverse(-0.01))    
})

test_that("Right domain for unifed.varf",{
    expect_error(unifed.varf(1.01))
    expect_error(unifed.varf(-0.01))    
})


