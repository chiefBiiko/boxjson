# test jsonbox

testthat::context('un/boxing atoms')

testthat::test_that('appropriate boxing', {
  
  # setup
  unboxd <- jsonlite::toJSON(list(a=11L, b=22L, c=c(3L, 6L)), auto_unbox=TRUE)
  inboxd <- jsonlite::toJSON(list(a=1L, b=2L, c=c(3L, 6L)), auto_unbox=FALSE)
  noboxd <- jsonlite::toJSON(list(a="dumb string", 
                                  b=list(c=9999L, d=list(list(11L)))),
                             auto_unbox=TRUE)
  
  # Predicate function
  testthat::expect_identical(hasUnboxedAtoms(unboxd), TRUE)
  testthat::expect_identical(hasUnboxedAtoms(inboxd), FALSE)
  testthat::expect_identical(hasUnboxedAtoms(noboxd), TRUE)
  
  # boxing
  testthat::expect_identical(boxAtoms(noboxd), 
                               structure(paste0('{"a":["dumb string"],', 
                                                '"b":{"c":[9999],"d":[[11]]}}'), 
                                         class='json'))
  
})