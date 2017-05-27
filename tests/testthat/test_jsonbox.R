# test jsonbox

testthat::context('box box atoms')

testthat::test_that('detects atoms appropriately', {
  
  # setup
  unboxd <- jsonlite::toJSON(list(ac=11L, ab=22L, c=c(3L, 6L)), auto_unbox=TRUE)
  inboxd <- jsonlite::toJSON(list(a=1L, b=2L, c=c(3L, 6L)), auto_unbox=FALSE)
  noboxd <- jsonlite::toJSON(list(a="dumb string", 
                                  b=list(c=9999L, d=list(list(11L)))),
                             auto_unbox=TRUE)
  
  # predicate function
  testthat::expect_identical(hasUnboxedAtoms(unboxd), TRUE)
  testthat::expect_identical(hasUnboxedAtoms(inboxd), FALSE)
  testthat::expect_identical(hasUnboxedAtoms(noboxd), TRUE)
  
  # boxing pt 1
  testthat::expect_identical(boxAtoms(noboxd), 
                               structure(paste0('{"a":["dumb string"],', 
                                                '"b":{"c":[9999],"d":[[11]]}}'), 
                                         class='json'))
  
  # boxing pt 2
  testthat::expect_identical(boxAtoms(unboxd),
                             structure('{"ac":[11],"ab":[22],"c":[3,6]}', 
                                       class='json'))
  
  # unboxing
  testthat::expect_identical(unboxAtoms(inboxd),
                             structure('{"a":1,"b":2,"c":[3,6]}', class='json'))
  
})