# test boxjson

testthat::context('box box atoms')

testthat::test_that('detects atoms appropriately', {
  
  # setup
  unboxd <- jsonlite::toJSON(list(ac=11L, ab=22L, c=c(3L, 6L)), auto_unbox=TRUE)
  inboxd <- jsonlite::toJSON(list(a=1L, b=2L, c=c(3L, 6L)), auto_unbox=FALSE)
  noboxd <- jsonlite::toJSON(list(a="dumb string", 
                                  b=list(c=9999L, d=list(list(11L)))),
                             auto_unbox=TRUE)
  pkgjson <- paste0("{\"name\":\"sheater\",\"version\":\"1.0.0\",", 
                     "\"description\":\"\",\"main\":\"index.js\",",
                     "\"scripts\":{\"test\":\"echo\\\"Error:no test specified\\\"&&exit1\"},",
                     "\"keywords\":[],\"author\":\"\",\"license\":\"ISC\",",
                     "\"dependencies\":{\"express\":\"^4.15.3\"}}")
  
  # predicate function
  testthat::expect_identical(hasUnboxedAtom(unboxd), TRUE)
  testthat::expect_identical(hasUnboxedAtom(inboxd), FALSE)
  testthat::expect_identical(hasUnboxedAtom(noboxd), TRUE)
  testthat::expect_identical(hasUnboxedAtom(pkgjson), TRUE)
  testthat::expect_identical(hasUnboxedAtom(boxAtoms(pkgjson)), FALSE)
  testthat::expect_identical(hasUnboxedAtom('[[551],[66]]'), FALSE)
  testthat::expect_identical(hasUnboxedAtom('[[551],66]'), TRUE)
  
  # boxing pt 1
  testthat::expect_identical(boxAtoms(noboxd), 
                               structure(paste0('{"a":["dumb string"],', 
                                                '"b":{"c":[9999],"d":[[11]]}}'), 
                                         class='json'))
  
  # boxing pt 2
  testthat::expect_identical(boxAtoms(unboxd),
                             structure('{"ac":[11],"ab":[22],"c":[3,6]}', 
                                       class='json'))
  
  # boxing pt 3
  testthat::expect_identical(boxAtoms('3,6'), 
                             structure('[3,6]', class='json'))
  testthat::expect_identical(boxAtoms('"ac", "ab"'), 
                             structure('["ac","ab"]', class='json'))
  # boxing pt 4
  testthat::expect_identical(boxAtoms('1'), structure('[1]', class='json'))
  testthat::expect_identical(boxAtoms('true'), 
                             structure('[true]', class='json'))
  testthat::expect_identical(boxAtoms('null'), 
                             structure('[null]', class='json'))
  testthat::expect_identical(boxAtoms('[false]'), 
                             structure('[false]', class='json'))
  
  # boxing pt 5
  testthat::expect_identical(boxAtoms('{"hello": [3]}'), 
                             structure('{"hello":[3]}', class='json'))
  
  # boxing pt 6
  testthat::expect_identical(boxAtoms('[77,44],["doo"]'),
                             structure('[[77,44],["doo"]]', class='json'))
  
  # boxing pt 7
  testthat::expect_identical(boxAtoms('[[77,44],"doo"]'),
                             structure('[[77,44],"doo"]', class='json'))
  
  # boxing pt 8
  testthat::expect_identical(boxAtoms('[4,1,9]'),
                             structure('[4,1,9]', class='json'))
  
  # unboxing pt 1
  testthat::expect_identical(unboxAtoms(inboxd),
                             structure('{"a":1,"b":2,"c":[3,6]}', class='json'))
  testthat::expect_identical(unboxAtoms('[2,3,4]'), 
                             structure('[2,3,4]', class='json'))
  
  # unboxing pt 2
  testthat::expect_identical(unboxAtoms('[2]'), 
                             structure('2', class='json'))
  testthat::expect_identical(unboxAtoms('[null]'), 
                             structure('null', class='json'))
  
  # unboxing pt 3
  testthat::expect_identical(unboxAtoms('[[55],[66]]'),
                             structure('[55,66]', class='json'))
  
  # unboxing pt 4
  testthat::expect_identical(unboxAtoms('[["hello,world"],[66]]'),
                             structure('["hello,world",66]', class='json'))
  
})