# Check objects creation
# 
# Author: Emilio L. Cano
###############################################################################

context("Object Creation")

# TODO: insert a useful test
testModel <- newSMS("aModel")
test_that("An empty SMS object can be created",
  {
    expect_that(class(testModel), matches("optimSMS"))
  })

