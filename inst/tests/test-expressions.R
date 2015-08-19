## Test set expressions

context("Getting expressions")

data(mod1SMS)
smsTest <- mod1SMS
lapply(c(TRUE, FALSE), function(s){
  lapply(c(TRUE, FALSE), function(z){
    lapply(optimr:::implemFormats, function(y) {
      sapply(SMSsets(smsTest)$id, function(x) {
        getExpr(smsTest, "sets", x, y, z, SET = s)
      })
    })
  })
})