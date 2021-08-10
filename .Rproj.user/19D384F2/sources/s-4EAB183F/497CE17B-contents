### speeding up

library(microbenchmark)

#microbenchmark(plyr::mdply(testrun, COVIDinfectioncalculator),
#               plyr::mdply(testrun, COVIDinfectioncalculator))


library(profvis)

profvis({
  run_model("Test", 10)
})
