

#w <- UKAnophelesPlumbeus %1%
#          UKAir %2%
#          OneHundredBackground %3%
#          LogisticRegression %4%
#          PrintMap



`%1%` <- function(occurrence, covariate){
  occSub <- substitute(occurrence)
  covSub <- substitute(covariate)

  occCov <- list(occurrence = occSub, covariate = covSub)

}


`%2%` <- function(occCov, process){
  proSub <- substitute(process)
  
  proc <- c(occCov, process = proSub)
  
}



`%3%` <- function(process, model){
  modSub <- substitute(model)

  mod <- c(process, model = modSub)
}



`%4%` <- function(model, output){

  outSub <- substitute(output)
  work <- c(model, output = outSub)
  
  workflow <- do.call(workflow, work)
  return(workflow)
}




