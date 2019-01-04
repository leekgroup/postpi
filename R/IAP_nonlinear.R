#' @import dplyr
#' @import tidyr
#' @import caret
#' @import ggplot2
#' @import broom
#' @import reshape2
#' @import parallel
#' @import InterVA4
#'
#' @title Bias correction in statistical inference after predictions for categorical case
#' @description This function is used for generating categorical verbal autopsy data
#' @param n.sim number of simulations
#' @return
#'  \item{sim.test}{data frame for simulated outcome, predictors, and predicted outcomes using specified method in training, testing, and validation set}
#' @keywords provide data frame
#' @examples
#' \dontrun{
#' sim.test = VA_data(n.sim)
#' }
#' @export
VA_data <- function(n.sim){

  rm(list=ls())


  ##load sim-func.R, this is what run
  source('sim_func.R')


  ##things you need to set
  # N, sample size to simulate
  # cond.prob= probability of symptom given cause
  # csmf= overall population cause specific mortality fraction
  # seed= the seed to use
  # group= set equal to 1.  this is only necessary in cases when you have multiple indices (e.g. different hospitals) so not relevant for us


  #get the conditional probabilities
  #from the InterVA4 package
  data(probbase)
  #remove some that are from "external causes", eg traffic accidents
  prob.interva <- c(0.9999, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002,
                    0.001, 0.0005, 0.0001, 0.00001, 0.000001)
  #the probbase conditional probabilities are in given as letter grades, this converts them to numbers using the above scale
  #note that this scale is highly skewed so the presence/absense of a particular symptom will be very meaningful
  cond.prob.sim=LevelTrans(probbase[2:246, 17:76],prob.interva)
  #cond.prob.sim=LevelTrans(probbase[2:246, 17:18],prob.interva)

  ##now the csmfs
  #Option 1: uniform cdfs
  csmf.sim=runif(60,.81,.99)
  #csmf.sim=runif(2,.51,.99)
  csmf.sim=csmf.sim/sum(csmf.sim)
  #Option 2: cdf from a HDSS site, will be unbalanced
  #DB link to download: https://www.dropbox.com/s/xzzd5t74qx3taom/InterVA-Agincourt.Rda?dl=0
  load("InterVA-Agincourt.Rda")
  csmf.agin=csmf.prior/sum(csmf.prior)

  #set the seed
  seed.sim=round(rnorm(1,4000,250))

  # try it out
  sim.test=sim(N=n.sim,cond.prob=cond.prob.sim,csmf=csmf.agin,seed=seed.sim,group=1)

  #note this returns a list
  #first item is the N*#symptoms(246) set of symptoms
  #second item is the assigned cause (1-60)
  #third item is the matrix of conditional probabilites used as input

  #we can check and see that we get a distribution in the csmf outcome that's similar to the input
  #round(csmf.agin,4)
  #table(sim.test[[2]])/n.sim
  return(sim.test)
}




#' @title simulate categorical data frame
#' @description This function is used for simulating outcome and predictors from nonlinear case, then using machine learning method to make predictions to get predicted outcome.
#' @param ss data size in each training, testing, and validation set.
#' @param n.sim number of simulations
#' @param sim.test simulated verbal autopsy data
#' @param method specify prediction method
#' @return
#'  \item{sim.test}{data frame for simulated outcome, predictors, and predicted outcomes using specified method in training, testing, and validation set}
#' @keywords provide data frame
#' @examples
#' \dontrun{
#' sim_dat_tv = iap_sim_nonlinear(ss, n_sim, sim.test, method)
#' }
#' @export
iap_sim_nonlinear = function(ss, n_sim, sim.test, method){
  ## a vector indicates whether outcome == 2 (infectious disease: pneumonia)
  outcome_2 = rep(0,length(sim.test[[2]]))
  outcome_2[which(sim.test[[2]] == 2)] = 1

  #####
  # filter relevant covariates for testing
  x = as.matrix(sim.test[[1]][,-1])
  x[which(x == "Y")] = 1
  x[which(x == "")] = 0
  x <- apply(x,2,as.numeric)
  syms_idx = order(colSums(x),decreasing = TRUE)[1:200] #1:10 for testing
  syms = colnames(x)[syms_idx]
  #####

  sim_dat = data.frame(outcome = outcome_2,
                       sim.test[[1]][,syms],#sim.test[[1]][,-1],
                       set = rep(c("training","testing","validation"),each=ss),
                       sim = 1)

  for(i in 2:n_sim){
    sim.test = IAP_simulation(n.sim)
    ## a vector indicates whether outcome == 2 (infectious disease: pneumonia)
    outcome_2 = rep(0,length(sim.test[[2]]))
    outcome_2[which(sim.test[[2]] == 2)] = 1

    sim_dat = rbind(sim_dat, data.frame(outcome = outcome_2,
                                        sim.test[[1]][,syms],#sim.test[[1]][,-1],,
                                        set = rep(c("training","testing","validation"),each=ss),
                                        sim = i))
  }



  ####
  #### fit the prediction model
  ####
  sim_dat_nested = sim_dat %>% nest(-set, -sim)

  ## for all simulations and all sets(train, test, val), we filter out columns(symptoms) that contain all 0s across observations
  temp = lapply(sim_dat_nested$data,function(x){
    x = as.matrix(x)
    x[which(x == "Y")] = 1
    x[which(x == "")] = 0
    x <- apply(x,2,as.numeric)
    x <- x[,!(colSums(x) == 0)]
    x = as.data.frame(apply(x,2,as.factor))
  })

  syms = lapply(temp, function(x) colnames(x)) %>% Reduce(intersect, .)
  sim_dat_nested$data = lapply(temp, function(x){ x[,syms]})

  ## fit rf model
  sim_dat_nested_train = filter(sim_dat_nested, set=="training") %>%
    mutate(model = purrr::map(data, ~  train(outcome ~ ., data = ., method = 'rf', importance = TRUE)))




  ####
  #### Get predicted values
  ####
  sim_dat_nested_tv = lapply(1:n_sim,function(x){
    sim_dat_nested %>%
      filter((set == "testing" | set == "validation") & sim == x) %>%
      mutate(pred = purrr::map(data, ~  predict(sim_dat_nested_train$model[[x]], .)))})

  sim_dat_tv = sim_dat_nested_tv %>% do.call(rbind, .) %>% unnest()

  ## relationship between Y and Y_p
  ## problem: predict all outcomes as 0s, no 1s are predicted using random forest
  #print(confusionMatrix(sim_dat_tv$pred, sim_dat_tv$outcome))

  return(sim_dat_tv)
}
