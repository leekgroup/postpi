#' @import dplyr
#' @import tidyr
#' @import caret
#' @import ggplot2
#' @import broom
#' @import reshape2
#' @import parallel

#' @title Bias correction in statistical inference after predictions
#' @description This function is used for simulating outcome and predictors from linear case, then using machine learning method to make predictions to get predicted outcome.
#' @param ss data size in each training, testing, and validation set.
#' @param n_sim number of simulations
#' @param beta1 coefficient for the first predictor
#' @param beta2 coefficient for the second predictor
#' @param beta3 coefficient for the third predictor
#' @param beta4 coefficient for the fourth predictor
#' @param method indicate prediction method
#' @return
#'  \item{sim_dat_tv}{data frame for simulated outcome, predictors, and predicted outcomes using specified method in training, testing, and validation set}
#' @keywords provide data frame
#' @examples
#' \dontrun{
#' sim_dat_tv = iap_sim(ss, n_sim, beta1, beta2, beta3, beta4, method)
#' }
#' @export
iap_sim = function(ss, n_sim, beta1, beta2, beta3, beta4, method){

  sim_dat = data.frame(x1 = rnorm(ss*3, mean = 0, sd = 1),
                       x2 = rnorm(ss*3, mean = 0, sd = 1),
                       x3 = rnorm(ss*3, mean = 0, sd = 1),
                       x4 = rnorm(ss*3, mean = 0, sd = 1),
                       e_g = rnorm(ss*3),
                       set = rep(c("training","testing","validation"),each=ss),
                       sim = 1)

  for(i in 2:n_sim){
    sim_dat = rbind(sim_dat, data.frame(x1 = rnorm(ss*3, mean = 0, sd = 1),
                                        x2 = rnorm(ss*3, mean = 0, sd = 1),
                                        x3 = rnorm(ss*3, mean = 0, sd = 1),
                                        x4 = rnorm(ss*3, mean = 0, sd = 1),
                                        e_g = rnorm(ss*3),
                                        set = rep(c("training","testing","validation"),each=ss),
                                        sim = i))
  }

  ## Set the ground truth model
  g = function(beta1,beta2,beta3,beta4,x1,x2,x3,x4){
    return(beta1 * x1 + beta2 * x2 + beta3 * (x3^2) + beta4 * (x4^2))
  }

  sim_dat = sim_dat %>% mutate(y = g(beta1,beta2,beta3,beta4,x1,x2,x3,x4) + e_g)


  ####
  #### fit the prediction model
  ####
  sim_dat_nested = sim_dat %>% nest(-set, -sim)

  sim_dat_nested_train = filter(sim_dat_nested, set=="training")
  sim_dat_nested_train = sim_dat_nested_train %>%
    mutate(model = mclapply(1:n_sim, function(x){
      df = sim_dat_nested_train$data[[x]]
      model = train(y ~. , data = df[-which(colnames(df) %in% "e_g")], method = method)
    }, mc.cores = 2))


  ####
  #### Get predicted values
  ####
  sim_dat_nested_tv = lapply(1:n_sim,function(x){
    sim_dat_nested %>% filter((set == "testing" | set == "validation") & sim == x) %>%
      mutate(pred = purrr::map(data, ~  predict(sim_dat_nested_train$model[[x]], .)))
  })
  sim_dat_tv = sim_dat_nested_tv %>% do.call(rbind, .) %>% unnest()

  return(sim_dat_tv)
}

#' @title plot relationship between outcome and predictor of interest, and relationship between outcome and predicted outcome
#' @description This function is used for making plots between outcome and predictor of interest, and between outcome and predicted outcome
#' @param sim_dat_tv simulated data frame from iap_sim(ss, n_sim, beta1, beta2, beta3, beta4, method)
#' @return
#'  \item{y_yp}{relationship plot between outcome and predicted outcome}
#'  \item{y_x}{relationship plot between outcome and predictor of interest}
#' @keywords provide relationship plot
#' @examples
#' \dontrun{
#' ypy_plot(sim_dat_tv)
#' }
#' @export
ypy_plot = function(sim_dat_tv){

  sim_dat_test = filter(sim_dat_tv, set == "testing") %>% nest(-sim)
  sim_dat_test = sim_dat_test %>%
    mutate(gg_y_yp = purrr::map(data, ~ ggplot(data = ., aes(y, pred)) +
                                  geom_point(color = densCols(.$y, .$pred, bandwidth = 2), size = 2) +
                                  geom_abline(linetype="dashed",color="red",size=1))) %>%
    mutate(gg_y_x = purrr::map(data, ~ ggplot(data = ., aes(x1, y)) +
                                 geom_point(color = densCols(.$x1, .$y, bandwidth = 2), size = 2)))

  return(list(y_yp=sim_dat_test$gg_y_yp[[1]], y_x=sim_dat_test$gg_y_x[[1]]))
}

#' @title inference after prediction model
#' @description specify inference after prediction model using predicted outcome and predictor of interest
#' @param outcome input predicted outcome
#' @param predictor input predictor of interest
#' @return
#'  \item{model}{provide inference after prediction model}
#' @keywords provide IAP model
#' @examples
#' \dontrun{
#' model = IAP_model(outcome, predictor)
#' }
#' @export
IAP_model = function(outcome, predictor){
  model = lm(outcome ~ predictor)
  return(model)
}

#' @title inference model
#' @description specify inference model between real outcome and predictor of interest
#' @param outcome input real outcome
#' @param predictor input predictor of interest
#' @return
#'  \item{model}{provide inference model}
#' @keywords provide inference model
#' @examples
#' \dontrun{
#' model = Inf_model(outcome, predictor)
#' }
#' @export
Inf_model = function(outcome, predictor){
  model = lm(outcome ~ predictor)
  return(model)
}

#' @title relationship model between real and predicted outcome
#' @description specify relationship model between real outcome and predicted outcome
#' @param outcome input real outcome
#' @param predictor input predicted outcome
#' @return
#'  \item{model}{provide relationship model between real and predicted outcome}
#' @keywords provide relationship model
#' @examples
#' \dontrun{
#' model = realpred_model(outcome, predictor)
#' }
#' @export
realpred_model = function(outcome, predictor){
  model = lm(outcome ~ predictor)
  return(model)
}

#' @title fit Inference after prediction model and inference model
#' @description fit Inference after prediction model and inference model
#' @param sim_dat_tv simulated data frame from iap_sim(ss, n_sim, beta1, beta2, beta3, beta4, method)
#' @return
#'  \item{sim_dat_tv_nested}{provide a nested data frame with model fit statistics from IAP and Inf model}
#' @keywords provide IAP and Inf model fit results
#' @examples
#' \dontrun{
#' sim_dat_tv_nested = fit_IAP_Inf(sim_dat_tv)
#' }
#' @export
fit_IAP_Inf = function(sim_dat_tv){
  sim_dat_tv_nested = sim_dat_tv %>% nest(-set, -sim) %>%
    mutate(yp_x = purrr::map(data, ~ tidy(IAP_model(.$pred, .$x1)))) %>%
    mutate(y_x = purrr::map(data, ~ tidy(Inf_model(.$y, .$x1))))

  return(sim_dat_tv_nested)
}

#' @title get t statistics from inference after prediction model and inference model
#' @description get t statistics from necessary models
#' @param sim_dat_tv_nested a nested data frame with model fit statistics from IAP and Inf model
#' @return
#'  \item{df}{a data frame with 2 colomns, each colomn with t statistics from IAP and Inf model}
#' @keywords provide IAP and Inf model fit t statistics
#' @examples
#' \dontrun{
#' t_df = t_IAP_Inf(sim_dat_tv_nested)
#' }
#' @export
t_IAP_Inf = function(sim_dat_tv_nested){
  sim_dat_val_nested = filter(sim_dat_tv_nested, set == "validation") %>%
    mutate(t_iap = purrr::map(sim, function(x){ .$yp_x[[x]][-1,"statistic"]})) %>%
    mutate(t = purrr::map(sim, function(x){ .$y_x[[x]][-1,"statistic"]}))

  return(df = cbind(sim_dat_val_nested %>% select(t_iap,t) %>% unnest()))
}

#' @title correct t statistics from derivation approach
#' @description get corrected t statistics from derivation approach
#' @param sim_dat_tv_nested a nested data frame with model fit statistics from IAP and Inf model
#' @param ss data size in each training, testing, and validation set.
#' @return
#'  \item{tidy_table}{bias corrected statistics table from derivation approach}
#' @keywords provide corrected t statistics from derivation approach
#' @examples
#' \dontrun{
#' t_pstar = Deriv_BC(sim_dat_tv_nested)
#' }
#' @export
Deriv_BC = function(sim_dat_tv_nested,ss){
  ################ Derivation Approach

  #### Correct bias
  sim_dat_test_nested = filter(sim_dat_tv_nested, set == "testing") %>%
    mutate(bias = purrr::map(sim, function(x) {.$yp_x[[x]][-1,"estimate"] - .$y_x[[x]][-1,"estimate"]}))

  sim_dat_val_nested = filter(sim_dat_tv_nested, set == "validation") %>%
    mutate(beta_BC = purrr::map(sim, function(x) {.$yp_x[[x]][-1,"estimate"] - sim_dat_test_nested$bias[[x]]}))

  #### Correct variance estimates
  sim_dat_test_nested = sim_dat_test_nested %>%
    mutate(model_y_yp = purrr::map(data, ~ realpred_model(.$y, .$pred))) %>%
    mutate(yp_y = purrr::map(data, ~ tidy(realpred_model(.$pred, .$y)))) %>%
    mutate(sigma_yx = purrr::map(data, ~ glance(Inf_model(.$y, .$x1)) %>% select(sigma))) %>%
    mutate(sigma_ypy = purrr::map(data, ~ glance(realpred_model(.$pred, .$y)) %>% select(sigma)))


  var_yp <- function(sim){
    gamma1 = sim_dat_test_nested$yp_y[[sim]][-1,"estimate"]
    sigma_p = sim_dat_test_nested$sigma_ypy[[sim]]
    sigma_y = sim_dat_test_nested$sigma_yx[[sim]]

    return(as.numeric(sigma_p + gamma1^2 * sigma_y))
  }


  sim_dat_val_nested = sim_dat_val_nested %>%
    mutate(var_beta_BC = purrr::map(sim, function(sim){
      x = cbind(rep(1, ss), .$data[[sim]] %>% select(x1)) %>% as.matrix()
      var_beta_BC = solve(t(x) %*% x) * var_yp(sim)
      var_beta_BC = var_beta_BC[2,2] ## change code here for generalization
      return(var_beta_BC) })) %>%
    mutate(se_beta_BC = purrr::map(sim, function(sim){ sqrt(var_beta_BC[[sim]])}))

  #### calculate t statistics
  sim_dat_val_nested = sim_dat_val_nested %>%
    mutate(t_pstar = purrr::map(sim, function(x){ .$beta_BC[[x]] / .$se_beta_BC[[x]]}))

  estimate = sim_dat_val_nested %>% select(beta_BC) %>% unnest()
  std_error = sim_dat_val_nested %>% select(se_beta_BC) %>% unnest()
  BC_t = sim_dat_val_nested %>% select(t_pstar) %>% unnest()
  p_val = 2*(1-pt(abs(BC_t[,1]),df=ss-1))
  tidy_table = data.frame(estimate,std_error,BC_t,p_val)
  colnames(tidy_table) = c("estimate","std error","BC_t","p_val")

  return(tidy_table)

}

#' @title correct t statistics from bootstrap approach1
#' @description get corrected t statistics from bootstrap approach: bootstrap from testing set
#' @param sim_dat_tv_nested a nested data frame with model fit statistics from IAP and Inf model
#' @param ss data size in each training, testing, and validation set.
#' @return
#'  \item{tidy_table}{bias corrected statistics table from derivation approach}
#' @keywords provide corrected t statistics from bootstrap approach1
#' @examples
#' \dontrun{
#' t_bs_test = BS_test(sim_dat_tv_nested)
#' }
#' @export
BS_test = function(sim_dat_tv_nested, ss){
  ############### Bootstrap Approach 1: bootsßtrap (Y_p, X_test) and (Y, X_test) from testing set
  bs = 100 # bootstrap bs times from testing set
  set.seed(2018)

  bs_test_nested = filter(sim_dat_tv_nested, set == "testing") %>%
    mutate(bs_iap_df = lapply(1:n_sim, function(sim){
      data = .$data[[sim]]

      beta_list = lapply(1:bs, function(i){
        bs_idx = sample(1:ss,ss,replace = TRUE)
        tidy_df = tidy(IAP_model(data$pred[bs_idx], data$x1[bs_idx]))
        beta = tidy_df[-1,"estimate"]

      }) %>% do.call(rbind,.)
    })) %>%
    mutate(bs_df = lapply(1:n_sim, function(sim){
      data = .$data[[sim]]

      beta_list = lapply(1:bs, function(i){
        bs_idx = sample(1:ss,ss,replace = TRUE)
        tidy_df = tidy(Inf_model(data$y[bs_idx], data$x1[bs_idx]))
        beta = tidy_df[-1,"estimate"]

      }) %>% do.call(rbind,.)
    }))


  bs_test_nested = bs_test_nested %>%
    mutate(bias = purrr::map(sim, function(x){
      beta_iap = .$bs_iap_df[[x]] %>% as.data.frame() %>% select(estimate) %>% apply(.,2,mean)
      beta = .$bs_df[[x]] %>% as.data.frame() %>% select(estimate) %>% apply(.,2,mean)
      bias = beta_iap - beta
    }))  %>%
    mutate(sd_scalar = purrr::map(sim, function(x){
      sd_iap = .$bs_iap_df[[x]] %>% as.data.frame() %>% select(estimate) %>% apply(.,2,sd)
      sd = .$bs_df[[x]] %>% as.data.frame() %>% select(estimate) %>% apply(.,2,sd)
      sd_scalar = sd_iap/sd
    }))


  #### correct both beta estimation and se in validation set
  bs_val_nested = filter(sim_dat_tv_nested, set == "validation") %>%
    mutate(bs_beta_BC = purrr::map(sim, function(x) {.$yp_x[[x]][-1,"estimate"] - bs_test_nested$bias[[x]]})) %>%
    mutate(bs_sd_BC = purrr::map(sim, function(x) {.$yp_x[[x]][-1,"std.error"] / bs_test_nested$sd_scalar[[x]]}))

  #### calculate t statistics
  bs_val_nested = bs_val_nested %>%
    mutate(t_bs_test = purrr::map(sim, function(x){ .$bs_beta_BC[[x]] / .$bs_sd_BC[[x]]}))

  estimate = bs_val_nested %>% select(bs_beta_BC) %>% unnest()
  std_error = bs_val_nested %>% select(bs_sd_BC) %>% unnest()
  BC_t = bs_val_nested %>% select(t_bs_test) %>% unnest()
  p_val = 2*(1-pt(abs(BC_t[,1]),df=ss-1))
  tidy_table = data.frame(estimate,std_error,BC_t,p_val)
  colnames(tidy_table) = c("estimate","std error","BC_t","p_val")

  return(tidy_table)
}


#' @title correct t statistics from bootstrap approach2
#' @description get corrected t statistics from bootstrap approach: bootstrap from validation set using relationship model between real and predicted outcome
#' @param sim_dat_tv_nested a nested data frame with model fit statistics from IAP and Inf model
#' @param ss data size in each training, testing, and validation set.
#' @return
#'  \item{tidy_table}{bias corrected statistics table from derivation approach}
#' @keywords provide corrected t statistics from bootstrap approach2
#' @examples
#' \dontrun{
#' t_bs_real = BS_test(sim_dat_tv_nested)
#' }
#' @export
BS_rel = function(sim_dat_tv_nested, ss){
  ############### Bootstrap Approach 3: bootstrap Y using the relationship model in the testing set
  # relationship model between y and y_p
  Rel_model = function(y, pred){
    model = lm(y ~ pred)
    return(model)
  }

  # fit relationship model in the testing
  yp_y_nested = filter(sim_dat_tv_nested, set == "testing") %>%
    mutate(ypy_model = purrr::map(data, ~ Rel_model(.$y, .$pred)))

  # bootstrap bs times from testing set
  bs = 100
  set.seed(2018)

  bs_val_nested = filter(sim_dat_tv_nested, set == "validation") %>%
    mutate(bbse_df = lapply(1:n_sim, function(sim){
      data = .$data[[sim]]
      model = yp_y_nested$ypy_model[[sim]]
      ## simulate y_p and x first
      beta_list = lapply(1:bs, function(i){
        bs_idx = sample(1:ss,ss,replace = TRUE)
        bs_data = data[bs_idx,]

        sim_y = rnorm(ss, mean=predict(model, bs_data), sd=glance(model)$sigma)
        sim_y_x = tidy(Inf_model(sim_y, bs_data$x1))

        bb = sim_y_x[-1,"estimate"]
        se = sim_y_x[-1,"std.error"]

        df = cbind(bb,se)
      }) %>% do.call(rbind,.)
    }))

  #### Find t statistics
  bs_val_nested = bs_val_nested %>%
    mutate(beta = purrr::map(sim, function(x){
      .$bbse_df[[x]][,"estimate"] %>% mean()
    }))  %>%
    mutate(se = purrr::map(sim, function(x){
      .$bbse_df[[x]][,"std.error"] %>% mean()
    })) %>%
    mutate(t_bs_rel = purrr::map(sim, function(x){
      .$bbse_df[[x]][,"estimate"] %>% mean() / .$bbse_df[[x]][,"std.error"] %>% mean()
    }))

  estimate = bs_val_nested %>% select(beta) %>% unnest()
  std_error = bs_val_nested %>% select(se) %>% unnest()
  BC_t = bs_val_nested %>% select(t_bs_rel) %>% unnest()
  p_val = 2*(1-pt(abs(BC_t[,1]),df=ss-1))
  tidy_table = data.frame(estimate,std_error,BC_t,p_val)
  colnames(tidy_table) = c("estimate","std error","BC_t","p_val")

  return(tidy_table)
}


