## IAP R package

`IAP` is an R package to correct bias in statistics for predicted data when making statistical inference after prediction. This package is currently under development, so there will be further updates to it.

## Installation

    devtools::install_github("SiruoWang/IAP")
    
Package can be loaded into R as follows:

    library('IAP')
    
## Example

We can simulate data with outcomes and four predictors. In the training set, model is trained by `method`. Here, for example, we use random forest model. In the testing and validation sets, we obtain predicted outcomes. All simulated data frame is saved in the data frame `sim_dat_tv`.

`ss = 100
 n_sim = 50
 method = "rf"

 beta1 = 1
 beta2 = 0
 beta3 = 4
 beta4 = 0
 
 sim_dat_tv = iap_sim(ss, n_sim, beta1, beta2, beta3, beta4, method)`
 
We can explore the relationship between real and predicted outcomes in the testing set, and the relationship between real outcome and predictor of interest.
`ypy_plot(sim_dat_tv)`

We can fit the inference after prediction model between predicted outcomes and predictor, and the inference model between real outcomes and predictor in the testing and validation sets, to get statistics from the models.
`sim_dat_tv_nested = fit_IAP_Inf(sim_dat_tv)
 t_t_iap = t_IAP_Inf(sim_dat_tv_nested)`

We provide three bias correction methods to correct bias in statistics due to prediction error in the validation set obtained from fitting inference after predition model between predicted outcomes and predictor of interest. Each of `Deriv_BC()`,`BS_test()`,`BS_rel()` functions provides a data frame with 4 columns: corrected coefficient estimate, standard error, t statistics, and p value. Each data frame contains `n_sim` rows where each row is for one simulation.

`tidy_table_der = Deriv_BC(sim_dat_tv_nested,ss)
 tidy_table_test = BS_test(sim_dat_tv_nested,ss)
 tidy_table_rel = BS_rel(sim_dat_tv_nested,ss)`


