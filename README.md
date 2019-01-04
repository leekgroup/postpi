# IAP
ss = 100
n_sim = 50
method = "rf"

beta1 = 1
beta2 = 0
beta3 = 4
beta4 = 0
sim_dat_tv = iap_sim(ss, n_sim, beta1, beta2, beta3, beta4, method)
ypy_plot(sim_dat_tv)
sim_dat_tv_nested = fit_IAP_Inf(sim_dat_tv)

t_t_iap = t_IAP_Inf(sim_dat_tv_nested)
df1 = Deriv_BC(sim_dat_tv_nested,ss)
df2 = BS_test(sim_dat_tv_nested,ss)
df3 = BS_rel(sim_dat_tv_nested,ss)

df1
