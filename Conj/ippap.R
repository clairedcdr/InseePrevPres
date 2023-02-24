rm(list=ls())
# library(pRev)
# library(insee)
# library(openxlsx)
# library(vars)
# library(urca)
# library(tsDyn)
# library(haven)      
# library(collapse)   
# library(magrittr)   
# library(tseries)    
# library(lmtest)     
# library(sandwich)   
# library(xts)        
# library(data.table)
# library(broom)
# library(MSwM)
# library(ggplot2)
# library(stringr)
library(dynlm)
library(claiRe)

############ Estim IPPAP hors fruits et légumes

deb_estim = c(1980,1)
fin_estim= c(2019,4)
fin_period= c(2022,2)
fin_estim_ <- fin_estim[1] + (fin_estim[2]-1)/4

##On passe au log 
ts_list = readRDS(file="../Conj/ts_list.Rdata")
names(ts_list)


df_log =cbind(time = time(ts_list$ippap),
              tend00 = ts(seq_along(ts_list$ippap), start = start(ts_list$ippap),
                          frequency = frequency(ts_list$ippap)),
              ippap = log(ts_list$ippap) ,
              ble = log(ts_list$ble),
              lait = log(ts_list$milk_EU),
              matprem = log(ts_list$prix_importationAZ),
              gaz = log(ts_list$gaz),
              brent = log(ts_list$brent),
              change = log(ts_list$change),
              prodLAZ = log(ts_list$prodLAZ),
              prix_prodAZ = log(ts_list$prix_prodAZ))
df_log <- window(df_log, start = deb_estim)

#ECM linéaire


coint = lm(ippap ~ tend00 + ble+ matprem              ,
           data = window(df_log, end = fin_estim) )
summary(coint)
resid = ts(df_log[,"ippap"] - predict(coint, df_log),
           start = start(df_log),
           frequency = frequency(df_log))
# adf.test(resid);dwtest(coint);plot(ts(resid,start = deb_estim,frequency = 4),type = 'l')
new_data = ts.union(df_log, resid, as.numeric(time(df_log) == 2008.75))
colnames(new_data) <- c(colnames(df_log), "resid", "dum0804")
ecm_lin = dynlm(diff(ippap,1) ~ dum0804+ lag(diff(ippap, 1), -1) +lag(diff(prodLAZ,1),-2) + 
               diff(ble,1) +lag(diff(lait,1),-1) + lag(diff(change,1),-2) + diff(change,1) +
               lag(resid,-1),
               data = window(new_data, end = fin_estim))
ecm_lin_compl = dynlm(diff(ippap,1) ~ dum0804+ lag(diff(ippap, 1), -1) +lag(diff(prodLAZ,1),-2) + 
                  diff(ble,1) +lag(diff(lait,1),-1) + lag(diff(change,1),-2) + diff(change,1) +
                  lag(resid,-1),
                data = window(new_data, end = 2022.5))
hansen.test(ecm_lin)
hansen.test(ecm_lin_compl)

y = diff(new_data[,"ippap"],1)
ecm_ssm <- ssm_lm(ecm_lin, trend = FALSE, 
                  fixed_intercept = FALSE, fixed_trend = FALSE, fixed_var = c(TRUE, rep(FALSE, 8)))
ecm_ssm_compl <- ssm_lm(ecm_lin_compl, trend = TRUE, 
                  fixed_intercept = FALSE, fixed_trend = FALSE, fixed_var = c(TRUE, rep(TRUE, 8)),
                  var_var = c(0, rep(10^6, 8)))
ecm_ssm_compl_def <- ssm_lm(ecm_lin_compl, trend = FALSE, 
                            fixed_var = c(TRUE, rep(FALSE, 8)),
                            var_var = c(0, rep(0.1, 8)))
fitted_ssm_comp_ss = y - ecm_ssm_compl$smoothed_states[,"noise"]
fitted_ssm_comp_fs = y - ecm_ssm_compl$filtering_states[,"noise"]
fitted_ssm_comp_ss_def = y - ecm_ssm_compl_def$smoothed_states[,"noise"]
fitted_ssm_comp_fs_def = y - ecm_ssm_compl_def$filtering_states[,"noise"]

data_p = ts.intersect(window(y, start = 2010),
                      fitted_ssm_comp_fs,
                      fitted_ssm_comp_fs_def,
                      fitted_ecm_full,
                      f_ets,
                      forecast(ets(window(y, start = 2000))) |> fitted())
AQLTools::graph_ts(data_p,
                   legende = c("y", "SSM var elevee", "SSM def", "lm", "ets"))
AQLTools::graph_ts(data_p[,c(1,4,5,6)],
                   legende = c("y","lm", "ets", "ets2"))
apply((y - data_p)^2,2, mean, na.rm = TRUE)
plot(window(y, start = 2010))
AQLTools::hc_stocks(data_p,
                   legende = c("y", "SSM var elevee", "SSM def", "lm"))
# lines(fitted_ecm, col = "blue")
lines(fitted_ssm_comp_fs, col = "red")
lines(fitted_ecm_full, col = "blue")
ecm_ssm_compl <- ssm_lm(ecm_lin_compl, trend = TRUE, 
                        fixed_intercept = TRUE, fixed_trend = TRUE, fixed_var = c(TRUE, rep(TRUE, 8)),
                        var_var = c(0, rep(10^6, 8)),
                        var_intercept = 10,
                        var_trend = 10)
ecm_ssm_compl <- ssm_lm2(ecm_lin_compl, trend = FALSE, 
                        fixed_intercept = FALSE, fixed_trend = FALSE, fixed_var = c(TRUE, rep(TRUE, 8)),
                        var_var = c(0, rep(10^6, 8)),var_noise = 10^2,
                        var_fixed = 0.1)
fitted_ecm_full = rowSums(apply(cbind(1, ecm_lin_compl$model[,-1]),2, as.numeric) %*% diag(as.numeric(coef(ecm_lin))))
fitted_ecm_full = ts(fitted_ecm_full, end = 2022.5, frequency = 4)
fitted_ecm = fitted((ecm_lin))
resid_ssm = (ecm_ssm$smoothed_states[,"noise"])
fitted_ssm = y - resid_ssm
resid_ssm_comp_ss = (ecm_ssm_compl$smoothed_states[,"noise"])
resid_ssm_comp_fs = (ecm_ssm_compl$filtering_states[,"noise"])

fitted_ssm_comp_ss = y - resid_ssm_comp_ss
fitted_ssm_comp_fs = y - resid_ssm_comp_fs

# library(forecast)
# fets <- function(x, h){
#   forecast(ets(x, model = "ZZN"), h = h)
# }
# # On enlève les 3 premières années pour X-13
# e_ets <- tsCV(window(y, start = 2000), fets, h = 1, initial = 3*12)
# f_ets <- y - e_ets
  
plot(window(y, start = 2010))
# lines(fitted_ecm, col = "blue")
lines(fitted_ssm_comp_fs, col = "red")
lines(fitted_ecm_full, col = "blue")
# lines(lag(y, - 1), col = "darkgreen")
# lines(f_ets, col = "orange")
lines(ets(window(y, start = 2000), model = "ZZN") |> fitted(), col = "orange")

all_f = (cbind(y, fitted_ssm_comp_fs, fitted_ecm_full, f_ets))
AQLTools::hc_stocks(all_f)

resid = (cbind(y - lag(y, -1), y - fitted_ssm_comp_fs, y - fitted_ecm_full, y - f_ets))
colMeans(window(resid,start = 2020, end = 2022.25)^2)

plot(window(y, end = end(fitted_ssm)))
lines(fitted_ecm, col = "blue")
lines(fitted_ssm, col = "red")

plot(window(y, start = 2008))
# lines(fitted_ecm, col = "blue")
lines(fitted_ssm_comp_fs, col = "red")
lines(fitted_ecm_full, col = "blue")

plot(resid_ecm)


ssm_lm2 <- function(model, trend = FALSE,
                   var_intercept = 0.1,
                   var_trend = 0.1,
                   var_var = 10,
                   start_break = 2020,
                   fixed_intercept = TRUE,
                   fixed_trend = TRUE,
                   fixed_var = TRUE, ...) {
  data <- get_data(model)
  intercept <- length(grep("Intercept", names(coef(model)))) > 0
  jmodel <- rjd3sts::model()
  jeq <- rjd3sts::equation("eq1",variance = 0, fixed = TRUE)  #ne pas modifier
  fixed_var = rep(fixed_var, ncol(data)-1)[1:(ncol(data)-1)]
  names(fixed_var) = colnames(data)[-1]
  
  
  if (trend) {
    rjd3sts::add(jmodel, rjd3sts::locallineartrend("Trend",
                                                   levelVariance  = var_intercept, fixedLevelVariance  = fixed_intercept,
                                                   slopevariance = var_trend, fixedSlopeVariance = fixed_trend))
    rjd3sts::add.equation(jeq, "Trend", coeff = 1, fixed = TRUE) #ne pas modifier
  } else if (intercept) {
    rjd3sts::add(jmodel, rjd3sts::locallevel("(Intercept)", variance = var_intercept, fixed = fixed_intercept))
    rjd3sts::add.equation(jeq, "(Intercept)", coeff = 1, fixed = TRUE) #ne pas modifier
  }
  var = rep(1, nrow(data))
  var[time(data) >= start_break] = 1
  for (nom_var in colnames(data)[2]) {
    rjd3sts::add(jmodel, rjd3sts::reg(nom_var, x = data[, nom_var], var = 0, fixed = fixed_var[nom_var]))
    rjd3sts::add.equation(jeq, nom_var, coeff = 1, fixed = TRUE) #ne pas modifier
  }
  for (nom_var in colnames(data)[3]) {
    rjd3sts::add(jmodel, rjd3sts::varreg(nom_var, x = data[, nom_var], stderr = var, 0.1))
    rjd3sts::add.equation(jeq, nom_var, coeff = 1, fixed = TRUE) #ne pas modifier
  }
  
  rjd3sts::add(jmodel, rjd3sts::noise("noise", variance = 0.1, fixed = FALSE)) #ne pas modifier
  rjd3sts::add.equation(jeq, "noise", coeff = 1, fixed = TRUE) #ne pas modifier
  
  rjd3sts::add(jmodel, jeq)
  
  jmodestimated <- rjd3sts::estimate(jmodel, data = data[,1])
  
  smoothed_states <- rjd3sts::smoothedstates(jmodestimated)
  smoothed_var <- rjd3sts::smoothedstatesstdev(jmodestimated)
  filtered_states <- rjd3sts::filteredstates(jmodestimated)
  filtering_states <- rjd3sts::filteringstates(jmodestimated)
  filtered_var <- rjd3sts::filteredstatesstdev(jmodestimated)
  
  cmp_names <- rjd3toolkit::result(jmodestimated, "ssf.cmpnames")
  
  if (trend) {
    cmp_names <- c("(Intercept)", cmp_names)
  }
  colnames(smoothed_states) <- colnames(smoothed_var) <-
    colnames(filtered_states) <-
    colnames(filtering_states) <-
    colnames(filtered_var) <-
    cmp_names
  filtering_states[,ncol(filtering_states)] = data[,1] - rowSums(cbind(1, data[,-1]) * filtering_states[,-ncol(filtering_states)])
  data_tmp = smoothed_states
  data_tmp[,2] = data_tmp[,2] - data_tmp[,1]
  smoothed_states[,ncol(filtered_states)] - 
    (data[,1] - rowSums(cbind(1,1,  data[,-1]) * smoothed_states[,-ncol(filtered_states)]))
  
  
  residuals <- rjd3toolkit::result(jmodestimated, "likelihood.residuals")
  ser <- rjd3toolkit::result(jmodestimated, "likelihood.ser")
  if (is.ts(data)){
    smoothed_states <- ts(smoothed_states, end = end(data), frequency = frequency(data))
    smoothed_var <- ts(smoothed_var, end = end(data), frequency = frequency(data))
    filtered_states <- ts(filtered_states, end = end(data), frequency = frequency(data))
    filtering_states <- ts(filtering_states, end = end(data), frequency = frequency(data))
    
    filtered_var <- ts(filtered_var, end = end(data), frequency = frequency(data))
    residuals <- ts(residuals, end = end(data), frequency = frequency(data))
  }
  
  res <- list(smoothed_states = smoothed_states,
              smoothed_var = smoothed_var,
              filtered_states = filtered_states,
              filtering_states = filtering_states,
              filtered_var = filtered_var,
              # residuals = residuals, # ??? enlevé car on ne comprend pas à quoi cela correspond
              # ser = ser,
              data = data)
  res
  # Clarifier : TODO ALAIN
  # scalingfactor ?
  # difference entre varreg et reg
  # reg : variance fixe ou estimée ? si fixed = FALSE, est-ce que cela veut bien dire que la variance est estimée ?
  # reg : initialisation ?
  # residuals ?
}

ssm_lm2 <- function(model, trend = FALSE,
                   var_intercept = 0,
                   var_trend = 0,
                   var_var = 1,
                   var_noise = 10,
                   start_break = 2020,
                   fixed_intercept = TRUE,
                   fixed_trend = TRUE,
                   fixed_var = TRUE, ...) {
  data <- get_data(model)
  intercept <- length(grep("Intercept", names(coef(model)))) > 0
  jmodel <- rjd3sts::model()
  jeq <- rjd3sts::equation("eq1",variance = 0, fixed = TRUE)  #ne pas modifier
  var_var = rep(var_var, ncol(data)-1)[1:(ncol(data)-1)]
  names(var_var) = colnames(data)[-1]
  fixed_var = rep(fixed_var, ncol(data)-1)[1:(ncol(data)-1)]
  names(fixed_var) = colnames(data)[-1]
  
  if (trend) {
    rjd3sts::add(jmodel, rjd3sts::locallineartrend("Trend",
                                                   levelVariance  = var_intercept, fixedLevelVariance  = fixed_intercept,
                                                   slopevariance = var_trend, fixedSlopeVariance = fixed_trend))
    rjd3sts::add.equation(jeq, "Trend", coeff = 1, fixed = TRUE) #ne pas modifier
  } else if (intercept) {
    rjd3sts::add(jmodel, rjd3sts::locallevel("(Intercept)", variance = var_intercept, fixed = fixed_intercept))
    rjd3sts::add.equation(jeq, "(Intercept)", coeff = 1, fixed = TRUE) #ne pas modifier
  }
  
  for (nom_var in colnames(data)[-1]) {
    rjd3sts::add(jmodel, rjd3sts::reg(nom_var, x = data[, nom_var], var = var_var[nom_var], fixed = fixed_var[nom_var]))
    rjd3sts::add.equation(jeq, nom_var, coeff = 1, fixed = TRUE) #ne pas modifier
  }
  
  var = rep(1, nrow(data))
  var[time(data) >= start_break] = var_noise
  rjd3sts::add(jmodel, rjd3sts::varnoise("noise", std = var, fixed = FALSE)) #ne pas modifier
  rjd3sts::add.equation(jeq, "noise", coeff = 1, fixed = TRUE) #ne pas modifier
  
  rjd3sts::add(jmodel, jeq)
  
  jmodestimated <- rjd3sts::estimate(jmodel, data = data[,1])
  
  smoothed_states <- rjd3sts::smoothedstates(jmodestimated)
  smoothed_var <- rjd3sts::smoothedstatesstdev(jmodestimated)
  filtered_states <- rjd3sts::filteredstates(jmodestimated)
  filtering_states <- rjd3sts::filteringstates(jmodestimated)
  filtered_var <- rjd3sts::filteredstatesstdev(jmodestimated)
  
  cmp_names <- rjd3toolkit::result(jmodestimated, "ssf.cmpnames")
  
  if (trend) {
    cmp_names <- c("(Intercept)", cmp_names)
  }
  colnames(smoothed_states) <- colnames(smoothed_var) <-
    colnames(filtered_states) <-
    colnames(filtering_states) <-
    colnames(filtered_var) <-
    cmp_names
  filtering_states[,ncol(filtering_states)] = data[,1] - rowSums(cbind(1, data[,-1]) * filtering_states[,-ncol(filtering_states)])
  
  
  residuals <- rjd3toolkit::result(jmodestimated, "likelihood.residuals")
  ser <- rjd3toolkit::result(jmodestimated, "likelihood.ser")
  if (is.ts(data)){
    smoothed_states <- ts(smoothed_states, end = end(data), frequency = frequency(data))
    smoothed_var <- ts(smoothed_var, end = end(data), frequency = frequency(data))
    filtered_states <- ts(filtered_states, end = end(data), frequency = frequency(data))
    filtering_states <- ts(filtering_states, end = end(data), frequency = frequency(data))
    
    filtered_var <- ts(filtered_var, end = end(data), frequency = frequency(data))
    residuals <- ts(residuals, end = end(data), frequency = frequency(data))
  }
  
  res <- list(smoothed_states = smoothed_states,
              smoothed_var = smoothed_var,
              filtered_states = filtered_states,
              filtering_states = filtering_states,
              filtered_var = filtered_var,
              # residuals = residuals, # ??? enlevé car on ne comprend pas à quoi cela correspond
              # ser = ser,
              data = data)
  res
  # Clarifier : TODO ALAIN
  # scalingfactor ?
  # difference entre varreg et reg
  # reg : variance fixe ou estimée ? si fixed = FALSE, est-ce que cela veut bien dire que la variance est estimée ?
  # reg : initialisation ?
  # residuals ?
}
