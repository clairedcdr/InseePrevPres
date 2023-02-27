data_pib <- window(data_pib, start = 2000, end = c(2022,4)
)
data_pib_trunc <- ts(data_pib[!trunc(time(data_pib)) == 2020,], end = c(2022,4),frequency = 4)


mod1 <- dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1, data = data_pib)
#mod2 <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib)
mod3 <- dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3, data = data_pib)

mod1_trunc <- dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1, data = data_pib_trunc)
# mod2_trunc <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib_trunc)
mod3_trunc <- dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3, data = data_pib_trunc)

ind <- cbind(time(data_pib) == 2020, time(data_pib) == 2020.25, time(data_pib) == 2020.5,
             time(data_pib) == 2020.75)
ind <- ts(apply(ind,2, as.numeric), start = start(data_pib), frequency = 4)
data2 <- cbind(data_pib, ind)
colnames(data2) <- c(colnames(data_pib), sprintf("ind2020Q%i", 1:4))

mod1_ind <-dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1 +
                          ind2020Q1 + ind2020Q2 + ind2020Q3 + ind2020Q4,
                        data = data2)
mod2_ind <-dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3 +
                          ind2020Q1 + ind2020Q2 + ind2020Q3 + ind2020Q4,
                        data = data2)
mod3_ind <-dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3 +
                          ind2020Q1 + ind2020Q2 + ind2020Q3 + ind2020Q4,
                        data = data2)
coef(mod1_ind)
coef(mod3_ind)
coef(mod3_trunc)

data2 = cbind(pib1 = data2[,"pib"] - rowSums(ind * coef(mod1_ind)[4:7]),
              pib2 = data2[,"pib"] - rowSums(ind * coef(mod2_ind)[4:7]),
              pib3 = data2[,"pib"] - rowSums(ind * coef(mod3_ind)[4:7]),
              data2)
colnames(data2) = c("pib1", "pib2", "pib3", colnames(data_pib), sprintf("ind2020Q%i", 1:4))

mod1_cor <- dynlm::dynlm(pib1 ~ climat_fr_m1 + diff_climat_fr_m1,
                        data = data2)
mod2_cor <- dynlm::dynlm(pib2 ~ climat_fr_m2 + diff_climat_fr_m2,
                         data = data2)
mod3_cor <- dynlm::dynlm(pib3 ~ climat_fr_m3 + diff_climat_fr_m3,
                         data = data2)

rmse_mod1 <- rmse_prev(mod1_cor, fixed_bw = TRUE)
rmse_mod2 <- rmse_prev(mod3_cor, fixed_bw = TRUE)
rmse_mod3 <- rmse_prev(mod3_cor, fixed_bw = TRUE)

ssm_lm_best(mod1_cor)
ssm_lm_best(mod3_cor)

hansen.test(mod3_cor)
colnames(data2)

ssm_mod1 = ssm_lm(mod1_cor, var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)
ssm_mod2 = ssm_lm(mod2_cor, var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)
ssm_mod3 = ssm_lm(mod3_cor, var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

autoplot(mod1_cor$model[,1]) +
  autolayer(mod1_cor$fitted.values, series = "lm") +
  autolayer(ts(rmse_mod1$model$tvlm$fitted, end = end(data2), frequency = 4), series = "tvlm") +
  autolayer(ssm_mod1$fitted[, "smoothed"], series = "ssm_lm")

autoplot(mod2_cor$model[,1]) +
  autolayer(mod2_cor$fitted.values, series = "lm") +
  autolayer(ts(rmse_mod2$model$tvlm$fitted, end = end(data2), frequency = 4), series = "tvlm") +
  autolayer(ssm_mod2$fitted[, "smoothed"], series = "ssm_lm")

autoplot(mod3_cor$model[,1]) +
  autolayer(mod3_cor$fitted.values, series = "lm") +
  autolayer(ts(rmse_mod3$model$tvlm$fitted, end = end(data2), frequency = 4), series = "tvlm") +
  autolayer(ssm_mod3$fitted[, "smoothed"], series = "ssm_lm")

