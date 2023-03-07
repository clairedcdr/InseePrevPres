climat_fr <- "001565530"
pib <- "010565708"
pib <- lectureBDM(pib)
climat_fr <- lectureBDM(climat_fr)

soldes_trim <- trimestrialise(climat_fr)

pib = (pib / stats::lag(pib, -1)-1)*100

data_pib_90 <- ts.union(pib, soldes_trim, diff(soldes_trim, 1))
colnames(data_pib_90) <- c("pib", sprintf("climat_fr_m%i", 1:3),
                        sprintf("diff_climat_fr_m%i", 1:3))

data_pib_90 <- window(data_pib_90, start = 1990, end = c(2022,4)
)

model <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib_90)

ind <- cbind(time(data_pib_90) == 2020, time(data_pib_90) == 2020.25, time(data_pib_90) == 2020.5,
             time(data_pib_90) == 2020.75)
ind <- ts(apply(ind,2, as.numeric), start = start(data_pib_90), frequency = 4)
data3 <- cbind(data_pib_90, ind)
colnames(data3) <- c(colnames(data_pib_90), sprintf("ind2020Q%i", 1:4))

model_ind <-dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2 +
                          ind2020Q1 + ind2020Q2 + ind2020Q3 + ind2020Q4,
                        data = data3)

data3 = cbind(pib_cor = data3[,"pib"] - rowSums(ind * coef(model_ind)[4:7]),
              data3)
colnames(data3) = c("pib_cor", colnames(data_pib_90), sprintf("ind2020Q%i", 1:4))

model_cor <- dynlm::dynlm(pib_cor ~ climat_fr_m2 + diff_climat_fr_m2,
                         data = data3)

rmse = rmse_prev(model_cor, fixed_bw = TRUE, var_fixes = fixed_coefficients(model_cor))
sort(rmse$rmse[[1]])
sort(rmse$rmse[[2]])

x = piece_reg(model_cor, break_dates = 2011)
summary(x$model)
x$model$residuals %>% rmse_res
soos_x = soos_prev(x$model)

dygraph(cbind(PIB = get_data(model_cor)[,1],
              lm = rmse$model$lm$fitted.values,
              tvlm = rmse$model$tvlm$fitted))%>%
  dyOptions(colors = c("black", "orange", "red"))

dygraph(get_data(model_cor)[,1], main = "PIB entre 1990 et 2022", xlab = "Time") %>%
  dyOptions(colors = c("black"))

dygraph(cbind(PIB = get_data(model_cor)[,1], lm = rmse$model$lm$fitted.values), main = "Fitted values linear model", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange"))

dygraph(cbind(PIB = get_data(model_cor)[,1], lm = rmse$model$lm$fitted.values, piecelm = rmse$model$piece_lm$fitted.values), main = "Fitted values piecewise regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "green"))

dygraph(cbind(PIB = get_data(model_cor)[,1], lm = rmse$model$lm$fitted.values, tvlm = rmse$model$tvlm_fixe$fitted), main = "Fitted values locale regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "blue"))

rmse2 = rmse_prev(model_ind, fixed_bw = TRUE)

dygraph(get_data(model_ind)[,1], main = "PIB entre 1990 et 2022", xlab = "Time") %>%
  dyOptions(colors = c("black"))

dygraph(cbind(PIB = get_data(model_ind)[,1], lm = rmse2$model$lm$fitted.values), main = "Fitted values linear model", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange"))

dygraph(cbind(PIB = get_data(model_ind)[,1], lm = rmse2$model$lm$fitted.values, piecelm = rmse2$model$piece_lm$model$fitted.values), main = "Fitted values piecewise regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "green"))

dygraph(cbind(PIB = get_data(model_ind)[,1], lm = rmse2$model$lm$fitted.values, tvlm = rmse2$model$tvlm$fitted), main = "Fitted values locale regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "blue"))
