climat_fr <- "001565530"
pib <- "010565708"
pib <- lectureBDM(pib)
climat_fr <- lectureBDM(climat_fr)

soldes_trim <- trimestrialise(climat_fr)

pib = (pib / stats::lag(pib, -1)-1)*100

data_pib_22 <- ts.union(pib, soldes_trim, diff(soldes_trim, 1))
colnames(data_pib_22) <- c("pib", sprintf("climat_fr_m%i", 1:3),
                        sprintf("diff_climat_fr_m%i", 1:3))

data_pib_23 <- window(data_pib_22, start = 2023, end = 2023)
data_pib_23 <- c("(Intercept)" = 1, data_pib_23[, c("climat_fr_m2", "diff_climat_fr_m2")])
data_pib_22 <- window(data_pib_22, start = 1990, end = c(2022,4))

model <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib_22)

ind <- cbind(time(data_pib_22) == 2020, time(data_pib_22) == 2020.25, time(data_pib_22) == 2020.5,
             time(data_pib_22) == 2020.75)
ind <- ts(apply(ind,2, as.numeric), start = start(data_pib_22), frequency = 4)
data3 <- cbind(data_pib_22, ind)
colnames(data3) <- c(colnames(data_pib_22), sprintf("ind2020Q%i", 1:4))

model_ind <-dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2 +
                          ind2020Q1 + ind2020Q2 + ind2020Q3 + ind2020Q4,
                        data = window(data3, start = 2000))

data3 = cbind(pib_cor = data3[,"pib"] - rowSums(ind * coef(model_ind)[4:7]),
              data3)
colnames(data3) = c("pib_cor", colnames(data_pib_22), sprintf("ind2020Q%i", 1:4))

model_cor <- dynlm::dynlm(pib_cor ~ climat_fr_m2 + diff_climat_fr_m2,
                         data = window(data3, start = 2000))

## Modèle corrigé

rmse_cor = rmse_prev(model_cor, bw = 0.294994, fixed_bw = TRUE, break_dates = 2011)

ssm_cor = ssm_lm(model_cor, var_intercept = 0.1, var_variables = 0.1, fixed_var_intercept = FALSE, fixed_var_variables = FALSE)

ssm_oos_cor = ssm_lm_oos(model_cor, var_intercept = 0.1, var_variables = 0.1, fixed_var_intercept = FALSE, fixed_var_variables = FALSE)

cor_last_coef_lm = last_coef(rmse_cor$model$lm)
cor_last_coef_tvlm = last_coef(rmse_cor$model$tvlm)
cor_last_coef_piece = last_coef(rmse_cor$model$piece_lm$model)[4:6]
cor_last_coef_ssm = tail(ssm_cor$smoothed_states,1)[1:3]

cor_prev_23_lm = sum(data_pib_23*cor_last_coef_lm)
cor_prev_23_tvlm = sum(data_pib_23*cor_last_coef_tvlm)
cor_prev_23_piece = sum(data_pib_23*cor_last_coef_piece)
cor_prev_23_ssm = sum(data_pib_23*cor_last_coef_ssm)

plot_cor_previsions = dygraph(cbind(PIB = get_data(model_cor)[,1],
           lm = ts(c(rmse_cor$prevision$prev_lm$prevision, cor_prev_23_lm), end = 2023, frequency = 4),
           piece_lm = ts(c(rmse_cor$prevision$prev_piece_lm$prevision, cor_prev_23_piece), end = 2023, frequency = 4),
           tvlm = ts(c(rmse_cor$prevision$prev_tvlm$prevision, cor_prev_23_tvlm), end = 2023, frequency = 4),
           ssm_cor = ts(c(ssm_oos_cor$prevision, cor_prev_23_ssm), end = 2023, frequency = 4)),
        main = "cor_previsions m2 2023 Q1") %>%
  dyOptions(colors = c("black", "orange", "green", "blue", "purple")) %>%
  dyRangeSelector(dateWindow = c("2018-01-01", "2023-01-01")) %>%
  dyLegend(width = 130)
saveRDS(plot_cor_previsions, file = "graphs_atelier/plot_cor_previsions.RDS")

rmse_ssm_cor = c(rmse_res(ssm_cor$smoothed_states[,"noise"]), rmse_res(ssm_oos_cor$oos_noise))
t(cbind(do.call(rbind, rmse_cor$rmse), rmse_ssm_cor))


## Modèle indicatrices

rmse_ind = rmse_prev(model_ind, bw = 0.294994, fixed_bw = TRUE, break_dates = 2011)

ssm_ind = ssm_lm(model_ind, var_intercept = 0.1,
                 var_variables = 0.1,
                 fixed_var_intercept = FALSE,
                 fixed_var_variables = FALSE)

ssm_oos_ind = ssm_lm_oos(model_ind, var_intercept = 0.1,
                         var_variables = 0.1,
                         fixed_var_intercept = FALSE,
                         fixed_var_variables = FALSE)

ind_last_coef_lm = last_coef(rmse_ind$model$lm)[1:3]
ind_last_coef_tvlm = last_coef(rmse_ind$model$tvlm)[1:3]
ind_last_coef_piece = last_coef(rmse_ind$model$piece_lm$model)[8:10]
ind_last_coef_ssm = tail(ssm_ind$smoothed_states,1)[1:3]

ind_prev_23_lm = sum(data_pib_23*ind_last_coef_lm)
ind_prev_23_tvlm = sum(data_pib_23*ind_last_coef_tvlm)
ind_prev_23_piece = sum(data_pib_23*ind_last_coef_piece)
ind_prev_23_ssm = sum(data_pib_23*ind_last_coef_ssm)

plot_ind_previsions = dygraph(cbind(PIB = get_data(model_ind)[,1],
                                    lm = ts(c(rmse_ind$prevision$prev_lm$prevision, ind_prev_23_lm), end = 2023, frequency = 4),
                                    piece_lm = ts(c(rmse_ind$prevision$prev_piece_lm$prevision, ind_prev_23_piece), end = 2023, frequency = 4),
                                    tvlm = ts(c(rmse_ind$prevision$prev_tvlm$prevision, ind_prev_23_tvlm), end = 2023, frequency = 4),
                                    ssm_ind = ts(c(ssm_oos_ind$prevision, ind_prev_23_ssm), end = 2023, frequency = 4)),
                              main = "ind_previsions m2 2023 Q1") %>%
  dyOptions(colors = c("black", "orange", "green", "blue", "purple")) %>%
  dyRangeSelector(dateWindow = c("2018-01-01", "2023-01-01")) %>%
  dyLegend(width = 130)
saveRDS(plot_ind_previsions, file = "graphs_atelier/plot_ind_previsions.RDS")

dygraph(cbind(get_data(model_ind)[,1], ssm_oos_ind$prevision))

rmse_ssm_ind = c(rmse_res(ssm_ind$smoothed_states[,"noise"]), rmse_res(ssm_oos_ind$oos_noise))
c(rmse_ind$rmse, rmse_ssm_ind)

resid_ind <- cbind(do.call(cbind,lapply(rmse_ind$prevision,`[[`, "residuals")),
                   ssm_oos_ind$oos_noise)
resid_ind[time(resid_ind) %in% seq(2020, by = 0.25, length.out = 4),] <- NA
apply(window(resid_ind, start = 2010, end = 2019.75), 2, rmse_res)

## Modèle complet

model <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = window(data_pib_22, start = 2000))

rmse = rmse_prev(model, fixed_bw = TRUE, break_dates = 2011)

ssm = ssm_lm(model, var_intercept = 0.1, var_variables = 0.1, fixed_var_intercept = FALSE, fixed_var_variables = FALSE)

ssm_oos = ssm_lm_oos(model, var_intercept = 0.1, var_variables = 0.1, fixed_var_intercept = FALSE, fixed_var_variables = FALSE)

last_coef_lm = last_coef(rmse$model$lm)
last_coef_tvlm = last_coef(rmse$model$tvlm)
last_coef_piece = last_coef(rmse$model$piece_lm$model)[4:6]
last_coef_ssm = tail(ssm$smoothed_states,1)[1:3]

prev_23_lm = sum(data_pib_23*last_coef_lm)
prev_23_tvlm = sum(data_pib_23*last_coef_tvlm)
prev_23_piece = sum(data_pib_23*last_coef_piece)
prev_23_ssm = sum(data_pib_23*last_coef_ssm)

plot_previsions = dygraph(cbind(PIB = get_data(model)[,1],
                                lm = ts(c(rmse$prevision$prev_lm$prevision, prev_23_lm), end = 2023, frequency = 4),
                                piece_lm = ts(c(rmse$prevision$prev_piece_lm$prevision, prev_23_piece), end = 2023, frequency = 4),
                                tvlm = ts(c(rmse$prevision$prev_tvlm$prevision, prev_23_tvlm), end = 2023, frequency = 4),
                                ssm = ts(c(ssm_oos$prevision, prev_23_ssm), end = 2023, frequency = 4)),
                          main = "Previsions m2 2023 Q1") %>%
  dyOptions(colors = c("black", "orange", "green", "blue", "purple")) %>%
  dyRangeSelector(dateWindow = c("2018-01-01", "2023-01-01")) %>%
  dyLegend(width = 130)
saveRDS(plot_previsions, file = "graphs_atelier/plot_previsions.RDS")

rmse_ssm = c(rmse_res(ssm$smoothed_states[,"noise"]), rmse_res(ssm_oos$oos_noise))
c(rmse$rmse, rmse_ssm)


