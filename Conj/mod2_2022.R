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
                        data = data3)

data3 = cbind(pib_cor = data3[,"pib"] - rowSums(ind * coef(model_ind)[4:7]),
              data3)
colnames(data3) = c("pib_cor", colnames(data_pib_22), sprintf("ind2020Q%i", 1:4))

model_cor <- dynlm::dynlm(pib_cor ~ climat_fr_m2 + diff_climat_fr_m2,
                         data = data3)

rmse = rmse_prev(model_cor, fixed_bw = TRUE, break_dates = 2011.25)

ssm = ssm_lm(model_cor, var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

ssm_oos = ssm_lm_oos(model_cor, var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

last_coef_lm = last_coef(rmse$model$lm)
last_coef_tvlm = last_coef(rmse$model$tvlm)
last_coef_piece = last_coef(rmse$model$piece_lm$model)[4:6]
last_coef_ssm = tail(ssm$smoothed_states,1)[1:3]

prev_23_lm = sum(data_pib_23*last_coef_lm)
prev_23_tvlm = sum(data_pib_23*last_coef_tvlm)
prev_23_piece = sum(data_pib_23*last_coef_piece)
prev_23_ssm = sum(data_pib_23*last_coef_ssm)

plot_previsions = dygraph(cbind(PIB = get_data(model_cor)[,1],
           lm = ts(c(rmse$prevision$prev_lm$prevision, prev_23_lm), end = 2023, frequency = 4),
           piece_lm = ts(c(rmse$prevision$prev_piece_lm$prevision, prev_23_piece), end = 2023, frequency = 4),
           tvlm = ts(c(rmse$prevision$prev_tvlm$prevision, prev_23_tvlm), end = 2023, frequency = 4),
           ssm = ts(c(ssm_oos$prevision, prev_23_ssm), end = 2023, frequency = 4)),
        main = "Previsions m2 2023 Q1") %>%
  dyOptions(colors = c("black", "orange", "green", "blue", "purple")) %>%
  dyRangeSelector(dateWindow = c("2018-01-01", "2023-01-01")) %>%
  dyLegend(width = 130)
saveRDS(plot_previsions, file = "graphs_atelier/plot_previsions.RDS")


