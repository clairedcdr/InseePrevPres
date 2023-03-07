climat_fr <- "001565530"
pib <- "010565708"
pib <- lectureBDM(pib)
climat_fr <- lectureBDM(climat_fr)

soldes_trim <- trimestrialise(climat_fr)

pib = (pib / stats::lag(pib, -1)-1)*100

data_pib <- ts.union(pib, soldes_trim, diff(soldes_trim, 1))
colnames(data_pib) <- c("pib", sprintf("climat_fr_m%i", 1:3),
                        sprintf("diff_climat_fr_m%i", 1:3))
data_pib_2019 <- window(data_pib, start = 1990, end = c(2019,4)
)

mod1_2019 <- dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1, data = data_pib_2019)
mod2_2019 <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib_2019)
mod3_2019 <- dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3, data = data_pib_2019)

sum = summary(mod2_2019)$coefficients
x = printCoefmat(sum)
saveRDS(sum, file = "summary.RDS")

hansen.test(mod2_2019)

test = rmse_prev(mod2_2019, fixed_bw = TRUE)

piecereg = piece_reg(mod2_2019, break_dates = 2011.25)
soos_piece = soos_prev(piecereg)

test2 = ssm_lm(mod2_2019, var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

test3 = ssm_lm_oos(mod2_2019,  var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

plot_all = dygraph(cbind(PIB = get_data(mod2_2019)[,1],
              lm = test$prevision$prev_lm$prevision,
              piecelm = test$prevision$prev_piece_lm$prevision,
              tvlm = test$prevision$prev_tvlm$prevision,
              ssm = test3$prevision))%>%
  dyOptions(colors = c("black", "orange", "green", "red", "purple" ))
saveRDS(plot_all, file = "graphs_atelier/plot_all.RDS")

## Graphs

data_plot = data.frame(time(get_data(mod2_2019)),
                       get_data(mod2_2019)[,1],
                       mod2_2019$fitted.values,
                       piecereg$model$fitted.values,
                       test$model$tvlm$fitted,
                       test2$fitted[, "smoothed"])
colnames(data_plot) = c("Time", "PIB", "lm", "piecelm", "tvlm", "ssm")

plot_pib = dygraph(data_plot$PIB, main = "PIB entre 1990 et 2019", xlab = "Time") %>%
  dyOptions(colors = c("black"))
saveRDS(plot_pib, file = "graphs_atelier/plot_pib.RDS")

plot_lm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm), main = "Fitted values linear model", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange"))
saveRDS(plot_lm, file = "graphs_atelier/plot_lm.RDS")

plot_piecelm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm, piecelm = data_plot$piecelm), main = "Fitted values piecewise regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "green"))
saveRDS(plot_piecelm, file = "graphs_atelier/plot_piecelm.RDS")

plot_tvlm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm, tvlm = data_plot$tvlm), main = "Fitted values locale regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "blue"))
saveRDS(plot_tvlm, file = "graphs_atelier/plot_tvlm.RDS")

plot_ssm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm, ssm = data_plot$ssm), main = "Fitted values state space model", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "purple"))
saveRDS(plot_ssm, file = "graphs_atelier/plot_ssm.RDS")


data_plot2 = cbind(time(get_data(mod2_2019)),
                                 get_data(mod2_2019)[,1],
                                 test$prevision$prev_lm$prevision,
                                 soos_piece$prevision,
                                 test$prevision$prev_tvlm$prevision,
                                 test3$prevision)
colnames(data_plot2) = c("Time", "PIB", "lm", "piecelm", "tvlm", "ssm")

oos_lm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"]), main = "Previsions linear model", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange"))
saveRDS(oos_lm, file = "graphs_atelier/oos_lm.RDS")

oos_piecelm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"], piecelm = data_plot2[,"piecelm"]), main = "Previsions piecewise regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "green"))
saveRDS(oos_piecelm, file = "graphs_atelier/oos_piecelm.RDS")

oos_tvlm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"], tvlm = data_plot2[,"tvlm"]), main = "Previsions locale regression", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "blue"))
saveRDS(oos_tvlm, file = "graphs_atelier/oos_tvlm.RDS")

oos_ssm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"], ssm = data_plot2[,"ssm"]), main = "Previsions state space model", xlab = "Time") %>%
  dyOptions(colors = c("black", "orange", "purple"))
saveRDS(oos_ssm, file = "graphs_atelier/oos_ssm.RDS")






