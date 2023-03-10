climat_fr <- "001565530"
pib <- "010565708"
pib <- lectureBDM(pib)
climat_fr <- lectureBDM(climat_fr)

soldes_trim <- trimestrialise(climat_fr)

pib = (pib / stats::lag(pib, -1)-1)*100

data_pib <- ts.union(pib, soldes_trim, diff(soldes_trim, 1))
colnames(data_pib) <- c("pib", sprintf("climat_fr_m%i", 1:3),
                        sprintf("diff_climat_fr_m%i", 1:3))
data_pib_2019 <- window(data_pib, start = 2000, end = c(2019,4))

#mod1_2019 <- dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1, data = data_pib_2019)
mod2_2019 <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib_2019)
#mod3_2019 <- dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3, data = data_pib_2019)

sum = summary(mod2_2019)$coefficients
x = printCoefmat(sum)
saveRDS(sum, file = "summary.RDS")

hansen.test(mod2_2019)

test = rmse_prev(mod2_2019, fixed_bw = TRUE)

test2 = ssm_lm(mod2_2019, var_intercept = 0.1, var_variables = 0.1, fixed_var_intercept = FALSE, fixed_var_variables = FALSE)

test3 = ssm_lm_oos(mod2_2019,  var_intercept = 0.1, var_variables = 0.1, fixed_var_intercept = FALSE, fixed_var_variables = FALSE)

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
                       test$model$piece_lm$model$fitted.values,
                       test$model$tvlm$fitted,
                       test2$fitted[, "smoothed"])
colnames(data_plot) = c("Time", "PIB", "lm", "piecelm", "tvlm", "ssm")

plot_pib = dygraph(data_plot$PIB, main = "PIB entre 1990 et 2019") %>%
  dyOptions(colors = c("black"))%>%
  dyRangeSelector() %>%
  dySeries("V1", label = "PIB") %>%
  dyLegend(show = "always", width = 130)

plot_lm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm), main = "Valeurs ajustées modèle linéaire") %>%
  dyOptions(colors = c("black", "red"))%>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

plot_piecelm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm, piecelm = data_plot$piecelm), main = "Valeurs ajustées régression par morceaux") %>%
  dyOptions(colors = c("black", "red", "green"))%>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

plot_tvlm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm, tvlm = data_plot$tvlm), main = "Valeurs ajustées régression locale") %>%
  dyOptions(colors = c("black", "red", "blue"))%>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

plot_ssm = dygraph(cbind(PIB = data_plot$PIB, lm = data_plot$lm, ssm = data_plot$ssm), main = "Valeurs ajustées modèle espace-état") %>%
  dyOptions(colors = c("black", "red", "purple"))%>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

saveRDS(plot_pib, file = "graphs_atelier/plot_pib.RDS")
saveRDS(plot_lm, file = "graphs_atelier/plot_lm.RDS")
saveRDS(plot_piecelm, file = "graphs_atelier/plot_piecelm.RDS")
saveRDS(plot_tvlm, file = "graphs_atelier/plot_tvlm.RDS")
saveRDS(plot_ssm, file = "graphs_atelier/plot_ssm.RDS")

data_plot2 = cbind(time(get_data(mod2_2019)),
                   get_data(mod2_2019)[,1],
                   test$prevision$prev_lm$prevision,
                   test$prevision$prev_piece_lm$prevision,
                   test$prevision$prev_tvlm$prevision,
                   test3$prevision)
colnames(data_plot2) = c("Time", "PIB", "lm", "piecelm", "tvlm", "ssm")

oos_lm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"]), main = "Estimations en temps réel modèle linéaire") %>%
  dyOptions(colors = c("black", "red"))%>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

oos_piecelm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"], piecelm = data_plot2[,"piecelm"]), main = "Estimations en temps réel régression par morceaux") %>%
  dyOptions(colors = c("black", "red", "green"))%>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

oos_tvlm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"], tvlm = data_plot2[,"tvlm"]), main = "Estimations en temps réel régression locale") %>%
  dyOptions(colors = c("black", "red", "blue"))%>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

oos_ssm = dygraph(cbind(PIB = data_plot2[,"PIB"], lm = data_plot2[,"lm"], ssm = data_plot2[,"ssm"]), main = "Prévisions en temps réel modèle espace-état") %>%
  dyOptions(colors = c("black", "red", "purple")) %>%
  dyRangeSelector() %>%
  dyLegend(width = 130)

saveRDS(oos_lm, file = "graphs_atelier/oos_lm.RDS")
saveRDS(oos_piecelm, file = "graphs_atelier/oos_piecelm.RDS")
saveRDS(oos_tvlm, file = "graphs_atelier/oos_tvlm.RDS")
saveRDS(oos_ssm, file = "graphs_atelier/oos_ssm.RDS")


coef_tvlm = ts(test$model$tvlm$coefficients, end = 2019.75, frequency = 4)
coef_ssm = test2$smoothed_states[, c(1:3)]
coef_lm = ts(cbind(rep(test$model$lm$coefficients[1], 80), test$model$lm$coefficients[2], test$model$lm$coefficients[3]), end = 2019.75, frequency = 4)
colnames(coef_lm) = colnames(coef_tvlm)

coef_piece = test$model$piece_lm$model$coefficients
coef_piece_cste = coef_piece[c(1, 4)]
coef_piece_clim = coef_piece[c(2, 5)]
coef_piece_diff = coef_piece[c(3, 6)]
coef_piece = ts(cbind(c(rep(coef_piece_cste[1], 45),
           rep(coef_piece_cste[2], 35)),
         c(rep(coef_piece_clim[1], 45),
           rep(coef_piece_clim[2], 35)),
         c(rep(coef_piece_diff[1], 45),
           rep(coef_piece_diff[2], 35))),
         end = 2019.75, frequency = 4)
colnames(coef_piece) = colnames(coef_tvlm)

coef_intercept = cbind(lineaire = coef_lm[,1], morceaux = coef_piece[,1], locale = coef_tvlm[,1], espace_etat = coef_ssm[,1])
coef_climat = cbind(lineaire = coef_lm[,2], morceaux = coef_piece[,2], locale = coef_tvlm[,2], espace_etat = coef_ssm[,2])
coef_diff = cbind(lineaire = coef_lm[,3], morceaux = coef_piece[,3], locale = coef_tvlm[,3], espace_etat = coef_ssm[,3])

plot_intercept = dygraph(coef_intercept, main = "Coefficients intercept") %>%
  dyLegend(width = 300) %>%
  dyRangeSelector() %>%
  dyAxis("y", valueRange = c(-3.5, 0)) %>%
  dyOptions(colors = c("red", "green", "blue", "purple"))
saveRDS(plot_intercept, file = "graphs_atelier/plot_intercept.RDS")

plot_climat = dygraph(coef_climat, main = "Coefficients climat") %>%
  dyLegend(width = 300) %>%
  dyRangeSelector() %>%
  dyAxis("y", valueRange = c(0,0.06)) %>%
  dyOptions(colors = c("red", "green", "blue", "purple"))
saveRDS(plot_climat, file = "graphs_atelier/plot_climat.RDS")

plot_diff = dygraph(coef_diff, main = "Coefficients diff_climat") %>%
  dyLegend(width = 300) %>%
  dyRangeSelector() %>%
  dyAxis("y", valueRange = c(-0.05, 0.15)) %>%
  dyOptions(colors = c("red", "green", "blue", "purple"))
saveRDS(plot_diff, file = "graphs_atelier/plot_diff.RDS")


plot_coef_tvlm = dygraph(coef_tvlm, main = "Coefficients régression locale") %>%
  dyLegend(width = 300) %>%
  dySeries("(Intercept)", label = "cste") %>%
  dySeries("climat_fr_m2", label = "climat") %>%
  dySeries("diff_climat_fr_m2", label = "diff_climat") %>%
  dyRangeSelector() %>%
  dyAxis("y", valueRange = c(-3, 1))
saveRDS(plot_coef_tvlm, file = "graphs_atelier/plot_coef_tvlm.RDS")

# plot_coef_ssm = dygraph(coef_ssm, main = "Coefficients modèle espace-état") %>%
#   dyLegend(width = 110) %>%
#   dySeries("(Intercept)", label = "cste") %>%
#   dySeries("climat_fr_m2", label = "climat") %>%
#   dySeries("diff_climat_fr_m2", label = "diff_climat") %>%
#   dyRangeSelector()
# saveRDS(plot_coef_ssm, file = "graphs_atelier/plot_coef_ssm.RDS")
#
# dygraph(coef_tvlm[,-1], main = "Coefficients régression locale") %>%
#   dyLegend(width = 110) %>%
#   dySeries("climat_fr_m2", label = "climat") %>%
#   dySeries("diff_climat_fr_m2", label = "diff_climat") %>%
#   dyRangeSelector()
#
#
# dygraph(coef_ssm[,-1], main = "Coefficients régression locale") %>%
#   dyLegend(width = 110) %>%
#   dySeries("climat_fr_m2", label = "climat") %>%
#   dySeries("diff_climat_fr_m2", label = "diff_climat") %>%
#   dyRangeSelector()
