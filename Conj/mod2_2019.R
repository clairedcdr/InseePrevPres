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

test = rmse_prev(mod2_2019, fixed_bw = TRUE)

test2 = ssm_lm(mod2_2019, var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

dygraph(cbind(PIB = get_data(mod2_2019)[,1],
              lm = test$model$lm$fitted.values,
              piecelm = test$model$piece_lm$model$fitted.values,
              tvlm = test$model$tvlm$fitted,
              ssm = test2$fitted[,"smoothed"]))%>%
  dyOptions(colors = c("black", "orange", "green", "red", "purple" ))

dygraph(cbind(PIB = get_data(mod2_2019)[,1],
              ssm = test2$fitted[,"smoothed"]))%>%
  dyOptions(colors = c("black", "purple" ))

test3 = ssm_lm_oos(mod2_2019,  var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

dygraph(cbind(PIB = get_data(mod2_2019)[,1],
              ssm = test3$prevision))%>%
  dyOptions(colors = c("black", "purple" ))

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

# plot_pib = ggplot(data_plot, aes(Time, PIB)) +
#   geom_line() +
#   theme_minimal()
# ggsave("graphs_atelier/plot_pib.svg", plot = plot_pib, height = 20, width = 40, units = "cm")
#
# plot_lm = ggplot(data_plot, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   theme_minimal()
# ggsave("graphs_atelier/plot_lm.svg", plot = plot_lm, height = 20, width = 40, units = "cm")
#
# plot_piecelm = ggplot(data_plot, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   geom_line(aes(,piecelm), color = "green") +
#   theme_minimal()
# ggsave("graphs_atelier/plot_piecelm.svg", plot = plot_piecelm, height = 20, width = 40, units = "cm")
#
# plot_tvlm = ggplot(data_plot, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   geom_line(aes(,tvlm), color = "red") +
#   theme_minimal()
# ggsave("graphs_atelier/plot_tvlm.svg", plot = plot_tvlm, height = 20, width = 40, units = "cm")
#
# plot_ssm = ggplot(data_plot, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   geom_line(aes(,ssm), color = "purple") +
#   theme_minimal()
# ggsave("graphs_atelier/plot_ssm.svg", plot = plot_ssm, height = 20, width = 40, units = "cm")


data_plot2 = cbind(time(get_data(mod2_2019)),
                                 get_data(mod2_2019)[,1],
                                 test$prevision$prev_lm$prevision,
                                 test$prevision$prev_piece_lm$prevision,
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

# oos_lm = ggplot(data_plot2, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   theme_minimal()
# ggsave("graphs_atelier/oos_lm.svg", plot = oos_lm, height = 20, width = 40, units = "cm")
#
# oos_piecelm = ggplot(data_plot2, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   geom_line(aes(,piecelm), color = "green") +
#   theme_minimal()
# ggsave("graphs_atelier/oos_piecelm.svg", plot = oos_piecelm, height = 20, width = 40, units = "cm")
#
# oos_tvlm = ggplot(data_plot2, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   geom_line(aes(,tvlm), color = "red") +
#   theme_minimal()
# ggsave("graphs_atelier/oos_tvlm.svg", plot = oos_tvlm, height = 20, width = 40, units = "cm")
#
# oos_ssm = ggplot(data_plot2, aes(Time, PIB)) +
#   geom_line() +
#   geom_line(aes(,lm), color = "orange") +
#   geom_line(aes(,ssm), color = "purple") +
#   theme_minimal()
# ggsave("graphs_atelier/oos_ssm.svg", plot = oos_ssm, height = 20, width = 40, units = "cm")




