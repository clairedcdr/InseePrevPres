mod3
test = rmse_prev(mod3, fixed_bw = TRUE)
dygraph(cbind(data2[,"pib"],
              test$model$lm$fitted.values,
              test$model$piece_lm$model$fitted.values,
              test$model$tvlm$fitted))
test2 = ssm_lm(mod3,  var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)
dygraph(cbind(PIB = data2[,"pib"],
              lm = test$model$lm$fitted.values,
              piecelm = test$model$piece_lm$model$fitted.values,
              tvlm = test$model$tvlm$fitted,
              ssm = test2$fitted[,"smoothed"]))%>%
  dyOptions(colors = c("black", "orange", "green", "red", "purple" ))

dygraph(cbind(PIB = data2[,"pib"],
              ssm = test2$fitted[,"smoothed"]))%>%
  dyOptions(colors = c("black", "purple" ))

test3 = ssm_lm_oos(mod3,  var_intercept = 0.1, var_variables = 0.1, fixed_intercept = FALSE, fixed_variables = FALSE)

dygraph(cbind(PIB = data2[,"pib"],
              ssm = test3$prevision))%>%
  dyOptions(colors = c("black", "purple" ))

dygraph(cbind(PIB = data2[,"pib"],
              lm = test$prevision$prev_lm$prevision,
              piecelm = test$prevision$prev_piece_lm$prevision,
              tvlm = test$prevision$prev_tvlm$prevision,
              ssm = test3$prevision))%>%
  dyOptions(colors = c("black", "orange", "green", "red", "purple" ))


## Graphs

data_plot = data.frame(time(data2),
                  data2[, "pib"],
                  mod3$fitted.values,
                  test$model$piece_lm$model$fitted.values,
                  test$model$tvlm$fitted,
                  test2$fitted[, "smoothed"])
colnames(data_plot) = c("Time", "PIB", "lm", "piecelm", "tvlm", "ssm")

plot_pib = ggplot(data_plot, aes(Time, PIB)) +
  geom_line() +
theme_minimal()
ggsave("graphs_atelier/plot_pib.svg", plot = plot_pib, height = 20, width = 40, units = "cm")

plot_lm = ggplot(data_plot, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  theme_minimal()
ggsave("graphs_atelier/plot_lm.svg", plot = plot_lm, height = 20, width = 40, units = "cm")

plot_piecelm = ggplot(data_plot, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  geom_line(aes(,piecelm), color = "green") +
  theme_minimal()
ggsave("graphs_atelier/plot_piecelm.svg", plot = plot_piecelm, height = 20, width = 40, units = "cm")

plot_tvlm = ggplot(data_plot, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  geom_line(aes(,tvlm), color = "red") +
  theme_minimal()
ggsave("graphs_atelier/plot_tvlm.svg", plot = plot_tvlm, height = 20, width = 40, units = "cm")

plot_ssm = ggplot(data_plot, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  geom_line(aes(,ssm), color = "purple") +
  theme_minimal()
ggsave("graphs_atelier/plot_ssm.svg", plot = plot_ssm, height = 20, width = 40, units = "cm")


data_plot2 = as.data.frame(cbind(time(data2),
                       data2[, "pib"],
                       test$prevision$prev_lm$prevision,
                       test$prevision$prev_piece_lm$prevision,
                       test$prevision$prev_tvlm$prevision,
                       test3$prevision))
colnames(data_plot2) = c("Time", "PIB", "lm", "piecelm", "tvlm", "ssm")

oos_lm = ggplot(data_plot2, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  theme_minimal()
ggsave("graphs_atelier/oos_lm.svg", plot = oos_lm, height = 20, width = 40, units = "cm")

oos_piecelm = ggplot(data_plot2, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  geom_line(aes(,piecelm), color = "green") +
  theme_minimal()
ggsave("graphs_atelier/oos_piecelm.svg", plot = oos_piecelm, height = 20, width = 40, units = "cm")

oos_tvlm = ggplot(data_plot2, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  geom_line(aes(,tvlm), color = "red") +
  theme_minimal()
ggsave("graphs_atelier/oos_tvlm.svg", plot = oos_tvlm, height = 20, width = 40, units = "cm")

oos_ssm = ggplot(data_plot2, aes(Time, PIB)) +
  geom_line() +
  geom_line(aes(,lm), color = "orange") +
  geom_line(aes(,ssm), color = "purple") +
  theme_minimal()
ggsave("graphs_atelier/oos_ssm.svg", plot = oos_ssm, height = 20, width = 40, units = "cm")


plot(cbind(data_plot$PIB, test$prevision$prev_piece_tvlm$prevision), plot.type = "s", col = c("black", "red"))
