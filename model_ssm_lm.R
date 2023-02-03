
ssm_lm_all = lapply(models, ssm_lm,
                    var_intercept = 0.1,
                    var_variables = 0.1,
                    fixed_intercept = FALSE,
                    fixed_variables = FALSE)

rmse_ssm_is = lapply(ssm_lm_all, function(x){
  rmse_res(x$smoothed_states[, ncol(x$smoothed_states)])
})

plot(cbind(ssm_lm_all$model_c5_6$data[,1], ssm_lm_all$model_c5_6$fitted[,1]),
     plot.type = "s",
     col = c("black", "red"))


ssm_lm_all_oos = lapply(names(models)[-c(1:26)],
                         function(x, ...) {
                           print(x)
                           tryCatch(ssm_lm_oos(models[[x]], var_intercept = 0.1,
                                               fixed_intercept = FALSE,
                                               var_variables = 0.1,
                                               fixed_variables = FALSE, ...),
                                    error = function(e){
                           tryCatch(ssm_lm_oos(models[[x]], var_intercept = 0.01,
                                               fixed_intercept = FALSE,
                                               var_variables = 0.1,
                                               fixed_variables = FALSE, ...),
                                    error = function(e) {
                                      tryCatch(ssm_lm_oos(models[[x]], var_intercept = 0,
                                                 fixed_intercept = FALSE,
                                                 var_variables = 0.001,
                                                 fixed_variables = FALSE, ...),
                                               error = function(e) {
                                                 tryCatch(ssm_lm_oos(models[[x]], var_intercept = 0,
                                                            fixed_intercept = TRUE,
                                                            var_variables = 100,
                                                            fixed_variables = FALSE, ...), error = function(e){
                                                              ssm_lm_oos(models[[x]], var_intercept = 0.1,
                                                                         fixed_intercept = FALSE,
                                                                         var_variables = 0,
                                                                         fixed_variables = TRUE)
                                                            })
                                               })
                                    })
                           })
                           })
tmp2 = ssm_lm_oos(models$model_manuf_1,
                  var_intercept = 0.1,
                  fixed_intercept = FALSE,
                  var_variables = 0,
                  fixed_variables = TRUE)
mean(tmp1[,"noise"]^2)
mean(tmp2[,"noise"]^2)
data

x = ssm_lm(model_c5_6, var_intercept = 0.1,
               var_variables = 0.1,
               fixed_intercept = FALSE,
               fixed_variables = FALSE)
x_oos = ssm_lm_oos(model_c5_6, var_intercept = 0.1,
               var_variables = 0.1,
               fixed_intercept = FALSE,
               fixed_variables = FALSE)



