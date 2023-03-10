
ssm_lm_all = lapply(models, ssm_lm,
                    var_intercept = 0.1,
                    var_variables = 0.1,
                    fixed_intercept = FALSE,
                    fixed_variables = FALSE)

rmse_ssm_is = lapply(ssm_lm_all, function(x){
  rmse(x$smoothed_states[, ncol(x$smoothed_states)])
})

plot(cbind(ssm_lm_all$model_c5_5$data[,1], ssm_lm_all$model_c5_5$fitted[,1]),
     plot.type = "s",
     col = c("black", "red"))


ssm_lm_all_oos = lapply(names(models),
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

names(ssm_lm_all_oos) = names(models)
rmse_ssm_oos = lapply(ssm_lm_all_oos, function(x){
  rmse(x$oos_noise)
})

