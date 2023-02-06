
# Comparaisons rmse_prev in sample ---------------------

rmse_is = sapply(test, function(x) {
  x$rmse[1]
})

rmse_is <- lapply(rmse_is, function(x) {
  c(x, rep(NA, max(lengths(rmse_is)) - length(x)))
})
rmse_is <- do.call(rbind, rmse_is)
colnames(rmse_is) = c(colnames(rmse_is)[1:4], "piece_lm fixed coeff", "piece_tvlm fixed coeff", "TvLM fixed coeff")


# Comparaisons rmse_prev out of sample  ------------------------

rmse_oos = sapply(test, function(x) {
  x$rmse[2]
})

rmse_oos <- lapply(rmse_oos, function(x) {
  c(x, rep(NA, max(lengths(rmse_oos)) - length(x)))
})
rmse_oos <- do.call(rbind, rmse_oos)
colnames(rmse_oos) = c(colnames(rmse_oos)[1:4], "piece_lm fixed coeff", "piece_tvlm fixed coeff", "TvLM fixed coeff")


# False oos tvlm ---------

tvlm = lapply(test_fixed, function(x) {
  x$model$tvlm
})
false_oos_tvlm = lapply(tvlm, rmse_false_oos_tvlm)

comp_tvlm = data.frame(cbind(oos_tvlm = rmse_oos[,"TvLM"], false_oos_tvlm, diff_1_2 = unlist(comp_tvlm[,1]) - unlist(comp_tvlm[,2])))


# Stats agrégées in sample ------------

rmse_is_all = data.frame(cbind(ssm_lm = rmse_ssm_is, rmse_is))
rmse_is_all = apply(rmse_is_all, 2, unlist)

rmse_is_c1 = rmse_is_all[rownames(rmse_is_all) %in% ls(pattern = "model_c1_"),]
rmse_is_c3 = rmse_is_all[rownames(rmse_is_all) %in% ls(pattern = "model_c3_"),]
rmse_is_c4 = rmse_is_all[rownames(rmse_is_all) %in% ls(pattern = "model_c4_"),]
rmse_is_c5 = rmse_is_all[rownames(rmse_is_all) %in% ls(pattern = "model_c5_"),]
rmse_is_manuf  = rmse_is_all[rownames(rmse_is_all) %in% ls(pattern = "model_manuf_"),]


mean_is_c1 = apply(apply(rmse_is_c1, 2, `/`, rmse_is_c1[,"lm"]), 2, mean, na.rm = TRUE)
mean_is_c3 = apply(apply(rmse_is_c3, 2, `/`, rmse_is_c3[,"lm"]), 2, mean, na.rm = TRUE)
mean_is_c4 = apply(apply(rmse_is_c4, 2, `/`, rmse_is_c4[,"lm"]), 2, mean, na.rm = TRUE)
mean_is_c5 = apply(apply(rmse_is_c5, 2, `/`, rmse_is_c5[,"lm"]), 2, mean, na.rm = TRUE)
mean_is_manuf = apply(apply(rmse_is_manuf, 2, `/`, rmse_is_manuf[,"lm"]), 2, mean, na.rm = TRUE)

stat_agr_is = rbind(mean_is_c1, mean_is_c3, mean_is_c4, mean_is_c5, mean_is_manuf)


# Stats agrégées out of sample ------------

rmse_oos_all = data.frame(cbind(ssm_lm = rmse_ssm_oos, rmse_oos, false_oos_tvlm))
rmse_oos_all = apply(rmse_oos_all, 2, unlist)

rmse_oos_c1 = rmse_oos_all[rownames(rmse_oos_all) %in% ls(pattern = "model_c1_"),]
rmse_oos_c3 = rmse_oos_all[rownames(rmse_oos_all) %in% ls(pattern = "model_c3_"),]
rmse_oos_c4 = rmse_oos_all[rownames(rmse_oos_all) %in% ls(pattern = "model_c4_"),]
rmse_oos_c5 = rmse_oos_all[rownames(rmse_oos_all) %in% ls(pattern = "model_c5_"),]
rmse_oos_manuf  = rmse_oos_all[rownames(rmse_oos_all) %in% ls(pattern = "model_manuf_"),]


mean_oos_c1 = apply(apply(rmse_oos_c1, 2, `/`, rmse_oos_c1[,"lm"]), 2, mean, na.rm = TRUE)
mean_oos_c3 = apply(apply(rmse_oos_c3, 2, `/`, rmse_oos_c3[,"lm"]), 2, mean, na.rm = TRUE)
mean_oos_c4 = apply(apply(rmse_oos_c4, 2, `/`, rmse_oos_c4[,"lm"]), 2, mean, na.rm = TRUE)
mean_oos_c5 = apply(apply(rmse_oos_c5, 2, `/`, rmse_oos_c5[,"lm"]), 2, mean, na.rm = TRUE)
mean_oos_manuf = apply(apply(rmse_oos_manuf, 2, `/`, rmse_oos_manuf[,"lm"]), 2, mean, na.rm = TRUE)

stat_agr_oos = rbind(mean_oos_c1, mean_oos_c3, mean_oos_c4, mean_oos_c5, mean_oos_manuf)

