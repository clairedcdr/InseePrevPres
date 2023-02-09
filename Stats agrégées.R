# RMSE bw non fixe ---------------

## In sample ---------------------

rmse_is = sapply(test, function(x) {
  x$rmse[1]
})

rmse_is <- lapply(rmse_is, function(x) {
  c(x, rep(NA, max(lengths(rmse_is)) - length(x)))
})
rmse_is <- do.call(rbind, rmse_is)
colnames(rmse_is) = c(colnames(rmse_is)[1:4], "piece_lm fixed coeff", "piece_tvlm fixed coeff", "TvLM fixed coeff")


## Out of sample  ------------------------

rmse_oos = sapply(test, function(x) {
  x$rmse[2]
})

rmse_oos <- lapply(rmse_oos, function(x) {
  c(x, rep(NA, max(lengths(rmse_oos)) - length(x)))
})
rmse_oos <- do.call(rbind, rmse_oos)
colnames(rmse_oos) = c(colnames(rmse_oos)[1:4], "piece_lm fixed coeff", "piece_tvlm fixed coeff", "TvLM fixed coeff")

# RMSE bw fixe -------------

## In sample ---------------------

rmse_is_fixed = sapply(test_fixed, function(x) {
  x$rmse[1]
})

rmse_is_fixed <- lapply(rmse_is_fixed, function(x) {
  c(x, rep(NA, max(lengths(rmse_is_fixed)) - length(x)))
})
rmse_is_fixed <- do.call(rbind, rmse_is_fixed)
colnames(rmse_is_fixed) = c(colnames(rmse_is_fixed)[1:4], "piece_lm fixed coeff", "piece_tvlm fixed coeff", "TvLM fixed coeff")


## Out of sample  ------------------------

rmse_oos_fixed = sapply(test_fixed, function(x) {
  x$rmse[2]
})

rmse_oos_fixed <- lapply(rmse_oos_fixed, function(x) {
  c(x, rep(NA, max(lengths(rmse_oos_fixed)) - length(x)))
})
rmse_oos_fixed <- do.call(rbind, rmse_oos_fixed)
colnames(rmse_oos_fixed) = c(colnames(rmse_oos_fixed)[1:4], "piece_lm fixed coeff", "piece_tvlm fixed coeff", "TvLM fixed coeff")


# False oos tvlm ---------

tvlm = lapply(test, function(x) {
  x$model$tvlm
})
false_oos_tvlm = lapply(tvlm, rmse_false_oos_tvlm)

comp_tvlm = data.frame(cbind(oos_tvlm = rmse_oos[,"TvLM"], false_oos_tvlm, diff_1_2 = unlist(comp_tvlm[,1]) - unlist(comp_tvlm[,2])))


# Stats agrégées ---------------

## Stats agrégées in sample ------------

rmse_is_all = data.frame(cbind(lm = rmse_is[,1],
                               ssm_lm = rmse_ssm_is,
                               rmse_is[,c(2:7)]))
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


## Stats agrégées out of sample ------------

rmse_oos_all = data.frame(cbind("lm" = rmse_oos[,1],
                               "ssm_lm" = rmse_ssm_oos,
                               rmse_oos[,c(2,3)],
                               "piece_tvlm_fixed" = rmse_oos_fixed[,3],
                               "Tvlm" = rmse_oos[,4],
                               "Tvlm_fixed" = rmse_oos_fixed[,4],
                               false_oos_tvlm,
                               rmse_oos[,c(5,6)],
                               "piece_tvlm_fixed fixed coeff" = rmse_oos_fixed[,6],
                               "Tvlm fixed coeff" = rmse_oos[,7],
                               "Tvlm_fixed fixed coeff" = rmse_oos_fixed[,7]))
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



