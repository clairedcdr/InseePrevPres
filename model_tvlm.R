
tvlm_c1_1 = tvLM(formula = prod_c1 ~ ., data = model_c1_1$model)
tvlm_c1_2 = tvLM(formula = prod_c1 ~ ., data = model_c1_2$model)
tvlm_c1_3 = tvLM(formula = prod_c1 ~ ., data = model_c1_3$model)
tvlm_c1_4 = tvLM(formula = prod_c1 ~ ., data = model_c1_4$model)
tvlm_c1_5 = tvLM(formula = prod_c1 ~ ., data = model_c1_5$model)
tvlm_c1_6 = tvLM(formula = prod_c1 ~ ., data = model_c1_6$model)
tvlm_c1_7 = tvLM(formula = prod_c1 ~ ., data = model_c1_7$model)
tvlm_c1_8 = tvLM(formula = prod_c1 ~ ., data = model_c1_8$model)
tvlm_c3_1 = tvLM(prod_c3 ~., data = model_c3_1$model)
tvlm_c3_3 = tvLM(prod_c3 ~., data = model_c3_3$model)
tvlm_c3_6 = tvLM(prod_c3 ~., data = model_c3_6$model)
tvlm_c4_1 = tvLM(prod_c4 ~., data = model_c4_1$model)
tvlm_c4_2 = tvLM(prod_c4 ~., data = model_c4_2$model)
tvlm_c4_3 = tvLM(prod_c4 ~., data = model_c4_3$model)
tvlm_c4_4 = tvLM(prod_c4 ~., data = model_c4_4$model)
tvlm_c4_5 = tvLM(prod_c4 ~., data = model_c4_5$model)
tvlm_c4_6 = tvLM(prod_c4 ~., data = model_c4_6$model)
tvlm_manuf_1 = tvLM(prod_manuf ~., data = model_manuf_1$model)
tvlm_manuf_2 = tvLM(prod_manuf ~., data = model_manuf_2$model)

formule = sapply(seq_along(model_variant), function(i){
  paste(colnames(model_variant[[i]]$model)[1], "~.")
})
tv = lapply(seq_along(model_variant), function(i) {
  tvLM(formula = as.formula(formule[[i]]), data = model_variant[[i]]$model)
})

tvlm_c3_2 = tv[[1]]
tvlm_c3_4 = tv[[2]]
tvlm_c3_5 = tv[[3]]
tvlm_c5_1 = tv[[4]]
tvlm_c5_2 = tv[[5]]
tvlm_c5_3 = tv[[6]]
tvlm_c5_4 = tv[[7]]
tvlm_c5_5 = tv[[8]]
tvlm_c5_6 = tv[[9]]
tvlm_manuf_3 = tv[[10]]
