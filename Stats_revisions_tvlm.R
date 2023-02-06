
test_variant_fixed = lapply(seq_along(model_variant), function(i) {
  rmse_prev(formula = get_formula_variants[[i]],
            data = data,
            var_fixes = fixed_coef_variants[[i]],
            date = 28,
            fixed_bw = TRUE)
})
names(test_variant_fixed) = names(model_variant)
test_invariant_fixed = lapply(lapply(model_invariant, get_formula), rmse_prev, data = data, date = 28, fixed_bw = TRUE)

test_fixed = c(test_invariant_fixed[1:9],
               test_variant_fixed[1],
               test_invariant_fixed[10],
               test_variant_fixed[2:3],
               test_invariant_fixed[11:17],
               test_variant_fixed[4:9],
               test_invariant_fixed[18:19],
               test_variant_fixed[10])

years = window(time(data), end = 2015)

indicateurs_revisions_tvlm = sapply(test_fixed, get_indicateurs_all, years = years)

# graphs boite à moustache
