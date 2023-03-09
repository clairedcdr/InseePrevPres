


model_manuf_1 = dynlm( prod_manuf ~ ind2008Q4 + ind2009Q1  + acquis_ipi0 + diff(ins_tppa_m2,1) ,
                       data = data
)

model_manuf_2 <- dynlm(prod_manuf ~ ind2008Q4 + ind2009Q1  + acquis_ipi0 + bdf_prodpre_m1+diff(bdf_sitcar_m1,1),
                       data = data
)



model_manuf_3 <- dynlm(prod_manuf ~ +ind1997Q2 + ind2008Q4 + ind2009Q1 + ind2011Q1
                       + ind2012Q3 + ind2013Q3
                       + diff(ins_tppre_m2,1) + ins_tppa_m2 +stats::lag(ins_tppre_m3,-1) + stats::lag(bdf_prodpre_m3,-1),
                       data = data
)


model_c1_1<- dynlm(
  formula = prod_c1 ~ins_climat_c1_m1+ins_tppa_c1_m1+diff(ins_oscd_c1_m1,1),
  data = data
)

model_c1_2<- dynlm(
  formula = prod_c1 ~acquis_ipi1_c1+bdf_prodpre_c1_m2 + diff(ins_tppre_c1_m3,1)+
    diff(bdf_sitcar_c1_m2,1),
  data = data
)

model_c1_3 <- dynlm(
  formula = prod_c1 ~acquis_ipi1_c1+bdf_prodpas_c1_m2,
  data = data
)


# model_c1_4<- dynlm(
#   formula = prod_c1 ~ins_climat_c1_m1+ins_tppa_c1_m1+diff(ins_oscd_c1_m1,1),
#   data = data
# )


model_c1_4<- dynlm(
  formula = prod_c1 ~stats::lag(prod_c1,-1)+acquis_ipi0_c1+
    bdf_evocar_c1_m1+diff(ins_climat_c1_m2,1),
  data = data
)

model_c1_5<- dynlm(
  formula = prod_c1 ~acquis_ipi1_c1+bdf_prodpre_c1_m2+diff(ins_tppre_c1_m3,1)+diff(ins_climat_c1_m3,1),
  data = data
)

model_c1_6<- dynlm(
  formula = prod_c1 ~acquis_ipi1_c1+diff(ins_climat_c1_m3,1),
  data = data
)

model_c1_7<- dynlm(
  formula = prod_c1 ~acquis_ipi1_c1,
  data = data
)



model_c3_1<- dynlm(
  formula = prod_c3 ~acquis_ipi1_c3+bdf_evocar_c3_m2,
  data = data
)

model_c3_2<- dynlm(
  formula = prod_c3 ~ins_climat_c3_m1+ins_tppre_c3_m1+diff(ins_tppre_c3_m1,1)+ind2009Q1,
  data = data
)


model_c3_3 <- dynlm(
  formula = prod_c3 ~acquis_ipi1_c3+ins_tppa_c3_m2+ins_tppre_c3_m2+bdf_evocar_c3_m2,
  data = data
)


# model_c3_4<- dynlm(
#   formula = prod_c3 ~ins_climat_c3_m1+ins_tppre_c3_m1+diff(ins_tppre_c3_m1,1)+ind2009Q1,
#   data = data
# )

model_c3_4<- dynlm(
  formula = prod_c3 ~ins_tppre_c3_m1+ins_tppa_c3_m1+diff(ins_climat_c3_m2,1)+ind2009Q1,
  data = data
)

model_c3_5<- dynlm(
  formula = prod_c3 ~acquis_ipi1_c3+ins_climat_c3_m3+diff(ins_tppre_c3_m3,1),
  data = data
)


model_c4_1<- dynlm(
  formula = prod_c4 ~diff(ins_climat_c4_m1)+ins_tppre_c4_m1+stats::lag(ins_oscd_c4_m2,-1)+ind2008Q4,
  data = data
)
model_c4_2<- dynlm(
  formula = prod_c4 ~ acquis_ipi1_c4 + bdf_prodpas_c4_m2+diff(ins_oscd_c4_m3,1),
  data = data
)

model_c4_3 <- dynlm(
  formula = prod_c4 ~ acquis_ipi1_c4+ins_tppa_c4_m2 + bdf_prodpas_c4_m2+diff(ins_oscd_c4_m2,1),
  data = data
)


# model_c4_4<- dynlm(
#   formula = prod_c4 ~diff(ins_climat_c4_m1)+ins_tppre_c4_m1+stats::lag(ins_oscd_c4_m2,-1)+ind2008Q4,
#   data = data
# )


model_c4_4<- dynlm(
  formula = prod_c4 ~acquis_ipi0_c4+diff(ins_climat_c4_m2,1)+bdf_prodpas_c4_m1+diff(bdf_tuc_c4_m1,1),
  data = data
)

model_c4_5<- dynlm(
  formula = prod_c4 ~acquis_ipi1_c4+diff(ins_climat_c4_m3,1)+bdf_prodpas_c4_m2+ind2008Q4,
  data = data
)



model_c5_1<- dynlm(
  formula = prod_c5 ~ins_climat_c5_m1+diff(ins_tppre_c5_m1,1)+
    ind2008Q4+ind2009Q1,
  data = data
)

model_c5_2<- dynlm(
  formula = prod_c5 ~ acquis_ipi1_c5+bdf_prodpas_c5_m2+stats::lag(ins_oscd_c5_m3,-1)+
    diff(bdf_sitcar_c5_m2,1),
  data = data)

model_c5_3 <- dynlm(
  formula = prod_c5 ~ acquis_ipi1_c5+bdf_prodpas_c5_m2+
    diff(bdf_evocar_c5_m2,1)+diff(ins_tppre_c5_m2,1),
  data = data
)


# model_c5_4<- dynlm(
#   formula = prod_c5 ~ins_climat_c5_m1+diff(ins_tppre_c5_m1,1)+
#     ind2008Q4+ind2009Q1,
#   data = data
# )


model_c5_4<- dynlm(
  formula = prod_c5 ~stats::lag(prod_c5,-1)+bdf_prodpre_c5_m1
  +diff(ins_climat_c5_m2,1)+diff(bdf_tuc_c5_m1,1)+ind2008Q4+ind2009Q1,
  data = data
)

model_c5_5<- dynlm(
  formula = prod_c5 ~acquis_ipi1_c5+ins_climat_c5_m3+ins_oscd_c5_m2
  +diff(ins_tppre_c5_m3,1)+diff(bdf_tuc_c5_m2,1),
  data = data
)




