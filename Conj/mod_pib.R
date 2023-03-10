library(ggplot2)
library(patchwork)
library(dygraphs)
library(dynlm)
library(kableExtra)
library(forecast)

# FONCTIONS =========

trimestrialise = function(x, prefixe_series = NULL)
{
  UseMethod("trimestrialise")
}
trimestrialise.ts = function(x, prefixe_series = NULL)
{
  if(frequency(x)!=12)
    stop("Il faut que la série à trimestrialiser soit mensuelle !")

  date_debut_trim<-c(start(x)[1],(start(x)[2]-1)%/%3+1)
  series_mens<-window(x,start=date_debut_trim,extend=T)


  if(is.null(prefixe_series)){
    prefixe_series<-deparse(substitute(x))
  }else{
    if(length(prefixe_series)!=1)
      stop("Il faut autant des préfixes que de séries à trimestrialiser !")
  }

  series_m1<-ts(series_mens[stats::cycle(series_mens)%in%c(1,4,7,10)],start=date_debut_trim,frequency = 4)
  series_m2<-ts(series_mens[stats::cycle(series_mens)%in%c(2,5,8,11)],start=date_debut_trim,frequency = 4)
  series_m3<-ts(series_mens[stats::cycle(series_mens)%in%c(3,6,9,12)],start=date_debut_trim,frequency = 4)

  series_trim<-ts.union(series_m1,series_m2,series_m3)
  colnames(series_trim)<-paste(rep(prefixe_series,3),1:3,sep="_m")

  return(series_trim)
}

trimestrialise.mts = function(x, prefixe_series = NULL)
{
  if(frequency(x)!=12)
    stop("Il faut que les séries à trimestrialiser soient mensuelles !")

  date_debut_trim<-c(start(x)[1],(start(x)[2]-1)%/%3+1)
  series_mens<-window(x,start=date_debut_trim,extend=T)


  if(is.null(prefixe_series)){
    prefixe_series<-colnames(x)
  }else{
    if(length(prefixe_series)!=dim(x)[2])
      stop("Il faut autant des préfixes que de séries à trimestrialiser !")
  }

  series_m1<-ts(series_mens[stats::cycle(series_mens)%in%c(1,4,7,10),],start=date_debut_trim,frequency = 4)
  series_m2<-ts(series_mens[stats::cycle(series_mens)%in%c(2,5,8,11),],start=date_debut_trim,frequency = 4)
  series_m3<-ts(series_mens[stats::cycle(series_mens)%in%c(3,6,9,12),],start=date_debut_trim,frequency = 4)

  series_trim<-ts.union(series_m1,series_m2,series_m3)
  colnames(series_trim)<-paste(rep(prefixe_series,3),rep(1:3,each=length(prefixe_series)),sep="_m")
  return(series_trim)
}

lectureBDM <- function (idbank, ...)
{
  idbank <- gsub(" ", "", c(idbank, unlist(list(...))))
  UrlData <- paste0("https://bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
                    paste(idbank, collapse = "+"))
  tryCatch({
    dataBDM <- as.data.frame(rsdmx::readSDMX(UrlData, isURL = T),
                             stringsAsFactors = TRUE)
  }, error = function(e) {
    stop(paste0("Il y a une erreur dans le téléchargement des données. Vérifier le lien\n",
                UrlData), call. = FALSE)
  })
  FREQ <- levels(factor(dataBDM$FREQ))
  if (length(FREQ) != 1)
    stop("Les séries ne sont pas de la même périodicité !")
  freq <- switch(FREQ, M = 12, B = 6, T = 4, S = 2, A = 1)
  sepDate <- switch(FREQ, M = "-", B = "-B", T = "-Q", S = "-S",
                    A = " ")
  dataBDM <- reshape2::dcast(dataBDM, "TIME_PERIOD ~ IDBANK",
                             value.var = "OBS_VALUE")
  dataBDM <- dataBDM[order(dataBDM$TIME_PERIOD), ]
  dateDeb <- dataBDM$TIME_PERIOD[1]
  dateDeb <- regmatches(dateDeb, gregexpr(sepDate, dateDeb),
                        invert = T)[[1]]
  dateDeb <- as.numeric(dateDeb)
  dataBDM$TIME_PERIOD <- NULL
  dataBDM <- apply(dataBDM, 2, as.numeric)
  if (ncol(dataBDM) != length(idbank))
    warning(paste("Le ou les idbank suivant n'existent pas :",
                  paste(grep(paste(colnames(dataBDM), collapse = "|"),
                             idbank, value = T, invert = T), collapse = ", ")))
  if (ncol(dataBDM) > 1) {
    idbank <- idbank[idbank %in% colnames(dataBDM)]
    dataBDM <- dataBDM[, idbank]
  }
  dataBDM <- ts(dataBDM, start = dateDeb, frequency = freq)
  return(dataBDM)
}
# trend = FALSE; var_intercept = 0; var_variables = 0;
# fixed_intercept = TRUE; fixed_variables = TRUE
ssm_oos2 <- function (model, trend = FALSE, var_intercept = 0, var_variables = 0,
                      fixed_intercept = TRUE, fixed_variables = TRUE, ...)
{
  data <- tvCoef::get_data(model)
  est_data <- lapply(time(data)[-(1:28)], function(end_date) {
    window(data, end = end_date)
  })
  est_models <- lapply(est_data, tvCoef::ssm_lm, trend = trend, var_intercept = var_intercept,
                       var_variables = var_variables, fixed_intercept = fixed_intercept,
                       fixed_variables = fixed_variables, remove_last_dummies = TRUE)
  data_est <- t(sapply(est_models, function(x) tail(x$filtering_states,
                                                    1)))
  # last_ss <- est_models[[length(est_models)]]$smoothed_states
  # data_est <- rbind(data_est,
  #                   tail(last_ss, 1))
  oos_f <- ts(t(sapply(est_models, function(x) tail(x$filtering_states,
                                                    1))),
              end = end(data), frequency = frequency(data))
  oos_f[, ncol(oos_f)] <- data[,1] -
    ts(sapply(est_models, function(x) tail(x$fitted[,"filtering"],
                                           1)),
       end = end(data), frequency = frequency(data))
  colnames(oos_f) <- colnames(est_models[[1]]$filtering_states)
  oos_f
}
plot_coef <- function(data, titre = NULL){
  time <- time(data)
  # freq <- frequency(data)
  dataGraph <- data.frame(cbind(time, data))
  colnames(dataGraph) <- c("date", colnames(data) )

  dataGraph <- reshape2::melt(dataGraph, id="date")
  dataGraph$variable = factor(dataGraph$variable,
                              levels = colnames(data),
                              ordered = TRUE)

  #Paramètres graphiques (titre, labels etc.)
  ggplot(data = dataGraph, aes(x = date, y = value, colour = variable)) +
    geom_line() +
    theme_bw() +
    labs(title = titre,
         x = NULL, y = NULL)+
    theme(plot.title = element_text(hjust = 0.5),
          legend.background = element_rect(fill = alpha('gray99', 0.4),
                                           colour = "gray80", linetype = "solid"),
          legend.justification = c(0,0),
          legend.position = c(0,0),
          legend.key = element_blank(),
          legend.title = element_blank()
    )
}

prepare_plot <- function(data_p, y, trunc = TRUE) {
  data_p <- lapply(data_p, function(x) {
    colnames(x) <- c("LM", "SSM")
    x
  })
  data_p[["Prev"]] <- ts.intersect(y,
                                   y - data_p[[length(data_p)]])
  colnames(data_p[["Prev"]]) <- c("Y", "LM", "SSM")
  if (trunc) {
    data_p <- lapply(data_p, function(x){
      ts(rbind(window(x, end = c(2020,4)),
               NA, NA, NA, NA,
               window(x, start = c(2021,1))),
         frequency = frequency(x),
         end = end(x))
    })
  }
  data_p <- lapply(data_p, window, start = 2010)

  # all_p_gg <- lapply(names(data_p),function(x) {
  #   AQLTools::graph_ts(data_p[[x]], titre = x)
  # })

  all_p_dy <- lapply(names(data_p),function(x) {
    dygraph(data_p[[x]], main = x) %>%
      dyRangeSelector()
  })
  all_p_dy[[length(all_p_dy)]] <- all_p_dy[[length(all_p_dy)]]  %>%
    dySeries("Y", color = "red") %>%
    dyGroup(c("LM", "SSM"), color = c("lightblue", "darkgreen"),
            strokePattern = c("dotted"))
  pval_dm <- function(x){
    forecast::dm.test(na.omit(c(x[,"LM"])),
                      na.omit(c(x[,"SSM"])),
                      alternative  = "greater")$p.value
  }

  rmse <- data.frame(all = apply(data_p[["noise"]], 2, tvCoef::rmse),
                     pre2020 = apply(window(data_p[["noise"]], end = c(2019, 4)), 2, tvCoef::rmse)
  )
  pval = c(pval_dm(data_p[["noise"]]),
           pval_dm(window(data_p[["noise"]], end = c(2019, 4))))
  rmse <- rbind(rmse, pval)
  colnames(rmse) <- c("Ens. période", "Avant 2020")
  rownames(rmse) <- c("LM", "SSM", "P-val LM > SSM")

  list(#ggplot = all_p_gg,
    dy = all_p_dy,
    rmse = rmse)
}
compute_stats <- function(x){
  lm_x <- tvCoef::ssm_lm(x)
  lm_oos <- ssm_oos2(x)
  ssm_is <- tvCoef::ssm_lm(x, var_intercept = 0.1, fixed_intercept = FALSE,
                           var_variables = 0.1, fixed_variables = FALSE)
  ssm_oos <- ssm_oos2(x, var_intercept = 0.1, fixed_intercept = FALSE,
                      var_variables = 0.1, fixed_variables = FALSE)
  y <- tvCoef::get_data(x)[,1]
  data_p_is <- lapply(colnames(ssm_oos), function(nom_var){
    cbind(lm_x$smoothed_states[,nom_var], ssm_is$smoothed_states[,nom_var])
  })
  data_p_oos <- lapply(colnames(ssm_oos), function(nom_var){
    cbind(lm_oos[,nom_var], ssm_oos[,nom_var])
  })
  names(data_p_oos) <- names(data_p_is) <- colnames(ssm_oos)
  data_p <- data_p_oos
  oos <- prepare_plot(data_p_oos, y = y, trunc = TRUE)
  is <- prepare_plot(data_p_is, y = y, trunc = TRUE)
  list(is = is,
       oos = oos)
}

# MODELES CONJ =========

climat_fr <- "001565530"
pib <- "010565708"
pib <- lectureBDM(pib)
climat_fr <- lectureBDM(climat_fr)

soldes_trim <- trimestrialise(climat_fr)

pib = (pib / stats::lag(pib, -1)-1)*100

data_pib <- ts.union(pib, soldes_trim, diff(soldes_trim, 1))
colnames(data_pib) <- c("pib", sprintf("climat_fr_m%i", 1:3),
                        sprintf("diff_climat_fr_m%i", 1:3))
data_pib <- window(data_pib, start = 2000, end = c(2022,4)
)
data_pib_trunc <- ts(data_pib[!trunc(time(data_pib)) == 2020,], end = c(2022,4),frequency = 4)


mod1 <- dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1, data = data_pib)
mod2 <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib)
mod3 <- dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3, data = data_pib)

mod1_trunc <- dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1, data = data_pib_trunc)
mod2_trunc <- dynlm::dynlm(pib ~ climat_fr_m2 + diff_climat_fr_m2, data = data_pib_trunc)
mod3_trunc <- dynlm::dynlm(pib ~ climat_fr_m3 + diff_climat_fr_m3, data = data_pib_trunc)

stats_m1 <- compute_stats(mod1_trunc)

ind <- cbind(time(data_pib) == 2020, time(data_pib) == 2020.25, time(data_pib) == 2020.5,
      time(data_pib) == 2020.75)
ind <- ts(apply(ind,2, as.numeric), start = start(data_pib), frequency = 4)
data2 <- cbind(data_pib, ind)
colnames(data2) <- c(colnames(data_pib), sprintf("ind2020Q%i", 1:4))
mod1_ind <-dynlm::dynlm(pib ~ climat_fr_m1 + diff_climat_fr_m1 +
                            ind2020Q1 + ind2020Q2 + ind2020Q3 + ind2020Q4,
                          data = data2)
coef(mod1_ind)
coef(mod1_trunc)

# x= rmse_prev(mod1_ind, fixed_bw = TRUE, bw = 20)
# x$model$tvlm
# x$model$piece_tvlm
# window(x$prevision$prev_piece_tvlm$residuals, end = 2020) %>% rmse
# window(x$prevision$prev_piece_lm$residuals, end = 2020) %>% rmse
# window(x$prevision$prev_lm$residuals, end = 2020) %>% rmse
# window(x$prevision$prev_tvlm$residuals, end = 2020) %>% rmse
# x$prevision$prev_tvlm$model
#
# mod1_ind$coefficients
# mod1_ind$model
# oos_prev(mod1_ind)$residuals %>% rmse
# test_lm = piece_reg(mod1_ind)
# tail(test_lm$model$model, 20)
# test = piece_reg(mod1_ind, tvlm = TRUE)
# tail(test$model$coefficients, 20)

