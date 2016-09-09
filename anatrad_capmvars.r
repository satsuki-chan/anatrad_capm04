################################################################################
# Script para generar variables CAPM en fondos de Renta Variable Mexicana
################################################################################
# descargar los paquetes de CRAN
#install.packages("data.table")
#install.packages("xts")
#install.packages("zoo")
#install.packages("PerformanceAnalytics")
# cargar paquetes a utilizar
require(PerformanceAnalytics)
# Configurar nombre para archivo con resultados
f_ana_result <- paste("RVMexico.capm_analysis_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep="")

# cargar archivos con series de tasas de returno de fondos e índices para comparación
df_founds <- read.csv("RVMexico.prices_return_rates.csv")
df_i_bmv <- read.csv("RVMexico.market_return_rates_BMV.csv")
df_rf_rate <- read.csv("RVMexico.market_rrates_return_rates_CETES_364D.csv")
# obtener datos de la lista de precios de fondos
l_founds <- levels(df_founds[, 1])
l_founds <- c("BMV", l_founds)

n_founds <- length(l_founds)
# Convertir listas de precios de índices a series de tiempo
df_tseries_i_bmv <- as.xts(df_i_bmv[, "c_rate"], as.Date.character(df_i_bmv[, "b_date"]), as.Date.character(df_i_bmv[, "b_date"]))
df_tseries_rf_rate <- as.xts(df_rf_rate[, "c_rate"], as.Date.character(df_rf_rate[, "b_date"]), as.Date.character(df_rf_rate[, "b_date"]))
# Crear lista de ganacias del índice BMV relativa a una tasa sin riesgo
df_tseries_xi_bmv <- Return.excess(df_tseries_i_bmv[, 1], df_tseries_rf_rate[, 1])
# preparar listas para registro de resultados
l_capm_alpha_i_bmv <- numeric(n_founds)
l_capm_beta_i_bmv <- numeric(n_founds)
l_capm_r2 <- numeric(n_founds)
l_capm_sd <- numeric(n_founds)
l_capm_sr_i_bmv <- numeric(n_founds)

# lista de fondos con número incompleto de registros
m_wrong_founds <- rbind()

# Variables con BMV #
### Cálculo de Alfa ###
l_capm_alpha_i_bmv[1] <- CAPM.alpha(df_tseries_i_bmv[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

### Cálculo de Beta (Coeficiente Beta) ###
l_capm_beta_i_bmv[1] <- CAPM.beta(df_tseries_i_bmv[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

### Cálculo de R-Cuadrada (Coeficiente de Determinación) ###
#l_lrm_data <- list(a_found = df_i_bmv[, "c_rate"])
#l_lrm_data$b_benchmark <- df_i_bmv[, "c_rate"]
#df_lrm_data <- as.data.frame(l_lrm_data)
df_tseries_xfound <- Return.excess(df_tseries_i_bmv[, 1], df_tseries_rf_rate[, 1])
df_lrm_data <- as.data.frame(na.omit(cbind(df_tseries_xfound, df_tseries_xi_bmv)))
names(df_lrm_data) <- c("a_found", "b_benchmark")
df_lrm_data.lm <- lm(a_found ~ b_benchmark, data=df_lrm_data)
l_capm_r2[1] <- summary(df_lrm_data.lm)$r.squared

### Cálculo de Derivada Estándar ###
l_capm_sd[1] <- StdDev(df_tseries_i_bmv[, 1], portfolio_method="single")

### Cálculo de la Razón de Sharpe ###
l_capm_sr_i_bmv[1] <- SharpeRatio(df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1], FUN="StdDev")


# Variables con el respo de los fondos #
for (i_found in 2:n_founds) {
    # Separar precios del fondo a analizar
    df_found <- df_founds[df_founds$a_found == l_founds[i_found],]
    # Verificar que la cantidad de registros de precios coincida con el índice de la BMV
    if (length(df_found[, "b_date"]) == length(df_i_bmv[, "b_date"])) {
    # Convertir la lista de precios de cada fondo a una serie de tiempo
        df_tseries_found <- as.xts(df_found[, "c_rate"], as.Date.character(df_found[, "b_date"]), as.Date.character(df_found[, "b_date"]))
    ### Cálculo de Alfa ###
    # CAPM.alpha(Ra, Rb, Rf = 0)
    # - Argumentos
    #   Ra: Un objeto xts, vector, matrix, data frame, timeSeries ó zoo con el retorno del instrumento de inversión
    #   Rb: Vector con el retorno del benchmark del instrumento de inversión
    #   Rf: Razón libre de riesgo, en el mismo período que el retorno de inversión
        l_capm_alpha_i_bmv[i_found] <- CAPM.alpha(df_tseries_found[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

    ### Cálculo de Beta (Coeficiente Beta) ###
    # CAPM.beta(Ra, Rb, Rf = 0)
    # - Argumentos
    #   Ra: Un objeto xts, vector, matrix, data frame, timeSeries ó zoo con el retorno del instrumento de inversión
    #   Rb: Vector con los retornos del benchmark del instrumento de inversión
    #   Rf: Razón libre de riesgo, en el mismo período que el retorno de inversión
        l_capm_beta_i_bmv[i_found] <- CAPM.beta(df_tseries_found[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

    ### Cálculo de R-Cuadrada (Coeficiente de Determinación) ###
    # summary.lm
    #   $r.squared: R^2, the ‘fraction of variance explained by the model’,
    #                  R^2 = 1 - Sum(R[i]^2) / Sum((y[i]- y*)^2),
    #               where y* is the mean of y[i] if there is an intercept and zero otherwise.
        #l_lrm_data <- list(a_found = df_found[, "c_rate"])
        #l_lrm_data$b_benchmark <- df_i_bmv[, "c_rate"]
        df_tseries_xfound <- Return.excess(df_tseries_found[, 1], df_tseries_rf_rate[, 1])
        #df_lrm_data <- as.data.frame(l_lrm_data)
        df_lrm_data <- as.data.frame(na.omit(cbind(df_tseries_xfound, df_tseries_xi_bmv)))
        names(df_lrm_data) <- c("a_found", "b_benchmark")
        df_lrm_data.lm <- lm(a_found ~ b_benchmark, data=df_lrm_data)
        l_capm_r2[i_found] <- summary(df_lrm_data.lm)$r.squared

    ### Cálculo de Derivada Estándar ###
    # StdDev(R, ..., clean = c("none", "boudt", "geltner"), portfolio_method = c("single", "component"), weights = NULL, mu = NULL, sigma = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
    # - Argumentos
    #   R:                A vector, matrix, data frame, timeSeries or zoo object of asset returns
    #   clean:            Method for data cleaning through Return.clean. Current options are "none", "boudt", or "geltner".
    #   portfolio_method: One of "single","component" defining whether to do univariate/multivariate or component calc, see Details.
    #   weights:          Portfolio weighting vector, default NULL, see Details
    #   mu:               If univariate, mu is the mean of the series. Otherwise mu is the vector of means of the return series, default NULL, see Details
    #   sigma:            If univariate, sigma is the variance of the series. Otherwise sigma is the covariance matrix of the return series, default NULL, see Details
    #   use:              An optional character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".
    #   method:           A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated.
    #   ...:              Any other passthru parameters
        l_capm_sd[i_found] <- StdDev(df_tseries_found[, 1], portfolio_method="single")

    ### Cálculo de la Razón de Sharpe ###
    # SharpeRatio(R, Rf = 0, p = 0.95, FUN = c("StdDev", "VaR", "ES"), weights = NULL, annualize = FALSE, ...)
    # - Argumentos
    #   R:         An xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
    #   Rf:        Risk free rate, in same period as your returns
    #   p:         Confidence level for calculation, default p=.95
    #   FUN:       One of "StdDev" or "VaR" or "ES" to use as the denominator
    #   weights:   Portfolio weighting vector, default NULL, see Details in VaR
    #   annualize: If TRUE, annualize the measure, default FALSE
    #   ...:       Any other passthru parameters to the VaR or ES functions
        l_capm_sr_i_bmv[i_found] <- SharpeRatio(df_tseries_found[, 1], Rf=df_tseries_rf_rate[, 1], FUN="StdDev")
    } # if
    else {
        m_wrong_founds <- rbind(m_wrong_founds, i_found)
    } # else
} # for
# Crea matriz con resultados en un data.frame para crear reporte
df_capm_results <- data.frame(
      l_capm_alpha_i_bmv
    , l_capm_beta_i_bmv
    , l_capm_r2
    , l_capm_sd
    , l_capm_sr_i_bmv
    , row.names=l_founds, stringsAsFactors=FALSE)
names(df_capm_results) <- c(
      "alpha v BMV - w/ CETES 364D"
    , "beta v BMV - w/ CETES 364D"
    , "r-squared"
    , "standar deviation"
    , "Sharpe Ratio - w/ CETES 364D"
    )
# Eliminar fondos con número incompleto de registros
n_wrong_founds <- length(m_wrong_founds[, 1])
l_wrong_founds <- integer(n_wrong_founds)
if (n_wrong_founds >= 1) {
    for (i_wrong_found in 1:n_wrong_founds) {
        l_wrong_founds[i_wrong_found] <- m_wrong_founds[i_wrong_found, 1]
    } # for
    df_capm_results <- df_capm_results[-c(l_wrong_founds),]
    l_founds <- l_founds[-c(l_wrong_founds)]
} # if

# Escribe y muestra resultados
write.csv(df_capm_results, f_ana_result)
print(df_capm_results)
