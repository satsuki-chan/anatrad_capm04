################################################################################
# Script to calculate Modern Portfolio Theory (MPT) variables from Mexican equity funds
################################################################################
# Base packages need:
# stats              # Version >=3.2.3
# Download and install of needed packages from CRAN Package archive website:
#install.packages("PerformanceAnalytics") # Version ==1.4.3541
#install.packages("xts")                  # Version >=0.9
#install.packages("zoo")                  # Version >=1.7-10

# Load required package, besides base packages from R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
require(PerformanceAnalytics)
# Name for file with results
f_ana_result <- paste("RVMexico.capm_analysis_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep="")

# Load of equity funds' and index yearly return rates series
df_founds <- read.csv("RVMexico.prices_return_rates.csv")
df_i_bmv <- read.csv("RVMexico.market_return_rates_BMV.csv")
df_rf_rate <- read.csv("RVMexico.market_rrates_return_rates_CETES_364D.csv")
# Equity fund's and index name list
l_founds <- levels(df_founds[, 1])
l_founds <- c("BMV", l_founds)
n_founds <- length(l_founds)

# Transformation of index and risk-free security's return rates from list format to xts time series format
df_tseries_i_bmv <- as.xts(df_i_bmv[, "c_rate"], as.Date.character(df_i_bmv[, "b_date"]), as.Date.character(df_i_bmv[, "b_date"]))
df_tseries_rf_rate <- as.xts(df_rf_rate[, "c_rate"], as.Date.character(df_rf_rate[, "b_date"]), as.Date.character(df_rf_rate[, "b_date"]))
# Excess returns lists of the BMV (Bolsa Mexicana de Valores, Mexican Stock Exchange) relative to a risk-free return
df_tseries_xi_bmv <- Return.excess(df_tseries_i_bmv[, 1], df_tseries_rf_rate[, 1])
# Allocation of result lists
l_capm_alpha_i_bmv <- numeric(n_founds)
l_capm_beta_i_bmv <- numeric(n_founds)
l_capm_r2 <- numeric(n_founds)
l_capm_sd <- numeric(n_founds)
l_capm_sr_i_bmv <- numeric(n_founds)

# List of funds with incomplete time series
m_wrong_founds <- rbind()

# BMV index MPT variables #
### Alpha calculation ###
l_capm_alpha_i_bmv[1] <- CAPM.alpha(df_tseries_i_bmv[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

### Beta calculation (Beta coefficient) ###
l_capm_beta_i_bmv[1] <- CAPM.beta(df_tseries_i_bmv[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

### R-squared calculation (coefficient of determination) ###
df_tseries_xfound <- Return.excess(df_tseries_i_bmv[, 1], df_tseries_rf_rate[, 1])
df_lrm_data <- as.data.frame(na.omit(cbind(df_tseries_xfound, df_tseries_xi_bmv)))
names(df_lrm_data) <- c("a_found", "b_benchmark")
df_lrm_data.lm <- lm(a_found ~ b_benchmark, data=df_lrm_data)
l_capm_r2[1] <- summary(df_lrm_data.lm)$r.squared

### Standar Deviation calculation ###
l_capm_sd[1] <- StdDev(df_tseries_i_bmv[, 1], portfolio_method="single")

### Sharpe Ratio calculation ###
l_capm_sr_i_bmv[1] <- SharpeRatio(df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1], FUN="StdDev")


# Equity funds MPT variables #
for (i_found in 2:n_founds) {
    # Separate return rates of the fund to analyze
    df_found <- df_founds[df_founds$a_found == l_founds[i_found],]
    # Check that the number of rates is the same as the BMV index
    if (length(df_found[, "b_date"]) == length(df_i_bmv[, "b_date"])) {
    # Transformation of each fund's return rates to xts time series format
        df_tseries_found <- as.xts(df_found[, "c_rate"], as.Date.character(df_found[, "b_date"]), as.Date.character(df_found[, "b_date"]))
    ### Alpha calculation ###
    # CAPM.alpha(Ra, Rb, Rf = 0)
    # - Arguments
    #   Ra: An xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
    #   Rb: Return vector of the benchmark asset
    #   Rf: Risk-free rate, in same period as your returns
        l_capm_alpha_i_bmv[i_found] <- CAPM.alpha(df_tseries_found[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

    ### Beta calculation (Beta coefficient) ###
    # CAPM.beta(Ra, Rb, Rf = 0)
    # - Arguments
    #   Ra: An xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
    #   Rb: Return vector of the benchmark asset
    #   Rf: Risk-free rate, in same period as your returns
        l_capm_beta_i_bmv[i_found] <- CAPM.beta(df_tseries_found[, 1], df_tseries_i_bmv[, 1], Rf=df_tseries_rf_rate[, 1])

    ### R-squared calculation (coefficient of determination) ###
    #   lm(formula, data, subset, weights, na.action, method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, ...)
    # Description:
    # ‘lm’ is used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance (although ‘aov’ may provide a more convenient interface for these).
    # - Arguments
    #          formula: An object of class ‘"formula"’ (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
    #             data: An optional data frame, list or environment (or object coercible by ‘as.data.frame’ to a data frame) containing the variables in the model.  If not found in ‘data’, the variables are taken from ‘environment(formula)’, typically the environment from which ‘lm’ is called.
    #           subset: An optional vector specifying a subset of observations to be used in the fitting process.
    #          weights: An optional vector of weights to be used in the fitting process. Should be ‘NULL’ or a numeric vector. If non-NULL, weighted least squares is used with weights ‘weights’ (that is, minimizing ‘sum(w*e^2)’); otherwise ordinary least squares is used.  See also ‘Details’,
    #        na.action: A function which indicates what should happen when the data contain ‘NA’s. The default is set by the ‘na.action’ setting of ‘options’, and is ‘na.fail’ if that is unset. The ‘factory-fresh’ default is ‘na.omit’.  Another possible value is ‘NULL’, no action.  Value ‘na.exclude’ can be useful.
    #           method: The method to be used; for fitting, currently only ‘method = "qr"’ is supported; ‘method = "model.frame"’ returns the model frame (the same as with ‘model = TRUE’, see below).
    #  model, x, y, qr: Logicals. If ‘TRUE’ the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
    #      singular.ok: Logical. If ‘FALSE’ (the default in S but not in R) a singular fit is an error.
    #        contrasts: an optional list. See the ‘contrasts.arg’ of ‘model.matrix.default’.
    #           offset: This can be used to specify an _a priori_ known component to be included in the linear predictor during fitting.  This should be ‘NULL’ or a numeric vector of length equal to the number of cases.  One or more ‘offset’ terms can be included in the formula instead or as well, and if more than one are specified their sum is used.  See ‘model.offset’.
    #              ...: Additional arguments to be passed to the low level regression fitting functions.
    # - Details
    #   Models for ‘lm’ are specified symbolically.  A typical model has the form ‘response ~ terms’ where ‘response’ is the (numeric) response vector and ‘terms’ is a series of terms which specifies a linear predictor for ‘response’.  A terms specification of the form ‘first + second’ indicates all the terms in ‘first’ together with all the terms in ‘second’ with duplicates removed.  A specification of the form ‘first:second’ indicates the set of terms obtained by taking the interactions of all terms in ‘first’ with all terms in ‘second’.  The specification ‘first*second’ indicates the _cross_ of ‘first’ and ‘second’.  This is the same as ‘first + second + first:second’.
    # - Value
    #   ‘lm’ returns an object of ‘class’ ‘"lm"’ or for multiple responses of class ‘c("mlm", "lm")’.
    #   The functions ‘summary’ and ‘anova’ are used to obtain and print a summary and analysis of variance table of the results.  The generic accessor functions ‘coefficients’, ‘effects’, ‘fitted.values’ and ‘residuals’ extract various useful features of the value returned by ‘lm’.
    #   An object of class ‘"lm"’ is a list containing at least the following components:
    #   -  coefficients: a named vector of coefficients
    #   -     residuals: the residuals, that is response minus fitted values.
    #   - fitted.values: the fitted mean values.
    #   -          rank: the numeric rank of the fitted linear model.
    #   -       weights: (only for weighted fits) the specified weights.
    #   -   df.residual: the residual degrees of freedom.
    #   -          call: the matched call.
    #   -         terms: the ‘terms’ object used.
    #   -     contrasts: (only where relevant) the contrasts used.
    #   -       xlevels: (only where relevant) a record of the levels of the factors used in fitting.
    #   -        offset: the offset used (missing if none were used).
    #   -             y: if requested, the response used.
    #   -             x: if requested, the model matrix used.
    #   -         model: if requested (the default), the model frame used.
    #   -     na.action: (where relevant) information returned by ‘model.frame’ on the special handling of ‘NA’s. In addition, non-null fits will have components ‘assign’, ‘effects’ and (unless not requested) ‘qr’ relating to the linear fit, for use by extractor functions such as ‘summary’ and ‘effects’.
        df_tseries_xfound <- Return.excess(df_tseries_found[, 1], df_tseries_rf_rate[, 1])
        df_lrm_data <- as.data.frame(na.omit(cbind(df_tseries_xfound, df_tseries_xi_bmv)))
        names(df_lrm_data) <- c("a_found", "b_benchmark")
        df_lrm_data.lm <- lm(a_found ~ b_benchmark, data=df_lrm_data)
    #   # S3 method for class 'lm'
    #   summary(object, correlation = FALSE, symbolic.cor = FALSE, ...)
    #   # S3 method for class 'summary.lm'
    #   print(x, digits = max(3, getOption("digits") - 3), symbolic.cor = x$symbolic.cor, signif.stars = getOption("show.signif.stars"), ...)
    # - Arguments
    #         object: An object of class ‘"lm"’, usually, a result of a call to ‘lm’.
    #    correlation: Logical; if ‘TRUE’, the correlation matrix of the estimated parameters is returned and printed.
    #   symbolic.cor: Logical. If ‘TRUE’, print the correlations in a symbolic form (see ‘symnum’) rather than as numbers.
    #            ...: Further arguments passed to or from other methods.
    # - Value
    #   The function ‘summary.lm’ computes and returns a list of summary statistics of the fitted linear model given in ‘object’, using the components (list elements) ‘"call"’ and ‘"terms"’ from its argument, plus:
    #      residuals: The _weighted_ residuals, the usual residuals rescaled by the square root of the weights specified in the call to ‘lm’.
    #   coefficients: A p x 4 matrix with columns for the estimated coefficient, its standard error, t-statistic and corresponding (two-sided) p-value. Aliased coefficients are omitted.
    #        aliased: Named logical vector showing if the original coefficients are aliased.
    #          sigma: The square root of the estimated variance of the random error
    #                 sigma^2 = 1/(n-p) Sum(w[i] R[i]^2),
    #                 where R[i] is the i-th residual, ‘residuals[i]’.
    #             df: Degrees of freedom, a 3-vector (p, n-p, p*), the first being the number of non-aliased coefficients, the last being the total number of coefficients.
    #     fstatistic: (For models including non-intercept terms) A 3-vector with the value of the F-statistic with its numerator and denominator degrees of freedom.
    #      r.squared: R^2, the ‘fraction of variance explained by the model’,
    #                 R^2 = 1 - Sum(R[i]^2) / Sum((y[i]- y*)^2),
    #                 where y* is the mean of y[i] if there is an intercept and zero otherwise.
    #  adj.r.squared: The above R^2 statistic ‘_adjusted_’, penalizing for higher p.
    #   cov.unscaled: A p x p matrix of (unscaled) covariances of the coef[j], j=1, ..., p.
    #    correlation: The correlation matrix corresponding to the above ‘cov.unscaled’, if ‘correlation = TRUE’ is specified.
    #   symbolic.cor: (Only if ‘correlation’ is true.)  The value of the argument ‘symbolic.cor’.
    #      na.action: From ‘object’, if present there.
        l_capm_r2[i_found] <- summary(df_lrm_data.lm)$r.squared

    ### Standar Deviation calculation ###
    # StdDev(R, ..., clean = c("none", "boudt", "geltner"), portfolio_method = c("single", "component"), weights = NULL, mu = NULL, sigma = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
    # - Arguments
    #                  R: A vector, matrix, data frame, timeSeries or zoo object of asset returns.
    #              clean: Method for data cleaning through Return.clean. Current options are "none", "boudt", or "geltner".
    #   portfolio_method: One of "single","component" defining whether to do univariate/multivariate or component calc.
    #            weights: Portfolio weighting vector, default NULL.
    #                 mu: If univariate, mu is the mean of the series. Otherwise mu is the vector of means of the return series, default NULL.
    #              sigma: If univariate, sigma is the variance of the series. Otherwise sigma is the covariance matrix of the return series, default NULL.
    #                use: An optional character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".
    #             method: A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated.
    #                ...: Any other passthru parameters
        l_capm_sd[i_found] <- StdDev(df_tseries_found[, 1], portfolio_method="single")

    ### Sharpe Ratio calculation ###
    # SharpeRatio(R, Rf = 0, p = 0.95, FUN = c("StdDev", "VaR", "ES"), weights = NULL, annualize = FALSE, ...)
    # - Arguments
    #           R: An xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
    #          Rf: Risk free rate, in same period as your returns
    #           p: Confidence level for calculation, default p=.95
    #         FUN: One of "StdDev" or "VaR" or "ES" to use as the denominator
    #     weights: Portfolio weighting vector, default NULL, see Details in VaR
    #   annualize: If TRUE, annualize the measure, default FALSE
    #         ...: Any other passthru parameters to the VaR or ES functions
        l_capm_sr_i_bmv[i_found] <- SharpeRatio(df_tseries_found[, 1], Rf=df_tseries_rf_rate[, 1], FUN="StdDev")
    } # if
    else {
        m_wrong_founds <- rbind(m_wrong_founds, i_found)
    } # else
} # for

# Matrix with results in data.frame format
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
    , "r squared"
    , "standar deviation"
    , "Sharpe ratio - w/ CETES 364D"
    )

# Removal of funds with incomplete number of returns
n_wrong_founds <- length(m_wrong_founds[, 1])
l_wrong_founds <- integer(n_wrong_founds)
if (n_wrong_founds >= 1) {
    for (i_wrong_found in 1:n_wrong_founds) {
        l_wrong_founds[i_wrong_found] <- m_wrong_founds[i_wrong_found, 1]
    } # for
    df_capm_results <- df_capm_results[-c(l_wrong_founds),]
    l_founds <- l_founds[-c(l_wrong_founds)]
} # if

# Writing and displaying of results
write.csv(df_capm_results, f_ana_result)
print(df_capm_results)
