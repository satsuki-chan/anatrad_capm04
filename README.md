# Modern Portfolio Theory (MPT) variables from Mexican Equity Funds (from 31-12-2010 to 31-12-2015)

## R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
Platform: x86_64-suse-linux-gnu (64-bit)
R Core Team (2015). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
https://www.R-project.org/

##Aditional packages:
###PerformanceAnalytics - Version == 1.4.3541
Brian G. Peterson and Peter Carl (2014). PerformanceAnalytics: Econometric tools for performance and risk analysis. R package version 1.4.3541.
https://CRAN.R-project.org/package=PerformanceAnalytics
https://cran.r-project.org/web/packages/PerformanceAnalytics/index.html

###xts - Version >= 0.9
Jeffrey A. Ryan and Joshua M. Ulrich (2014). xts: eXtensible Time Series. R package version 0.9-7.
https://CRAN.R-project.org/package=xts
https://cran.r-project.org/web/packages/xts/index.html

###zoo - Version >= 1.7-10
Achim Zeileis and Gabor Grothendieck (2005). zoo: S3 Infrastructure for Regular and Irregular Time Series. Journal of Statistical Software, 14(6), 1-27.
http://www.jstatsoft.org/v14/i06/
https://cran.r-project.org/web/packages/zoo/index.html

##Execution instructions
####Script file:
* `anatrad_capmvars.r`
####Input files:
* `RVMexico.prices_return_rates.csv`
* `RVMexico.market_return_rates_BMV.csv`
* `RVMexico.market_rrates_return_rates_CETES_364D.csv`
####Output file:
* `RVMexico.capm_analysis_<YYYYmmddHHMMSS>.csv`
To execute the script:
1. Verify that you have installed R, version 3.2.3 or above
2. Check that all the files are in the same directory and that you have reading permissions
3. Open a terminal  window and execute:
  * `R -f anatrad_capmvars.r`
