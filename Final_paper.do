clear matrix
clear mata
clear

set more off

**********************************************************************************************
* Data and panel structure
**********************************************************************************************

* Load dataset
bcuse keane, clear

* Define panel structure
xtset id year

* Keeping only ids present every year
xtpatternvar,gen(patternvar)
egen tag = tag(id patternvar)
egen IDcount = total(tag), by(patternvar)
drop if IDcount < 1738

* Panel structure
xtdes
* After keeping only ids present every year we have a perfectly balanced panel data with each entity observed at every time period.
* In total we have 1738 individuals and 7 time periods of information for each.

* Firm-specific effect
gen c = 5*id  

/*
The dataset used in this paper is an instructional dataset for econometrics from Boston College Department of Economics. This is a panel data with 12k observations across 7 years (1981-1987). We decided to keep only the observations that were present in all 7 time periods since they were more than 90% of the total, so to have a perfectly balanced data.
The aim of the paper is to analyze how the wage of individuals is affected by multiple factors, such as the years of education and the experience in different types of occupations (white collar, blue collar, services).



Panel data, which combines cross-sectional and time-series data, allows us to account for unobserved heterogeneity and capture the temporal dimension of wage changes, offering a more robust framework compared to pure cross-sectional or time-series analyses.

Regarding the panel structure, using descriptive statistics

Static model
Examine the direct impact of factors such as education and occupational experience on wage levels at a given point in time.
Control for individual-specific effects that do not change over time, providing a clear picture of cross-sectional relationships.
*/



**********************************************************************************************
* Descriptive statistics
**********************************************************************************************

xtsum lwage wage educ expwc expbc expser exper expersq

histogram wage, addplot(kdensity wage, lcolor(red) lwidth(medium)) legend(off)
histogram lwage, addplot(kdensity lwage, lcolor(red) lwidth(medium)) legend(off)
* Wages often exhibit a right-skewed distribution, where a small number of individuals earn very high wages compared to the majority.
* By looking at the histogram this is the case, so we decide to use lwage (log(wage)) as dependent variable, which, looking at the distribution plot, is normally distributed, helping in meeting the assumptions like normally distributed errors and heteroskedasticity, and reducing the impact of outliers.


egen mean_lwage = mean(lwage), by(year)
tab mean_lwage

egen mean_wage = mean(wage), by(year)
tab mean_wage
twoway (scatter mean_lwage year) 
/*
twoway (scatter lwage year) (line mean_lwage year), ///
    title("LWage vs Year with Mean Line") ///
    xlabel(, format(%ty)) ylabel(, format(%9.2f)) ///
    legend(order(1 "Individual LWage" 2 "Mean LWage"))
*/
	
**********************************************************************************************
* Definition of lagged values, first differences and lagged differences
**********************************************************************************************

foreach x of varlist lwage educ expwc expbc expser exper expersq {
        gen L1`x'=L.`x'      /* x_i,t-1 (1st lag) */
        gen L2`x'=L2.`x'     /* x_i,t-2 (2nd lag) */
    }

foreach x of varlist lwage educ expwc expbc expser exper expersq {
        gen D1`x'=D.`x'      /* x_i,t - x_i,t-1 (1st diff)*/
    }

foreach x of varlist lwage educ expwc expbc expser exper expersq {
        gen L1D1`x'=L.D1`x'      /* x_i,t-1 - x_i,t-2 (2nd diff)*/
        gen L2D1`x'=L.L1D1`x'    /* x_i,t-2 - x_i,t-3 (3rd diff)*/
		gen L3D1`x'=L.L2D1`x'    /* x_i,t-3 - x_i,t-4 (4th diff)*/
    }

	
**********************************************************************************************
* Assumptions Tests
**********************************************************************************************
	
***** Heteroskedasticity Test 

** Run OLS 
reg lwage educ expwc expbc expser exper expersq

** Test for heteroskedasticity 
estat hettest
* To test for heteroskedasticity we perform a Breusch–Pagan/Cook–Weisberg test, where the null hypothesis says that the errors are homoskedastic.
* Looking at the result we get a p-value of  0.5359, so we do not reject the null hypothesis of homoskedasticity, thus there is no need for standard error correction to control for heteroskedasticity.

** Run robust OLS
reg lwage educ expwc expbc expser exper expersq, robust
* For the sake of comparison we use robust to obtain heteroskedasticity consistent standard errors. We observe almost same values of the SE's compared with the SE's of the simple OLS. Thus, we do not have any indication of heteroskedasticity being present in the model. 


***** Unobserved Heterogeneity Test

** Fixed Effects (Within)
xtreg lwage educ expwc expbc expser exper expersq, fe
* The F-test for fixed effects is used to determine whether the fixed effects (entity-specific effects) in a panel data model are jointly significant. 
* Looking at the F-test we get a significant p-value, rejecting the null hypothesis that the firm-specific effect is equals zero for every id and indicating that the fixed effects are significant and that there is unobserved heterogeneity. 
* Thus, pooled OLS is not a suitable model to fit our data. 


***** Violation of Strict Exogeneity Test

 * Wooldrige strict exogeneity test
xtreg lwage educ expwc expbc expser exper expersq year F.educ F.expwc F.expbc F.expser F.exper F.expersq, fe
xtreg lwage educ expwc expbc expser exper expersq year F.educ F.expwc F.expbc F.expser F.exper F.expersq L.educ L.expwc L.expbc L.expser L.exper L.expersq, fe
* To test for strict exogeneity in a fixed effects model using the approach suggested by Wooldridge, we include leads and lags of the independent variables in the regression and test their joint significance.
* Looking at the output we can see that the leads of expwc and expbc are significant and, about the F-test, we get a significant p-value, rejecting the null hypothesis and indicating that the lagged and future values of the explanatory variables are jointly significant. 
* This suggests that the explanatory variables are correlated with the error term, violating the strict exogeneity assumption.


**********************************************************************************************
* Static Models
**********************************************************************************************

* Our main goal its to see which one fits better (random vs fixed) but we will first still run a pooled ols and a between estimator.

** POOLED OLS
reg lwage educ expwc expbc expser exper expersq
* Running the pooled OLS we see that all variables are significant and the R-squared is 0.23.
* But the pooled OLS is not a suitable model since the panel structure of our data is ignored and the data are treated as if they had a cross-section structure. The pooled OLS does not account for the unobserved heterogeneity within our panels.

** BETWEEN ESTIMATOR
xtreg lwage educ expwc expbc expser exper expersq, be
* The between estimator is also not suitable model as it only accounts for the between countries variation, ignoring for the existence of firm-specific effect.

** FIXED EFFECTS (WITHIN) MODEL
xtreg lwage educ expwc expbc expser exper expersq year, fe
xtreg lwage educ expwc expbc expser exper expersq year, fe robust
est store fixed
* 

** FIRST DIFFERENCES MODEL
reg D.lwage D.educ D.expwc D.expbc D.expser D.exper D.expersq year, nocons

* We obtain 1 significant variable and using differences we control for the unobserved heterogeneity, but the violation of strict exogeneity is still not addressed. With the robust option we get similar SE's indicating no heteroskedasticity issue. 

** RANDOM EFFECTS MODEL 
xtreg lwage educ expwc expbc expser exper expersq year, re
* The majority (56.96%) of the variability in the outcome variable can be explained by group-level characteristics.
* This suggests that there is significant variation between groups, justifying the use of a random effects model to account for these differences.
* In the random effect model we assume that our regressors are not correlated with the firm-specific effect. We observe that the variance of the firm-specific effect explains only small fraction of the overall variance. That is an indicator that the unobserved heterogeneity is not very important in our model.
* Large fraction of the variance of the overall error term is due to the variance of the individual effect. Indicates that individual heterogeneity is important.



* Hausman Test (Compare Random vs. Fixed Effets Model)
hausman fixed .
* Given the low p-value, lower than the  conventional significance level (0.05), we reject the null hypothesis. This indicates that there is a systematic difference between the fixed and random effects models, suggesting that the fixed effects model is more appropriate for our data.

* Make use of instruments for the regressors that violate strict exogeneity assumption 
 * All lagged instruments
ivregress 2sls D.lwage  D.expser D.exper D.educ D.expersq year (D.expwc D.expbc = L.expwc L.expbc), nocons first small robust

* All lagged instruments plus all lagged difference (best model, all variables significant)
ivregress 2sls D.lwage D.educ D.expser D.exper D.expersq year (D.expwc D.expbc = L.expwc L.expbc L.D.expwc L.D.expbc), nocons first small robust

* All lagged difference
ivregress 2sls D.lwage D.educ D.expser D.exper D.expersq year (D.expwc D.expbc = L.D.expwc L.D.expbc), nocons first small robust

* All lagged difference plus all lagged difference and all 2nd lag differences
ivregress 2sls D.lwage D.educ D.expser D.exper D.expersq year (D.expwc D.expbc = L.expwc L.expbc L.D.expwc L.D.expbc L2D1expwc L2D1expbc), nocons first small robust


**********************************************************************************************
* Dynamic Models
**********************************************************************************************

***** 1. IV-AH (Anderson-Hsiao) estimator 

** Using yi,t-2 - yi,t-3 as instrument
    xtivreg2 lwage educ expwc expbc expser exper expersq (L.lwage = L2.lwage), fd
	xtivreg2 lwage educ expwc expbc expser exper expersq (L.lwage = L2.lwage), fd robust
/*  ivregress 2sls D.lwage D.educ D.expwc D.expbc (L.D.lwage = L2.D.lwage) */

  /*
    Comments:
    The lagged dependent variable is significant. All other variables remain 
    significant.
    Coefficient estimates for w (from -0.36 to -0.56) and k (from 0.54 to 0.40),
    while the coeffient for ys is nearly unchanged by introducing dynamics.
    Note that we cannot apply an overidentification test on instrument
    validity.
    */   


** Using yi,t-2 as instrument
    ivregress 2sls D.lwage D.educ D.expwc D.expbc (L.D.lwage = L2.lwage)

	    /* 
    Comments:
    Results are very sensitive to the instrument which we choose. Now k does not
    significantly enter the equation anymore.
    */
	
	
** look at correlation in first stage regression
    xtivreg2 lwage educ expwc expbc (L.lwage = L2.lwage), fd first
	/* F test > 10 (34.4) shows that our instrument is significant */
    ivregress 2sls D.lwage D.educ D.expwc D.expbc (L.D.lwage = L2.lwage), first
    estat first
* Comments: Lagged level and difference are significant in first stage.



***** 2. GMM estimation	

/* gmm and iv: used to specify correlation between expl. variables and idiosyncratic error term
   * gmm: for endogenous or predetermined variables
        predetermined, all possible lagged values as instruments: gmm(w) or gmm(w, laglimits(1 .))
        endogenous, all possible lagged values as instruments: gmm(w, laglimits(2 .))
   * iv: for strictly exogenous variables: iv(w) 
   * noleveleq: indicates differenced GMM (Arellano-Bond), default: System GMM
   * robust: finite-sample correction of the standard errors	
 */
	
* 1. exper expwc expbc endogenous using all possible lagged values
	
* One-step and no finite-sample correction	
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(2 .)) gmm(expwc, laglimits(2 .)) gmm(expbc, laglimits(2 .)) iv(year, eq(both)) noleveleq
	
* One-step and finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(2 .)) gmm(expwc, laglimits(2 .)) gmm(expbc, laglimits(2 .)) iv(year, eq(both)) noleveleq robust
	
* Two-step and no finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(2 .)) gmm(expwc, laglimits(2 .)) gmm(expbc, laglimits(2 .)) iv(year, eq(both)) noleveleq twostep 
	
* Two-step and finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(2 .)) gmm(expwc, laglimits(2 .)) gmm(expbc, laglimits(2 .)) iv(year, eq(both)) noleveleq twostep robust
	
* Two-Step SYSGMM and finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(2 .)) gmm(expwc, laglimits(2 .)) gmm(expbc, laglimits(2 .)) iv(year, eq(both)) twostep robust

	
	
* 2. exper expwc expbc predetermined using all possible lagged values	
	
* One-step and no finite-sample correction	
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(1 .)) gmm(expwc, laglimits(1 .)) gmm(expbc, laglimits(1 .)) iv(year, eq(both)) noleveleq
	
* One-step and finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(1 .)) gmm(expwc, laglimits(1 .)) gmm(expbc, laglimits(1 .)) iv(year, eq(both)) noleveleq robust
	
* Two-step and no finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(1 .)) gmm(expwc, laglimits(1 .)) gmm(expbc, laglimits(1 .)) iv(year, eq(both)) noleveleq twostep 
	
* Two-step and finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(1 .)) gmm(expwc, laglimits(1 .)) gmm(expbc, laglimits(1 .)) iv(year, eq(both)) noleveleq twostep robust
	
* Two-Step SYSGMM and finite-sample correction
	xtabond2  lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) gmm(exper, laglimits(1 .)) gmm(expwc, laglimits(1 .)) gmm(expbc, laglimits(1 .)) iv(year, eq(both)) twostep robust

	
	xtabond2 lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) iv(exper) iv(expwc) iv(expbc) iv(year) noleveleq 

	xtabond2 lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) iv(exper) iv(expwc) iv(expbc) iv(year) noleveleq robust
	
	xtabond2 lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) iv(exper) iv(expwc) iv(expbc) iv(year) noleveleq twostep

	xtabond2 lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) iv(exper) iv(expwc) iv(expbc) iv(year) noleveleq twostep robust
	
	xtabond2 lwage L.lwage educ exper expwc expbc year, gmm(L.lwage) iv(exper) iv(expwc) iv(expbc) iv(year) twostep robust
	
	