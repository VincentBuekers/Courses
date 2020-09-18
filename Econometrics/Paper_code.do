clear
capture log close
set more off

cd "/Users/Vincent/Desktop/AA Econometrics/Paper"
log using Paper_Vincent_Buekers.log, replace

* -----------------------------------------------------
* Do-file: Paper for Advanced Applied Econometrics 2019
* Vincent Buekers
*------------------------------------------------------

* Data
import delimited data-cmbs.txt

* Panel data specification
tsset cid year

* remove k due to lots of missing values.
* --> kbc30 will be used as physical capital variable
drop k

* info on panel data
xtdes

*-----------------------
* Descriptive statistics
*-----------------------

	xtsum y kbc30 lwdi secondary egc mlines troads cells proads rails

*-------------------------
* Static Regression models
*-------------------------

******
** OLS
******
    reg y L.y kbc30 lwdi secondary egc mlines troads cells proads rails

* Test on homoskedasticity 

    estat hettest
	
* robust OLS
	reg y L.y kbc30 lwdi secondary egc mlines troads cells proads rails, robust

*****************
* FIXED EFFECTS *
*****************

** within estimation
	xtreg y kbc30 lwdi secondary egc mlines troads cells proads rails, fe
	est store fixed

** additional goodness of fit criteria 
	estat ic

** fitted values:

    ** linear prediction (x*beta_hat)
        predict hfe_xb, xb

    ** linear prediction including FE component (x*beta_hat + alphai_hat)
        predict hfe_xb_alphai, xbu

    ** prediction of FE (alphai_hat)
        predict hfe_alphai, u

    ** prediction of idiosyncratic error term (e_hat)
        predict hfe_e, e

    ** prediction of FE and idiosyncratic error term (alphai_hat + e_hat)
        predict hfe_alphai_e, ue

        
    list cid year y hfe_xb hfe_xb_alphai hfe_alphai hfe_e in 1/20


** within estimation with heteroskedasticity-robust standard errors:

    xtreg y kbc30 lwdi secondary egc mlines troads cells proads rails, fe robust


** First differences
	reg D.y D.kbc30 D.lwdi D.secondary D.egc D.mlines D.troads D.cells D.proads D.rails, nocons

** test for strict exogeneity
	* within
	xtreg y kbc30 F.kbc30 lwdi F.lwdi secondary F.secondary egc F.egc mlines F.mlines troads F.troads cells F.cells proads F.proads rails F.rails, fe

	* FD
	reg D.y D.kbc30 kbc30 D.lwdi lwdi D.secondary secondary D.egc egc D.mlines mlines D.troads troads D.cells cells D.proads proads D.rails rails, nocons

*****************
** Random effects
*****************

	xtreg y kbc30 lwdi secondary egc mlines troads cells proads rails, re
	est store random

** fitted values:

    ** linear prediction (x*beta_hat)
        predict hre_xb, xb

    ** linear prediction including FE component (x*beta_hat + alphai_hat)
        predict hre_xb_alphai, xbu

    ** prediction of FE (alphai_hat)
        predict hre_alphai, u

   ** prediction of idiosyncratic error term (e_hat)
        predict hre_e, e

    ** prediction of FE and idiosyncratic error term (alphai_hat + e_hat)
        predict hre_alphai_e, ue
        
list cid year y hre_xb hre_xb_alphai hre_alphai hre_e in 1/20

	
** Test on random effects

    xttest0

** RE estimation with heteroskedasticity-robust standard errors:

    xtreg y kbc30 lwdi secondary egc mlines troads cells proads rails, re robust
	
	
*-----------------
* Dynamic models
*-----------------

********
** IV **
********

* Robust SE's using standard IV using DELTA(y_t-2) as instrument
ivregress 2sls D.y D.kbc30 D.lwdi D.secondary D.egc D.mlines D.troads D.cells D.proads D.rails (L.D.y = L2.D.y)
ivregress 2sls D.y D.kbc30 D.lwdi D.secondary D.egc D.mlines D.troads D.cells D.proads D.rails (L.D.y = L2.D.y), robust


*******
* GMM *
*******
* Two-step + finite sample correction + predetermined variables + laglimit = 3
xtabond2 y L.y kbc30 lwdi secondary egc mlines troads cells proads rails year, gmm(L.y) gmm(kbc30, laglimits(1 3)) gmm(lwdi, laglimits(1 3)) gmm(secondary, laglimits(1 3)) gmm(egc, laglimits(1 3)) gmm(mlines, laglimits(1 3)) gmm(troads, laglimits(1 3)) gmm(cells, laglimits(1 3)) gmm(proads, laglimits(1 3)) gmm(rails, laglimits(1 3)) iv(year) noleveleq twostep robust

log close
