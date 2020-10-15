********************************************************************************
* 							 Problem Set 2 Review                 		       *
*								October 14, 2020							   *
********************************************************************************

		
********************************************************************************
* 									PROBLEM ONE:                               *
*                                                                              *
********************************************************************************
eststo clear
use "$data/pointonepctsampleE.dta", clear
*===========*
*   PART A  *
*===========*

//Define "three binary outcomes": should be five (50, 75, 100, 125, 150)

foreach j of numlist    50 75 100 125 150  {
	cap drop underp`j'
	g underp`j' = poverty<`j'
}


*i. DiD regression - Place and time fixed effects you need for a basic DID
g post = year==2006
g lnMW = ln(minwage)

	areg underp50  lnMW  post , cluster(state) absorb(state)

*ii. Add listed controls EXCEPT Bad controls: Be sure to justify which controls are "bad"
*(justify the ones you use and the ones you exclude as "bad")

/*iii. Division - specific effect: See slides 21-28 in lecture 7,8,9, slides :
	controlling for trend differences by interacting between dividion (census
	region divisions) and time
	
	use these with i and ii above

*/

xi i.division*post, pre(_d)
egen d_div_time = group(d_2006 division)




/*Produce a ``journal style'' (latex) table where the columns are the four 
different specifications and the rows are the five different cutoffs.

You should addnotes to the bottom of the table explaining the specifications.

In your writeup  be sure to throuoghly explain the intreptation of the results 
and the different identifying assumptions

How do the results vary accross specifications? How do the results vary across
poverty cutoffs?
*/




*===========*
*   PART B  *
*===========*

* NOTE: we haven't yet covered this material in class yet, we will cover quantile
* regressions on Monday Oct 19 and W October 21

/* i. distributional regressions: regressions on poverty cutoffss like above 
but now defining poverty cutoffs between 0 - 250 in increments of 25

*/
	
areg underp50 lnMW  i.d_div_time ///
		age age2 sex married i.racesingd i.citizen hieduc ///	
		, a(statefips) vce(cluster statefips)


scalar b_50 = _b[lnMW]

*make graph using coefplot




/* ii. 

Use kdensity function to find density at poverty cutoffs then define your rifreg calc as:

((-1)*b_`poverty_threshold')/Density_`poverty_threshold'
	
[hint be sure to store your betas in part i above so you can use them here]
	
	
 Then use the rifreg command to compare your results: 
	
	https://faculty.arts.ubc.ca/nfortin/datahead.html
	

NOTES: 
	* your calculations and rifreg may not exactly align but should be close (+-2) 	
	* rifreg does not allow factor vairables or interactions - create dummies or 
	use prefix xi: 

*/
 	xi: rifreg lnMW age sex i.married i.racesingd i.citizen hieduc _d*, q(`quantile')


// You should have:
		* graph of coefficients w/ confidence bands (part i)
		* table comparing your calcs to rifreg command (ii)
		* plot of the rifreg coefficients (ii)
		* intrepretation for all parts!
		
		
********************************************************************************
* 									PROBLEM TWO:                               *
*                                                                              *
********************************************************************************
eststo clear
use  "$data/emp_wage_data.dta",clear
	

* Focusing on 1982q1 - 1990q1 period
drop if quarterdate>=121 | quarterdate<=87	
	
*NOTE: using loops will be important for this problem as you perform the same analysis
*on multiple outcome variables (logwages_teen logwages_overall logemp_teen logemp_overall)	

*===========*
*   PART A  *
*===========*

* first, identify all states that did NOT raise their minwage in this period: 
*these are the "donor" states.	Then you want to keep just treated (CA) and donors
* for analysis

//i. Basic DiD
	*define treated state
	*define post period
	*define treatment (postXtreat)



*Show figures with the difference between CA and control states over time using quarterly bins.
* explain figure - what do we see about the trends? explain.

* create quarterly bins
g period_4q = floor((quarterdate-114)/4)


collapse (mean) meanemp=lnteenemp, by(treatsample quarterdate)

twoway (line  meanemp quarterdate if treatsample==1) || ///
(line  meanemp quarterdate if treatsample==0)

//ii. How sensitive are estimates to control for pre-exisiting trends? - use lead/lag variables

//iii.  Similar to what we did in PS1 - use pre-t averaged covariates and logit
* look at the weights, which states get the most?

//iv. Conley-Taber: necessary b/c we have only 1 treated unit.

 // can base code off of ga_ew.do available at https://www.ssc.wisc.edu/~ctaber/DD/diffdiff.html;
	// use https://www.ssc.wisc.edu/~ctaber/742/contab12.pdf to adjust for our purposes

	
*have table showing results for all vars and specifications. and interpret results.	
	
*===========*
*   PART B  *
*===========*
	*ssc install synth

/*Synthetic controls:
• All pre-treatment quarterly outcomes
• Averaged pre-treatment outcomes, and also pre-treatment period averaged:
– industry shares of employment
– demographic shares
*/



*i. 

synth teen_logwage    ///
  teen_logwage(92) teen_logwage(94(1)110) teen_logwage(98(2)101)  teen_logwage(102&104&106) ///
  teen_logwage teen_logwage(110(1)113) , trunit(6) trperiod(114) keep("$data/example_synth.dta", replace) 

  
/*
teen_logwage(92): the value of teen_logwage in the quarter 92 is entered as a predictor.

teen_logwage(102&104&106): the value of teen_logwage averaged over the quarters 102, 104, and 106 is
   entered as a predictor.

teen_logwage(94(1)110): the value of teen_logwage averaged over the quarters 94,95,...,110 is entered as
	a predictor.
			
teen_logwage: since no variable specific period is provided, the value of the variable X4
	is averaged either over the entire pretreatment period (default) or the period 
	specified in xperiod(numlist) and then enteredas a predictor.
		

trunit(#) the unit number of the unit affected by the intervention
  
trperiod(#) the time period when the intervention occurred.
  
keep(filename) saves a dataset with the results in the file filename.dta

 lnteenwage(92(1)113) race_share1 race_share2 race_share3 hispanic_share ///
  emp_sh_ind1 emp_sh_ind2 emp_sh_ind3 emp_sh_ind4 emp_sh_ind5 emp_sh_ind6 emp_sh_ind7 emp_sh_ind8 emp_sh_ind9 
  
 */
 

 use "$data/example_synth.dta", clear
 * show table of state names and weights

 
 cap drop period_4q time
g time = floor((_time-114))
*g time = floor((_time-114)/4)


* ii. show how well synthetic Ca matches real Ca - this is similar to graph we made
* in part A showing parallel trends b/e Ca and donors. this time b/w CA and synthetic CA


collapse (mean) _Y_treated _Y_synthetic , by(time)
twoway scatter _Y_syn _Y_treated  time, mcolor(red green) || ///
 line  _Y_syn _Y_treated  time, lcolor(red green) lpat(solid solid) scheme(plotplain) ///
 legend(order(1 "Synthetic CA" 2 "CA")) xtitle(Event time) xline(0, lcolor(gs7)) ///
 ytitle(log teen wage)
 
 

 
g dif = _Y_treated - _Y_synthetic 

twoway scatter dif  time, mcolor(blue) || ///
 line  dif time, lcolor(blue) lpat(solid ) scheme(plotplain) ///
 legend(order(1 "CA - Synthetic CA" )) xtitle(Event time) xline(0, lcolor(gs7)) ///
 ytitle(log teen wage) ylabel(-0.05(.05)0.1)

 
 

/*iii. randomization inference:
 placebo treatment effects where the effect should be zero (basically pretend 
the min wage increase happened in the other donor states) these cna be used to 
construct a counter-factual distribution of treatment effects
 
 
 have data set of all the donor states, and loop through and say the "true unit"
 is that donor state
 
 then make a graph showing all thats "treatment effects" for the donors make these 
 light grey and make CA in dark.
  */

  
 /*iv. Use 1982q1-1986q2 as the "training period" and 1986q3-1988q2 as the "validation period"
 Based on the predictor values in the training period, the predictor weights are selected 
 to minimize the Mean Squared Prediction Error (MSPE) in the validation period. These weights
 are then used withpredictor data in the validation period to create the synthetic control
 
 seee mspeperiod(numlist) option in synth
 
 the again show graphs comparing synthetic CA to true CA - how do they compare now?
 */
