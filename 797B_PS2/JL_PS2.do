* Jesús Lara
* 797B Problem Set 2*
*********************



clear all
cd "C:\Users\User\Documents\GitHub\797b-problem-sets\797B_PS2"
eststo clear
use pointonepctsampleE, clear

**** PROBLEM ****
*****   1   *****
*****************

** PART 1A **
*************

//Define the 5 binary outcomes


foreach j of numlist    50 75 100 125 150  {
	cap drop I`j'
	g I`j' = poverty<`j'
}

** 1A i) **  // DID regression without covariates
g post = year==2006 // Generates a dummy equal to 1 if year is 2006. Including it= time effects
g lnMW = ln(minwage) 

// areg, regression with many dummy variables (doesn't show coefficients and I would guess it corrects for degrees of freedom for standard errors) In this case the large dummy are the individual effects (50 states), absorb 

** 1A ii) **

// Choose controls
// Bad controls
// Treatment: lnMW
// 1. "A control is bad when it is causally affected by treatment"
// 2. Colliders: "The control is affected by the treatment"
// Effect of School (treatment randomly assigned) on Height controlling for Income. The treatment (School) causally affects Income, the control. We are likely to find that school reduces height

//All controls:

// division: Not affected by treatment
//age: Not affected by treatment 
// sex: Not affected by treatment 
// married: Not affected by treatment
// racesingd: Not affected by treatment
// citizen: Not affected by treatment
// hieduc: Not affected by treatment | Probably it affects the decision of going to college?
// empstadt: Potentially causally affected by treatment 
// uhrswork: Potentially causally affected by treatment
// incwelfr: potentially causally affected by treatment

** 1A iii) **

// Now including division-specific effects

global X "age i.sex i.married i.racesingd i.citizen hieduc"

egen d_div_time = group(post division) // The variable d_div_time assigns the same value to a division interacted with time

// Estimation

foreach i of numlist 50 75 100 125 150 {

// (1) DID regression estimations without covariates
quiet areg I`i' lnMW post, cluster(state) absorb(state)
eststo  I`i'1

// (2) With covariates
quiet areg I`i' lnMW post age i.sex i.married i.racesingd i.citizen hieduc, cluster(state) absorb(state)
eststo I`i'2


// (3) Div without controls
quiet areg I`i'  lnMW  i.d_div_time, cluster(state) absorb(state) 
eststo I`i'3

// (4) Div with controls 
quiet areg I`i'  lnMW  i.d_div_time  $X, cluster(state) absorb(state) 
eststo I`i'4

if `i'== 50 {

esttab  I`i'1 I`i'2 I`i'3 I`i'4  using jeje3.tex, replace ty keep(lnMW) varlabels(lnMW " Income $ < 50\% $ Poverty Line ") nonum se noobs nonotes mlabels("(1)" "(2)" "(3)" "(4)") b(4) fragment
}

else{

esttab I`i'1 I`i'2 I`i'3 I`i'4  using jeje3.tex, append ty keep(lnMW) varlabels(lnMW "Income $ < `i'\% $ Poverty Line") nonum se noobs nonotes  mlabels(none)  b(4) fragment

}
}

** 1A iv) ** Make table:
/* When adding this to Latex: 


\begin{table}[htbp]\centering
\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
\caption{"Title of the Table" \label{auto}}
\begin{tabular}{l*{4}{c}}
\hline\hline
\input{tables:figures/jeje3}
\hline\hline
\multicolumn{5}{l}{\footnotesize "Explanation of the 4 different models"}\\
\end{tabular}
\end{table}

It works!
<3

*/ 

** PART 1B **
*************

** 1B i) **
eststo clear
foreach j of numlist    25(25)250  {
	cap drop I`j'
	g I`j' = poverty<`j'
}

foreach i of varlist I*{
quiet areg `i' i.d_div_time lnMW  $X, absorb(statefips) cluster(state)
sca bMW_`i'=_b[lnMW] 
sca sebMW_`i'=_se[lnMW]
eststo r`i'
}
*coefplot rI*, ciopts(recast(rarea) color(gs13)) vertical keep(lnMW) swapnames scheme(s1color) yline(0) title ("Conditional quantile partial effects") nolabel

*coefplot rI*, ciopts(recast(rarea)) vertical keep(lnMW) swapnames scheme(s1color) yline(0) title ("Conditional quantile partial effects") nolabel

coefplot (rI25 \ rI50 \ rI75\ rI100\ rI125 \ rI150 \rI175\ rI200\ rI225\ rI250, lcolor(black) mcolor(black)), vertical keep(lnMW)  aseq swapnames scheme(s1color)  title ("Conditional quantile partial effects") ciopts(recast(rarea) color(gs13))  recast(connected) yline(0, lcolor(black)) xtitle(Poverty Cutoffs) ytitle(Estimated lnMW Coefficient)

graph export "1Bi.png", as(png) replace

//recast ciops 
// mat b with estimates 
// ciopts(recast(rarea) color(gs13))


** 1B ii) **

/// Own calculations

cap drop cutoffs
gen cutoffs=.

forvalues i= 1/10 {
quietly replace cutoffs= 25*`i' in `i'
}

cap drop x
cap drop d
kdensity poverty, gen(x d) at(cutoffs) // Obtain the pdf at the cutoffs

cap drop bq // Coefficients obtained by OLS
gen bq=. 
forvalues i=25(25)250{

quiet replace bq=bMW_I`i' if cutoffs==`i'
}

cap drop b_upe
gen b_upe= bq/(-1*d)  //  b_upe are my own calculations 
// Now with the package rifreg 

** 1B ii) **

su poverty
sca N=r(N)
forvalues i=25(25)250{
quiet su I`i' if poverty<`i' & poverty!=.
local quant = round(r(N)/N, 0.01) 

quiet xi: rifreg poverty lnMW  i.d_div_time i.statefips $X, q(`quant')
sca brif_`i'=_b[lnMW]
eststo RI`i'

}

cap drop brif
gen brif=.
forvalues i=25(25)250{
quiet replace brif=brif_`i' if cutoffs==`i'
}

coefplot (RI25 \ RI50 \ RI75\ RI100\ RI125 \ RI150 \RI175\ RI200\ RI225\ RI250, lcolor(black) mcolor(black)), vertical keep(lnMW) aseq swapnames scheme(s1color) yline(0, lcolor(black)) title ("Unconditional quantile partial effects") ciopts(recast(rarea) color(gs13)) recast(connected) xtitle(Poverty Cutoffs) ytitle(Estimated lnMW Coefficient)

graph export "1Bii.png", as(png) replace

mkmat d bq b_upe brif if d!=., mat(Table_1Bii) 
mat list Table_1Bii
mat colnames Table_1Bii= "PDF" "Conditional" "Own Calculations" "Rifreg"
mat rownames Table_1Bii= "25" "50" "75" "100" "125" "150" "175" "200" "225" "250"


esttab m(Table_1Bii,fmt(%9.4f)) using "Table_1Bii.tex", replace title(Conditional and unconditional qunatile partial effects) nomtitles booktabs

// Make table to present my results



**** PROBLEM ****
*****   2   *****
*****************


**** PROBLEM ****
*****   2   *****
*****************

use emp_wage_data,clear
gen time= year+(qtr-1)/4

keep if time >=1982 & time <=1990
** PART 2A **
*************
// Identify treatment and donor states
* First Visual Inspection
//separate MW, by(statenum)
// xtline MW
//Now formally
sum statenum
local min=r(min)
local max=r(max)
gen donor=.
forvalues i=`min'/`max'{
	quietly sum MW if statenum==`i'
		local minMW=r(min)
		local maxMW=r(max)
	if `minMW'==`maxMW'{
		quietly replace donor=1 if(statenum==`i')
		}
	else{
	 di "`i' -- Not a donor"
		}
	}
//California is num 6
drop if donor!=1& stateabb!="CA"

// Treatment variables
gen post=0
replace post=1 if(time>=1988.75) 

/// post: post treatment
gen CA=0
replace CA=1 if (stateabb=="CA") 

/// CA: treated
gen CA_post=post*CA  
bys statenum: gen date=_n
/// CA_post: treatment

** 2A i) **
//Gen logs of dependent variables
gen overall_logemp = ln(overall_emp) 
gen teen_logemp=ln(teen_emp)

// Our 4 outcome variables: teen_logwage, overall_logwage, overall_logemp, teen_logemp
g period_4q = floor((quarterdate-114)/4)

save "emp_wage_data_normal.dta", replace

foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
preserve 
collapse (mean) `var', by(CA period_4q)

if "`var'"=="teen_logwage"{
	local title ="Wage: Teen"
	local ytitle= "log(wage)"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall"
	local ytitle="log(wage)"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen"
	local ytitle="log(employment)"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall"
	local ytitle="log(employment)"
	}
else{
}


twoway (connected `var' period_4q if CA==1)  ///
(connected  `var' period_4q if CA==0, lcolor(blue) mcolor(blue)), xline(0, lcolor(black)) scheme(s1color)  xtitle("Event Time") ytitle(`ytitle') title(`title') legend(order(1 "CA" 2 "Donor states"))
graph export "2Ai`var'.png", as(png) replace
restore
}





//DD regressions
xtset statenum quarterdate



foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
	quiet areg `var' CA_post i.quarterdate, absorb(statenum) cluster(statenum)
		eststo DD`var'
}

//Personalize formating
*esttab using 2ai.tex, replace keep(CA_post) nonum se noobs mlabels("Wage (Teen)" "Wage (Overall)" "Employment (Teen)" "Employment (Overall)") title(Overall Difference in Difference\label{auto}) b(3) coef(CA_post "diff-in-diff")	

** 2A ii) **


///Checking lags and leads to see if there are pre-treatment trends: (that is, this is just just detecting pre-existing trends, not controlling for it, no?)
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
quiet reghdfe `var' CA_post L3.CA_post L2.CA_post L1.CA_post f1.CA_post f2.CA_post f3.CA_post i.quarterdate, cluster(statenum) absorb(statenum)
eststo L1`var'
}


xi i.statenum*quarterdate, pre(_d)   
*xi just creates dummies with the categorical values of `division' multiplied for the values of post; pre(_d) is just choosing a prefix for each of these dummies created.


///trying to control for trend differences (see slide 21 lecture 7  9) - Adding state*time fixed effect.
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
quiet reghdfe `var' CA_post L3.CA_post L2.CA_post L1.CA_post  f1.CA_post f2.CA_post f3.CA_post  _d*, cluster(statenum) absorb(statenum)
eststo L2`var'


}


foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
quiet reg `var' CA_post i.quarterdate l3.`var' l2.`var' l1.`var', cluster(statenum)
eststo L3`var'
}

//What happens if we control for pre-existing linear and cuadratic trends?

** 2A iii) **
//Propensity Score Reweighting

global covariates "race_share1 race_share2 race_share3 hispanic_share age_group_sh1 age_group_sh2 age_group_sh3 age_group_sh4 age_group_sh5 age_group_sh6"

gen pretreated = 0
replace pretreated = 1 if statenum==6 & quarterdate<114 
/// California Pretreatment

foreach j of global covariates {
egen av`j' = mean(`j') if inrange(quarterdate,88,113) , by(statenum)
egen av`j'post = mean(`j') if inrange(quarterdate,114,120) , by(statenum)
}

foreach j of global covariates {
replace av`j'post=f1.av`j'post
}

foreach j of global covariates {
	
gen di`j' =  av`j' - av`j'[5]
gen dii`j' =  av`j'post - av`j'post[5]

g d`j' =  di`j' - dii`j'
drop di`j' dii`j'

egen dmean`j' = sum(abs(d`j'))
}
save "emp_wage_data2Aii.dta",replace


global precovariates "avrace_share1 avrace_share2 avrace_share3 avhispanic_share avage_group_sh2"

logit pretreated $precovariates 
predict treatmentprop

bysort statenum: replace treatmentprop = l1.treatmentprop if treatmentprop==.

/// Generate variable of weights:

qui sum treatmentprop
local p = r(mean)
cap drop w2Aiii
gen w2Aiii = cond(pretreated==1,1, treatmentprop/(1-treatmentprop))

preserve
collapse (mean) w2Aii, by(stateabb)
drop if stateabb=="CA"
mkmat w2Aii, mat(w2Aii)
mat colnames w2Aii= "Weights Logit"
mat rownames  w2Aii="AL" "AK" "AZ" "AR" "CO" "DE" "FL" "GA" "ID" "IL" "IN" "KS" "KY" "LA" "MD" "MI" "MS" "MO" "MT" "NE" "NV" "NJ" "NM" "NY" "NC" "OH" "OK" "SC" "SD" "TN" "TX" "UT" "VA" "WV" "WY"
mat list w2Aii
esttab m(w2Aii, fmt(%9.4f)) using w2Aii.tex, replace title(Weights to Each State: Logit) nomtitles booktabs



restore



// Now run the regression using these weights 

foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{

quiet areg `var' CA_post i.quarterdate [aw=w2Aiii], absorb(statenum) robust
eststo PSR`var'
}
//Own calculations








*******************************************
***************** 2A) iv) *****************
*******************************************

use emp_wage_data,clear
gen time= year+(qtr-1)/4

keep if time >=1982 & time <=1990
** PART 2A **
*************
// Identify treatment and donor states
* First Visual Inspection
//separate MW, by(statenum)
// xtline MW
//Now formally
sum statenum
local min=r(min)
local max=r(max)
gen donor=.
forvalues i=`min'/`max'{
	quietly sum MW if statenum==`i'
		local minMW=r(min)
		local maxMW=r(max)
	if `minMW'==`maxMW'{
		quietly replace donor=1 if(statenum==`i')
		}
	else{
	 di "`i' -- Not a donor"
		}
	}
//California is num 6
drop if donor!=1& stateabb!="CA"

// Treatment variables
gen post=0
replace post=1 if(time>=1988.75) 

/// post: post treatment
gen CA=0
replace CA=1 if (stateabb=="CA") 

/// CA: treated
gen CA_post=post*CA  

/// CA_post: treatment

** 2A i) **
//Gen logs of dependent variables
gen overall_logemp = ln(overall_emp) 
gen teen_logemp=ln(teen_emp)

// Our 4 outcome variables: teen_logwage, overall_logwage, overall_logemp, teen_logemp
g period_4q = floor((quarterdate-114)/4)


foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
preserve 
collapse (mean) `var', by(CA period_4q)

if "`var'"=="teen_logwage"{
	local title ="Wage: Teen"
	local ytitle= "log(wage)"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall"
	local ytitle="log(wage)"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen"
	local ytitle="log(employment)"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall"
	local ytitle="log(employment)"
	}
else{
}


twoway (connected `var' period_4q if CA==1)  ///
(connected  `var' period_4q if CA==0, lcolor(blue) mcolor(blue)), xline(0, lcolor(black)) scheme(s1color)  xtitle("Event Time") ytitle(`ytitle') title(`title') legend(order(1 "CA" 2 "Donor states"))
graph export "2Ai`var'.png", as(png) replace
restore
}





//DD regressions
xtset statenum quarterdate



foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
	quiet areg `var' CA_post i.quarterdate, absorb(statenum) cluster(statenum)
		eststo DD`var'
}

//Personalize formating
*esttab using 2ai.tex, replace keep(CA_post) nonum se noobs mlabels("Wage (Teen)" "Wage (Overall)" "Employment (Teen)" "Employment (Overall)") title(Overall Difference in Difference\label{auto}) b(3) coef(CA_post "diff-in-diff")	

** 2A ii) **


///Checking lags and leads to see if there are pre-treatment trends: (that is, this is just just detecting pre-existing trends, not controlling for it, no?)
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
quiet reghdfe `var' CA_post L3.CA_post L2.CA_post L1.CA_post f1.CA_post f2.CA_post f3.CA_post i.quarterdate, cluster(statenum) absorb(statenum)
eststo L1`var'
}


xi i.statenum*quarterdate, pre(_d)   
*xi just creates dummies with the categorical values of `division' multiplied for the values of post; pre(_d) is just choosing a prefix for each of these dummies created.


///trying to control for trend differences (see slide 21 lecture 7  9) - Adding state*time fixed effect.
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
quiet reghdfe `var' CA_post L3.CA_post L2.CA_post L1.CA_post  f1.CA_post f2.CA_post f3.CA_post  _d*, cluster(statenum) absorb(statenum)
eststo L2`var'


}


foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
quiet reg `var' CA_post i.quarterdate l3.`var' l2.`var' l1.`var', cluster(statenum)
eststo L3`var'
}

//What happens if we control for pre-existing linear and cuadratic trends?

** 2A iii) **
//Propensity Score Reweighting

global covariates "race_share1 race_share2 race_share3 hispanic_share age_group_sh1 age_group_sh2 age_group_sh3 age_group_sh4 age_group_sh5 age_group_sh6"

gen pretreated = 0
replace pretreated = 1 if statenum==6 & quarterdate<114 
/// California Pretreatment

foreach j of global covariates {
egen av`j' = mean(`j') if inrange(quarterdate,88,113) , by(statenum)
egen av`j'post = mean(`j') if inrange(quarterdate,114,120) , by(statenum)
}

foreach j of global covariates {
replace av`j'post=f1.av`j'post
}

foreach j of global covariates {
	
gen di`j' =  av`j' - av`j'[5]
gen dii`j' =  av`j'post - av`j'post[5]

g d`j' =  di`j' - dii`j'
drop di`j' dii`j'

egen dmean`j' = sum(abs(d`j'))
}
save "emp_wage_data2Aii.dta",replace


global precovariates "avrace_share1 avrace_share2 avrace_share3 avhispanic_share avage_group_sh2"

logit pretreated $precovariates 
predict treatmentprop

bysort statenum: replace treatmentprop = l1.treatmentprop if treatmentprop==.
/// Generate variable of weights:
qui sum treatmentprop
local p = r(mean)
cap drop w2Aiii
gen w2Aiii = cond(pretreated==1,1, treatmentprop/(1-treatmentprop))
preserve
collapse (mean) w2Aii, by(stateabb)
drop if stateabb=="CA"
mkmat w2Aii, mat(w2Aii)
mat colnames w2Aii= "Weights Logit"
mat rownames  w2Aii="AL" "AK" "AZ" "AR" "CO" "DE" "FL" "GA" "ID" "IL" "IN" "KS" "KY" "LA" "MD" "MI" "MS" "MO" "MT" "NE" "NV" "NJ" "NM" "NY" "NC" "OH" "OK" "SC" "SD" "TN" "TX" "UT" "VA" "WV" "WY"
mat list w2Aii
esttab m(w2Aii, fmt(%9.4f)) using w2Aii.tex, replace title(Weights to Each State: Logit) nomtitles booktabs
restore

// Now run the regression using these weights 

foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{

quiet areg `var' CA_post i.quarterdate [aw=w2Aiii], absorb(statenum) robust
eststo PSR`var'
}


****************
****** Conley Taber

use emp_wage_data_normal, clear

//  https://www.ssc.wisc.edu/~ctaber/DD/diffdiff.html https://www.ssc.wisc.edu/~ctaber/742/contab12.pdf 
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
	/* Create dummies for state+year interactions*/
		egen Dstyr=group(date statenum)
		reg `var' i.Dstyr //not using controls like example (asian etc.)

		predict beta, xb

	/* Predict residuals from regression */
		quietly reg beta CA_post i.statenum i.date, noc
			quietly predict `var'_res, res
			quietly replace `var'_res=`var'_res+_b[CA_post]*CA_post

		/* Create d tilde variable*/
		quietly bysort date: egen `var'_djtCA=mean(CA_post) if CA==1
		quietly bysort date: egen `var'_djt=sum(`var'_djtCA) 
		quietly bysort statenum: egen `var'_meandjt=mean(`var'_djt)
		qui: g `var'_dtil=`var'_djt-`var'_meandjt

		/* Obtain difference in differences coefficient*/
		qui: reg `var'_res `var'_dtil if CA==1, noc
		matrix `var'_alpha=e(b)

		/* Simulations*/
			foreach i of numlist 1/56 {
				capture {
				reg `var'_res `var'_dtil if statenum==`i' & CA !=1, noc
				matrix `var'_alpha = `var'_alpha\e(b)
			}
			}
		

		matrix `var'_asim = `var'_alpha[2...,1]
		matrix `var'_alpha = `var'_alpha[1,1]

		/* Confidence intervals */
		qui: svmat `var'_alpha 
		qui: svmat `var'_asim

		qui: sum `var'_alpha
		qui: gen `var'_alpha=r(mean)
		qui: g `var'_ci=`var'_alpha - `var'_asim

		/* form confidence intervals */
		local numst=36
		local i05=floor(0.050*(`numst'-1))
		local i95=ceil(0.950*(`numst'-1))

	quietly sum `var'_alpha
		display as text "Difference in Differences coefficient=" as result _newline(2) r(mean)
			scalar `var'_DiD = r(mean)

		di " "	
		di " "

		sort `var'_asim
		quietly sum `var'_ci if _n==`i05'|_n==`i95' 
		display as text "90% Confidence interval=" as result _newline(2) r(min) _col(15) r(max)
			scalar `var'_LCI = r(min)
			scalar `var'_UCI = r(max)
		cap drop beta Dstyr
}

sum teen_logwage*_ci 
	local 1_min=r(min)
	local 1_max=r(max)
sum teen_logemp*_ci 
	local 2_min=r(min)
	local 2_max=r(max)
sum overall_logwage*_ci 
	local 3_min=r(min)
	local 3_max=r(max)
sum overall_logemp*_ci 
	local 4_min=r(min)
	local 4_max=r(max)
mat confidence=(`1_min',`1_max'\ `2_min',`2_max'\ `3_min',`3_max'\ `4_min',`4_max')
mat rown confidence= "Wage(Teen)" "Emp (Teen)" "Wage (Overall)" "Emp (Overall)" 
mat coln confidence= "LowerBound(90\%)" "UpperBound(90\%)"
mat list confidence

outtable using "table2aiv.tex", mat(confidence) replace
esttab m(confidence, fmt(%9.3f)) using "table2aiv.tex", replace title(Confidence Interval with Conley Taber's method) nomtitles booktabs


** PART 2B **
*************

// Synthetic controls

** 2B i) **
//S1: All quarterly pretreatment outcomes
 
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {

quiet synth `var' ///
`var'(88) `var'(89) `var'(90) `var'(91) /// 
`var'(92) `var'(93) `var'(94) `var'(95) `var'(96) ///
`var'(97) `var'(98) `var'(99) `var'(100) ///
`var'(101) `var'(102) `var'(103) ///
`var'(104) `var'(105) `var'(106) `var'(107) /// 
`var'(108) `var'(109) `var'(110) `var'(111) ///
`var'(112) `var'(113), trunit(6) trperiod(114) keep("spec1_`var'.dta", replace) 

preserve

use spec1_`var', clear 
gen CA_diff_`var'=_Y_synthetic-_Y_treated
rename _W_Weight s1w_`var'
sa spec1_`var', replace 

gen period_4q = floor((_time-114)/4)
collapse (mean) _Y_treated (mean) _Y_synthetic, by(period_4q)

if "`var'"=="teen_logwage"{
	local title ="Wage: Teen"
	local ytitle= "log(wage)"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall"
	local ytitle="log(wage)"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen"
	local ytitle="log(employment)"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall"
	local ytitle="log(employment)"
	}
else{
}
/// More formating
twoway (connected _Y_treated period_4q) (connected _Y_synthetic period_4q, lcolor(blue)  mcolor(blue)), xline(0)  scheme(s1color) xtitle("Event Time")  title(`title') ytitle(`ytitle') legend(order(1 "CA" 2 "Synthetic CA"))
graph export "2Bi`var'_spec1.png", as(png) replace

gen Yt_minus_Ys=_Y_treated-_Y_synthetic

twoway (connected Yt_minus_Ys period_4q, lcolor(red)  mcolor(red)), xline(0) yline(0) scheme(s1color) xtitle("Event Time") title(`local') ytitle(Gap between real and synthetic CA)
graph export "2Bi`var'dif_spec1.png", as(png) replace
restore 
}

///////////////////////////////////////////
//S2: All quarterly pretreatment outcomes
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {

quiet synth `var' `var'(88(1)113) race_share1 race_share2 race_share3 hispanic_share emp_sh_ind1 emp_sh_ind2 emp_sh_ind3 emp_sh_ind4 emp_sh_ind5 emp_sh_ind6 emp_sh_ind7 emp_sh_ind8 emp_sh_ind9, trunit(6) trperiod(114) keep("spec2_`var'.dta", replace)

preserve

use spec2_`var', clear 
rename _W_Weight s2w_`var'
gen CA_diff_`var'=_Y_synthetic-_Y_treated
sa spec2_`var', replace 

gen period_4q = floor((_time-114)/4)
collapse (mean) _Y_treated (mean) _Y_synthetic, by(period_4q)


/// Locals for formatting 
if "`var'"=="teen_logwage"{
	local title ="Wage: Teen"
	local ytitle= "log(wage)"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall"
	local ytitle="log(wage)"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen"
	local ytitle="log(employment)"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall"
	local ytitle="log(employment)"
	}
else{
}


/// Graph 1: CA vs Synthetic CA
twoway (connected _Y_treated period_4q) (connected _Y_synthetic period_4q, lcolor(blue) mcolor(blue)), xline(0)  scheme(s1color) title(`title') ytitle(`ytitle') xtitle("Event Time") legend(order(1 "CA" 2 "Synthetic CA"))
graph export "2Bi`var'_spec2.png", as(png) replace

/// Graph 2: Difference between the two
gen Yt_minus_Ys=_Y_treated-_Y_synthetic
twoway (connected Yt_minus_Ys period_4q, lcolor(red) mcolor(red)), xline(0) yline(0) scheme(s1color) xtitle("Event Time")  title(`local') ytitle(Gap between real and synthetic CA)
graph export "2Bi`var'dif_spec2.png", as(png) replace

/// Estimate of the effect:

restore 
}

//GET ESTIMATES (alpha)
preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use spec1_`var', clear
drop if _time==.
gen post=0
replace post=1 if _time>114
su CA_diff_`var' if post==1
sca S1`var'=r(mean)
}
restore

preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use spec2_`var', clear
drop if _time==.
gen post=0
replace post=1 if _time>114
su CA_diff_`var' if post==1
sca S2`var'=r(mean)
}
restore

mat SC_est = (S1teen_logwage, S1overall_logwage, S1teen_logemp, S1overall_logemp\ S2teen_logwage, S2overall_logwage, S2teen_logemp, S2overall_logemp)
mat list SC_est
mat rownames SC_est= "Specification 1" "Specification 2"
mat colnames SC_est= " Wage (Teen)" "Wage (Overall)" "Employment (Teen)" "Employment (Overall)"
esttab m(SC_est, fmt(%9.3f)) using "2BiiSC_Estimates.tex", replace title(Synthetic Controls Estimates) nomtitles booktabs


// Make the table of weights 
preserve 
use spec1_overall_logwage, clear 
merge 1:1 _Co_Number using spec2_overall_logwage, nogen
merge 1:1 _Co_Number using spec1_teen_logwage, nogen
merge 1:1 _Co_Number using spec2_teen_logwage, nogen
merge 1:1 _Co_Number using spec1_overall_logemp, nogen
merge 1:1 _Co_Number using spec2_overall_logemp, nogen
merge 1:1 _Co_Number using spec1_teen_logemp, nogen
merge 1:1 _Co_Number using spec2_teen_logemp, nogen
drop _time _Y_treated _Y_synthetic CA*
rename _Co_Number statenum
save "weights.dta",replace
*merge 1:1 statenum using statelabels, keepusing(stateabb) nogen

mkmat statenum s1w_overall_logwage s2w_overall_logwage s1w_teen_logwage s2w_teen_logwage, mat(Weights_Wage)

mat colnames Weights_Wage ="State Number" "S1: Overall Wage" "S2: Overall Wage" "S1: Teen Wage" "S2: Teen Wage"  
mat rownames Weights_Wage="AL" "AK" "AZ" "AR" "CO" "DE" "FL" "GA" "ID" "IL" "IN" "KS" "KY" "LA" "MD" "MI" "MS" "MO" "MT" "NE" "NV" "NJ" "NM" "NY" "NC" "OH" "OK" "SC" "SD" "TN" "TX" "UT" "VA" "WV" "WY"
esttab m(Weights_Wage) using 2Bi_Weights_Wage.tex, replace title(Weights to Each State: Wage) nomtitles booktabs

mkmat statenum s1w_overall_logemp s2w_overall_logemp s1w_teen_logemp s2w_teen_logemp, mat(Weights_Emp)
mat colnames Weights_Emp ="State Number" "S1: Overall Emp" "S2: Overall Emp" "S1: Teen Emp" "S2: Teen Emp"
mat rownames Weights_Emp= "AL" "AK" "AZ" "AR" "CO" "DE" "FL" "GA" "ID" "IL" "IN" "KS" "KY" "LA" "MD" "MI" "MS" "MO" "MT" "NE" "NV" "NJ" "NM" "NY" "NC" "OH" "OK" "SC" "SD" "TN" "TX" "UT" "VA" "WV" "WY"

esttab m(Weights_Emp) using 2Bi_Weights_Emp.tex, replace title(Weights to Each State: Employment) nomtitles booktabs

restore 

** 2B ii)  **

//GET ESTIMATES (alpha)
preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use spec1_`var', clear
drop if _time==.
gen post=0
replace post=1 if _time>114
su CA_diff_`var' if post==1
sca S1`var'=r(mean)
}
restore

preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use spec2_`var', clear
drop if _time==.
gen post=0
replace post=1 if _time>114
su CA_diff_`var' if post==1
sca S2`var'=r(mean)
}
restore

mat SC_est = (S1teen_logwage, S1overall_logwage, S1teen_logemp, S1overall_logemp\ S2teen_logwage, S2overall_logwage, S2teen_logemp, S2overall_logemp)
mat list SC_est
mat rownames SC_est= "Specification 1 $\hat{\alpha}$" "Specification 2 $\hat{\alpha}$"
mat colnames SC_est= " Wage (Teen)" "Wage (Overall)" "Employment (Teen)" "Employment (Overall)"
esttab m(SC_est,fmt(%9.4f)) using "2BiiSC_Estimates.tex", replace title(Synthetic Controls Estimates) nomtitles booktabs



** 2B iii) **

//Placebo synthetic controls

 //statenum2 was generated after sropping observations of non-donors ==> has no breaks and it is more adequate for loops over states

// SPECIFICATION 2



save "emp_wage_data2biii.dta", replace // We will drop CA to facilitate loops but want to have this last version of the data for further exercises


drop if statenum==6 //drop california
egen statenum2=group(statenum) // list of donors without breaks 1-35

preserve  // Generate a dataset with statelabels 
keep if quarterdate==100
keep statenum statenum2 stateabb
save "statelabels.dta",replace
restore


xtset statenum2 quarterdate //necessary for synth


foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
forvalues i=1/35 {
quiet synth `var' `var'(88(1)113) race_share1 race_share2 race_share3 hispanic_share emp_sh_ind1 emp_sh_ind2 emp_sh_ind3 emp_sh_ind4 emp_sh_ind5 emp_sh_ind6 emp_sh_ind7 emp_sh_ind8 emp_sh_ind9, trunit(`i') trperiod(114) keep("S2_`var'_`i'.dta", replace)
preserve 

use S2_`var'_`i'.dta, clear 
gen diff_`i'=_Y_treated-_Y_synthetic   //gen diff, or gap
keep diff_`i' _time                   //keep relevant variables
save S2_`var'_`i', replace 
restore 
}
}

// Merge Datasets of SC specification 2


preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
forvalues i=1/35{

if `i'==1{ 

use  spec2_`var', clear // use our SC estimates for california 
keep _time CA_diff_`var' // keep relevant veriables
merge 1:1 _n using S2_`var'_`i', nogen  //merge with the SC estimates for state1
}


if `i'==35 {
merge 1:1 _n using S2_`var'_`i', nogen // merge with the remaining 34 states
drop if _time==.
save "S2_placebos_`var'.dta", replace 

// Make diagram // MORE FORMATTING NEEDED!

}
else {

merge 1:1 _n using S2_`var'_`i', nogen  //merge with the SC estimates for state1
}
}
}
restore 


// Placebo SPECIFICATION 1 
xtset statenum2 quarterdate 

foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
forvalues i=1/35 {
quiet synth `var' ///
`var'(88) `var'(89) `var'(90) `var'(91) /// 
`var'(92) `var'(93) `var'(94) `var'(95) `var'(96) ///
`var'(97) `var'(98) `var'(99) `var'(100) ///
`var'(101) `var'(102) `var'(103) ///
`var'(104) `var'(105) `var'(106) `var'(107) /// 
`var'(108) `var'(109) `var'(110) `var'(111) ///
`var'(112) `var'(113), trunit(`i') trperiod(114) keep("S1_`var'_`i'.dta", replace)

preserve 
use S1_`var'_`i'.dta, clear 
gen diff_`i'=_Y_treated-_Y_synthetic   //gen diff, or gap
keep diff_`i' _time                   //keep relevant variables
save S1_`var'_`i', replace 
restore 
}
}

// Merge Datasets of SC specification 1


preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
forvalues i=1/35{

if `i'==1{ 
use  spec1_`var', clear // use our SC estimates for california 
keep _time CA_diff_`var' // keep relevant veriables
merge 1:1 _n using S1_`var'_`i', nogen  //merge with the SC estimates for state1
}

if `i'==35 {
merge 1:1 _n using S1_`var'_`i', nogen // merge with the remaining 34 states
drop if _time==.
save "S1_placebos_`var'.dta", replace 
}
else {
merge 1:1 _n using S1_`var'_`i', nogen  //merge with the SC estimates for state1
}
}
}
restore 


/// Make figures here (S1)
preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use S1_placebos_`var', clear

if "`var'"=="teen_logwage"{
	local title ="Wage: Teen S1"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall S1"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen S1"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall S1"
	}
else{
}

line diff_1 _time, lcolor(dimgray)  || line diff_2 _time, lcolor(dimgray) || line diff_3 _time, lcolor(dimgray) || line diff_4 _time, lcolor(dimgray) || line diff_5 _time, lcolor(dimgray) || line diff_6 _time, lcolor(dimgray) || line diff_7 _time, lcolor(dimgray) || line diff_8 _time, lcolor(dimgray) || line diff_9 _time, lcolor(dimgray) || line diff_10 _time, lcolor(dimgray) || line diff_11 _time, lcolor(dimgray) || line diff_12 _time, lcolor(dimgray) || line diff_13 _time, lcolor(dimgray) || line diff_14 _time, lcolor(dimgray) || line diff_15 _time, lcolor(dimgray) || line diff_16 _time, lcolor(dimgray) || line diff_17 _time, lcolor(dimgray) || line diff_18 _time, lcolor(dimgray)  || line diff_19 _time, lcolor(dimgray)|| line diff_20 _time, lcolor(dimgray) || line diff_21 _time, lcolor(dimgray) || line diff_22 _time, lcolor(dimgray)  || line diff_23 _time, lcolor(dimgray) || line diff_24 _time, lcolor(dimgray) || line diff_25 _time, lcolor(dimgray) || line diff_26 _time, lcolor(dimgray) || line diff_27 _time, lcolor(dimgray) || line diff_28 _time, lcolor(dimgray) || line diff_29 _time, lcolor(dimgray) || line diff_30 _time, lcolor(dimgray) || line diff_31 _time, lcolor(dimgray) || line diff_32 _time, lcolor(dimgray) || line diff_33 _time, lcolor(dimgray) || line diff_34 _time, lcolor(dimgray) || line diff_35 _time, lcolor(dimgray) ||line CA_diff_`var' _time, lwidth (medthick) xtitle("Time") ytitle("Gap real-synthetic") lcolor(black) scheme(s1color) legend(off) yline(0) xline(114) title("GAP Synthetic and Real, CA vs Donor states:`var'") title(`title')  caption("Black line: California, Gray lines: donor states")

graph export "2Biii_ S1_`var'.png", as(png) replace

}
restore

/// Make figures here (S2)
preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use S2_placebos_`var', clear


if "`var'"=="teen_logwage"{
	local title ="Wage: Teen S2"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall S2"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen S2"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall S2"
	}
else{
}
	
line diff_1 _time, lcolor(dimgray)  || line diff_2 _time, lcolor(dimgray) || line diff_3 _time, lcolor(dimgray) || line diff_4 _time, lcolor(dimgray) || line diff_5 _time, lcolor(dimgray) || line diff_6 _time, lcolor(dimgray) || line diff_7 _time, lcolor(dimgray) || line diff_8 _time, lcolor(dimgray) || line diff_9 _time, lcolor(dimgray) || line diff_10 _time, lcolor(dimgray) || line diff_11 _time, lcolor(dimgray) || line diff_12 _time, lcolor(dimgray) || line diff_13 _time, lcolor(dimgray) || line diff_14 _time, lcolor(dimgray) || line diff_15 _time, lcolor(dimgray) || line diff_16 _time, lcolor(dimgray) || line diff_17 _time, lcolor(dimgray) || line diff_18 _time, lcolor(dimgray)  || line diff_19 _time, lcolor(dimgray)|| line diff_20 _time, lcolor(dimgray) || line diff_21 _time, lcolor(dimgray) || line diff_22 _time, lcolor(dimgray)  || line diff_23 _time, lcolor(dimgray) || line diff_24 _time, lcolor(dimgray) || line diff_25 _time, lcolor(dimgray) || line diff_26 _time, lcolor(dimgray) || line diff_27 _time, lcolor(dimgray) || line diff_28 _time, lcolor(dimgray) || line diff_29 _time, lcolor(dimgray) || line diff_30 _time, lcolor(dimgray) || line diff_31 _time, lcolor(dimgray) || line diff_32 _time, lcolor(dimgray) || line diff_33 _time, lcolor(dimgray) || line diff_34 _time, lcolor(dimgray) || line diff_35 _time, lcolor(dimgray) ||line CA_diff_`var' _time, lwidth (medthick) xtitle("Time") ytitle("Gap real-synthetic") lcolor(black) scheme(s1color) legend(off) yline(0) xline(114) title("GAP Synthetic and Real, CA vs Donor states:`var'") title(`title') caption("Black line: California, Gray lines: donor states")
graph export "2Biii_ S2_`var'.png", as(png) replace

}
restore

//Random Inference S1

use emp_wage_data2biii, clear
xtset statenum quarterdate

preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use S1_placebos_`var', clear
rename CA_diff_`var' diff_36 //Now state 36 is CALIFORNIA REMEMBER
forvalues i=1/36{
gen diff2_`i'=(diff_`i')^2
}
gen post=0
replace post=1 if _time>114
collapse (mean) diff*, by(post)

reshape long diff_ diff2_, i(post) j(statenum2)
reshape wide diff_ diff2_, i(statenum2) j(post)

forvalues i=0/1{
gen sqrtdiff2_`i'= sqrt(diff2_`i')

}
gen r=sqrtdiff2_1/sqrtdiff2_0
merge 1:1 statenum2 using statelabels, keepusing(stateabb) nogen
replace stateabb="CA" if statenum2==36
label values statenum2 stateabb



hdquantile r, p(5 10 90 95) matname(cutoffs)
local q5=cutoffs[1,1]
local q10=cutoffs[1,2]
local q90=cutoffs[1,3]
local q95=cutoffs[1,4]

set scheme s1color

if "`var'"=="teen_logwage"{
	local title ="Wage: Teen S1"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall S1"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen S1"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall S1"
	}
else{
}

graph dot (asis) r,  over (stateabb, sort(1)) yline(`q5' `q10' `q90' `q95', lcolor(black)) ysc(fex r(0 3)) marker(1, ms(Oh)) title(`title')
graph play 2Biii_editing // I stored an editing of my graph (2Biii_editing.)
graph export "2Biii_S1_inference_`var'.png", as(png) replace
}

restore

//Random Inference S1
///////////////////////
/// Specification 2////

preserve
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use S2_placebos_`var', clear
rename CA_diff_`var' diff_36 //Now state 36 is CALIFORNIA REMEMBER
forvalues i=1/36{
gen diff2_`i'=(diff_`i')^2
}
gen post=0
replace post=1 if _time>114
collapse (mean) diff*, by(post)

reshape long diff_ diff2_, i(post) j(statenum2)
reshape wide diff_ diff2_, i(statenum2) j(post)

forvalues i=0/1{
gen sqrtdiff2_`i'= sqrt(diff2_`i')

}
gen r=sqrtdiff2_1/sqrtdiff2_0
merge 1:1 statenum2 using statelabels, keepusing(stateabb) nogen
replace stateabb="CA" if statenum2==36
label values statenum2 stateabb



hdquantile r, p(5 10 90 95) matname(cutoffs)
local q5=cutoffs[1,1]
local q10=cutoffs[1,2]
local q90=cutoffs[1,3]
local q95=cutoffs[1,4]

set scheme s1color
if "`var'"=="teen_logwage"{
	local title ="Wage: Teen S2"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall  S2"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen S2"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall S2"
	}
else{
}

graph dot (asis) r,  over (stateabb, sort(1)) yline(`q5' `q10' `q90' `q95', lcolor(black)) ysc(fex r(0 3)) marker(1, ms(Oh)) title(`title')
graph play 2Biii_editing // I stored an editing of y grap

graph export "2Biii_S2_inference_`var'.png", as(png) replace
}
restore



** 2B iv) **
// Estimate the SC during 1983q1-1986q2 (92-105)
// Use 1986q3-1988q2 as validation  (106-113)
// 1988q3-1990q1: post period  (114-120)
use emp_wage_data2biii, clear
xtset statenum quarterdate

foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {

synth `var' ///
///`var'(88) `var'(89) `var'(90) `var'(91) /// 
`var'(92) `var'(93) `var'(94) `var'(95) `var'(96) ///
`var'(97) `var'(98) `var'(99) `var'(100) ///
`var'(101) `var'(102) `var'(103) ///
`var'(104) `var'(105) `var'(106) `var'(107) /// 
`var'(108) `var'(109) `var'(110) `var'(111) ///
`var'(112) `var'(113), trunit(6) mseperiod(106(1)113) trperiod(114) keep("validS1_`var'.dta", replace)  
preserve

use validS1_`var', clear 
gen period_4q = floor((_time-114)/4)
collapse (mean) _Y_treated (mean) _Y_synthetic, by(period_4q)
/// Locals for formatting 
if "`var'"=="teen_logwage"{
	local title ="Wage: Teen | Validation 1986q3-1990q1"
	local ytitle= "log(wage)"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall | Validation 1986q3-1990q1"
	local ytitle="log(wage)"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen | Validation 1986q3-1990q1"
	local ytitle="log(employment) "
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall | Validation 1986q3-1990q1"
	local ytitle="log(employment)"
	}
else{
}

/// Graph Validation 
twoway (connected _Y_treated period_4q) (connected _Y_synthetic period_4q, lcolor(blue) mcolor(blue)), xline(0)  scheme(s1color) title(`title') ytitle(`ytitle') xtitle("Event Time") legend(order(1 "CA" 2 "Synthetic CA"))
graph export "2Bv`var'_spec1.png", as(png) replace
restore
}

//Method two: All quarterly pretreatment outcomes
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {

synth `var' `var'(92(1)113) race_share1 race_share2 race_share3 hispanic_share emp_sh_ind1 emp_sh_ind2 emp_sh_ind3 emp_sh_ind4 emp_sh_ind5 emp_sh_ind6 emp_sh_ind7 emp_sh_ind8 emp_sh_ind9, trunit(6) trperiod(114) mseperiod(106(1)113) xperiod(92(1)113) keep("validS2_`var'.dta", replace)

preserve

use validS2_`var', clear 
gen period_4q = floor((_time-114)/4)
collapse (mean) _Y_treated (mean) _Y_synthetic, by(period_4q)
/// Locals for formatting 
if "`var'"=="teen_logwage"{
	local title ="Wage: Teen | Validation 1986q3-1990q1"
	local ytitle= "log(wage)"
	}
	if "`var'"=="overall_logwage"{
	local title ="Wage: Overall | Validation 1986q3-1990q1"
	local ytitle="log(wage)"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen | Validation 1986q3-1990q1"
	local ytitle="log(employment) "
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall | Validation 1986q3-1990q1"
	local ytitle="log(employment)"
	}
else{
}

/// Graph Validation 
twoway (connected _Y_treated period_4q) (connected _Y_synthetic period_4q, lcolor(blue) mcolor(blue)), xline(0)  scheme(s1color) title(`title') ytitle(`ytitle') xtitle("Event Time") legend(order(1 "CA" 2 "Synthetic CA"))
graph export "2Bv`var'_spec2.png", as(png) replace
restore
}



** 2B  v) ** 
// Pseudo synthetic DID


use emp_wage_data2biii,clear
xtset statenum quarterdate
merge m:1 statenum using weights.dta, nogenerate // Merge with my dta containing all weights assigned by the SC method


// DID with the weights form SC

//Weights from Specification 1

foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{

replace s1w_`var'=1 if stateabb=="CA"
	   areg `var' CA_post i.quarterdate [aw=s1w_`var'], absorb(statenum) cluster(statenum)
	  eststo SCDD1`var'


}

//Weights from Specification 2

foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{

replace s2w_`var'=1 if stateabb=="CA"
	 areg `var' CA_post i.quarterdate [aw=s2w_`var'], absorb(statenum) cluster(statenum)
    eststo SCDD2`var'
}


///////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////
/////////// Here I will produce my table with all my estimates (I still have to add the lags, pscore... etc)
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{

if "`var'"=="teen_logwage" {
local title ="Wage: Teen"

esttab  DD`var' L1`var' L2`var' L3`var' PSR`var' SCDD1`var' SCDD2`var'  using Table_2.tex, replace ty keep(CA_post) varlabels(CA_post " `title' ") nonum se noobs nonotes mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)") b(4) fragment
}

else{
if "`var'"=="overall_logwage"{
	local title ="Wage: Overall"
	}
if "`var'"=="teen_logemp"{
	local title ="Employment: Teen"
	}

if "`var'"=="overall_logemp"{
	local title ="Employment: Overall"
	}
else{
}


esttab  DD`var' L1`var' L2`var' L3`var' PSR`var' SCDD1`var' SCDD2`var'  using Table_2.tex, append ty keep(CA_post) varlabels(CA_post "`title'") nonum se noobs nonotes mlabels(none) b(4) fragment

}
}


///////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////


** 2B vi) **