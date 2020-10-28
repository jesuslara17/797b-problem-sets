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
quiet areg I`i'  lnMW  i.d_div_time $X, cluster(state) absorb(state) 
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
areg `i' i.d_div_time lnMW  $X, absorb(statefips) cluster(state)
sca bMW_`i'=_b[lnMW] 
sca sebMW_`i'=_se[lnMW]
eststo r`i'
}

coefplot rI*, vertical keep(lnMW) aseq swapnames scheme(s1color) yline(0) title ("Conditional quantile partial effects") nolabel


coefplot (rI25 \ rI50 \ rI75\ rI100\ rI125 \ rI150 \rI175\ rI200\ rI225\ rI250), vertical keep(lnMW) aseq swapnames scheme(s1color) yline(0) title ("Conditional quantile partial effects")
graph export "1Bi.png", as(png) replace



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

xi: rifreg poverty lnMW  i.d_div_time i.statefips $X, q(`quant')
sca brif_`i'=_b[lnMW]
eststo RI`i'

}

cap drop brif
gen brif=.
forvalues i=25(25)250{
quiet replace brif=brif_`i' if cutoffs==`i'
}

coefplot (RI25 \ RI50 \ RI75\ RI100\ RI125 \ RI150 \RI175\ RI200\ RI225\ RI250), vertical keep(lnMW) aseq swapnames scheme(s1color) yline(0) title ("Unconditional quantile partial effects")
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

egen statenum2=group(statenum)
// Treatment variables
gen post=0
	replace post=1 if(time>=1988.75)
gen CA=0
	replace CA=1 if(stateabb=="CA")

gen CA_post=post*CA
** 2A i) **
//Gen logs of dependent variables
gen overall_logemp = ln(overall_emp) 
gen teen_logemp=log(teen_emp)





// Our 4 outcome variables: teen_logwage, overall_logwage, overall_logemp, teen_logemp
g period_4q = floor((quarterdate-114)/4)


//More formatting needed
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
preserve 
collapse (mean) `var', by(CA period_4q)

twoway (connected `var' period_4q if CA==1)  ///
(connected  `var' period_4q if CA==0), xline(0) scheme(s1color)  ///
xtitle("Event Time") ///

graph export "2Ai`var'.png", as(png) replace
restore
}


//DD regressions
xtset statenum quarterdate


foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp{
	xtreg `var' CA_post i.quarterdate, fe robust
		eststo
}

//Personalize formating
esttab using 2ai.tex, replace keep(CA_post) nonum se noobs mlabels("log(teen wage)" "log(teen employ)" "log(overall wage)" "log(overall employ)") title(Overall Difference in Difference\label{auto}) b(3) coef(CA_post "diff-in-diff")	

** 2A ii) **

//What happens if we control for pre-existing linear and cuadratic trends?



** 2A iii) **

//Propensity Score Reweighting
//Own calculations
//teffects ipw

** 2A iv) **



** PART 2B **
*************

// Synthetic controls


** 2B i) **




//Method one: All quarterly pretreatment outcomes
 
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {

synth `var' ///
///`var'(88) `var'(89) `var'(90) `var'(91) /// 
`var'(92) `var'(93) `var'(94) `var'(95) `var'(96) ///
`var'(97) `var'(98) `var'(99) `var'(100) ///
`var'(101) `var'(102) `var'(103) ///
`var'(104) `var'(105) `var'(106) `var'(107) /// 
`var'(108) `var'(109) `var'(110) `var'(111) ///
`var'(112) `var'(113), trunit(6) trperiod(114) keep("spec1_`var'.dta", replace)

preserve

use spec1_`var', clear 
rename _W_Weight s1w_`var'
sa spec1_`var', replace 



gen period_4q = floor((_time-114)/4)
collapse (mean) _Y_treated (mean) _Y_synthetic, by(period_4q)
/// More formating
twoway (connected _Y_treated period_4q) (connected _Y_synthetic period_4q), xline(0)  scheme(s1color) xtitle("Event Time") 
graph export "2Bi`var'_spec1.png", as(png) replace

gen Yt_minus_Ys=_Y_treated-_Y_synthetic
/// More formating
twoway (connected Yt_minus_Ys period_4q), xline(0) yline(0) scheme(s1color) xtitle("Event Time") 
graph export "2Bi`var'dif_spec1.png", as(png) replace
restore 
}




//Method two: All quarterly pretreatment outcomes


foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {

synth `var' `var'(92(1)113) race_share1 race_share2 race_share3 hispanic_share emp_sh_ind1 emp_sh_ind2 emp_sh_ind3 emp_sh_ind4 emp_sh_ind5 emp_sh_ind6 emp_sh_ind7 emp_sh_ind8 emp_sh_ind9, trunit(6) trperiod(114) keep("spec2_`var'.dta", replace)

preserve

use spec2_`var', clear 
rename _W_Weight s2w_`var'
sa spec2_`var', replace 

gen period_4q = floor((_time-114)/4)
collapse (mean) _Y_treated (mean) _Y_synthetic, by(period_4q)


/// Graph 1: CA vs Synthetic CA
twoway (connected _Y_treated period_4q) (connected _Y_synthetic period_4q), xline(0)  scheme(s1color) xtitle("Event Time") 
graph export "2Bi`var'_spec2.png", as(png) replace

/// Graph 2: Difference between the two
gen Yt_minus_Ys=_Y_treated-_Y_synthetic
twoway (connected Yt_minus_Ys period_4q), xline(0) yline(0) scheme(s1color) xtitle("Event Time") 
graph export "2Bi`var'dif_spec2.png", as(png) replace

/// Estimate of the effect:

restore 
}

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
drop _time _Y_treated _Y_synthetic


mkmat _Co_Number s1w_overall_logwage s2w_overall_logwage s1w_teen_logwage s2w_teen_logwage, mat(Weights_Wage)

mat colnames Weights_Wage ="State Number" "S1: Overall Wage" "S2: Overall Wage" "S1: Teen Wage" "S2: Teen Wage"  

esttab m(Weights_Wage) using 2Bi_Weights_Wage.tex, replace title(Weights to Each State: Wage) nomtitles booktabs

mkmat _Co_Number s1w_overall_logemp s2w_overall_logemp s1w_teen_logemp s2w_teen_logemp, mat(Weights_Emp)

mat colnames Weights_Emp ="State Number" "S1: Overall Emp" "S2: Overall Emp" "S1: Teen Emp" "S2: Teen Emp" 

esttab m(Weights_Emp) using 2Bi_Weights_Emp.tex, replace title(Weights to Each State: Employment) nomtitles booktabs

restore 

** 2B ii) **

** 2B iii) **

xtset quarterdate statenum

*doesn't work yet
local placebolist "1 2 4 5 8 10 12 13 16 17 18 20 21 22 24 26 28 29 30 31 32 34 35 36 37 39 40 45 46 47 48 49 51 54 56" 

foreach i in `placebolist'{
foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {

quiet synth `var' `var'(92(1)113) race_share1 race_share2 race_share3 hispanic_share emp_sh_ind1 emp_sh_ind2 emp_sh_ind3 emp_sh_ind4 emp_sh_ind5 emp_sh_ind6 emp_sh_ind7 emp_sh_ind8 emp_sh_ind9, trunit(`i') trperiod(114) keep("$dta2b/2_`var'_`i'.dta", replace)

preserve

use "$dta2b/2_`var'_`i'.dta", clear 
gen dif_`var'_`i'=
save "$dta2b/2_`var'_`i'.dta", replace
restore 
}
}

// PLACEBO SYNTHETIC CONTROLS


** 2B iv) **

** 2B v) **


** 2B vi) **