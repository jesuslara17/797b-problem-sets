
//////// THIS

use s1placebos_overall_logwage, clear
rename CA_diff_overall_logwage diff_36 //Now state 36 is CALIFORNIA REMEMBER
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


sort r
g axis=_n

hdquantile r, p(5 10 90 95) matname(cutoffs)
local q5=cutoffs[1,1]
local q10=cutoffs[1,2]
local q90=cutoffs[1,3]
local q95=cutoffs[1,4]

set scheme s1color

graph dot (asis) r,  over (stateabb, sort(1)) yline(`q5' `q10' `q90' `q95', lcolor(black)) ysc(fex r(0 3)) marker(1, ms(Oh)) 

graph play 2Biii_editing // I stored an editing of y grap
 

//// NOW I HAVE MY r and alpha for all states...



// Loop over all variables


foreach var of varlist teen_logwage overall_logwage teen_logemp overall_logemp {
use s1placebos_`var', clear
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

graph dot (asis) r,  over (stateabb, sort(1)) yline(`q5' `q10' `q90' `q95', lcolor(black)) ysc(fex r(0 3)) marker(1, ms(Oh)) 

graph play 2Biii_editing // I stored an editing of y grap

graph export "2Biii_inference_`var'.png", as(png) replace
}
*****
use emp_wage_data2Biii,replace
xtset statenum quarterdate

foreach var of varlist teen_logwage  {

synth `var' ///
///`var'(88) `var'(89) `var'(90) `var'(91) /// 
`var'(92) `var'(93) `var'(94) `var'(95) `var'(96) ///
`var'(97) `var'(98) `var'(99) `var'(100) ///
`var'(101) `var'(102) `var'(103) ///
`var'(104) `var'(105) `var'(106) `var'(107) /// 
`var'(108) `var'(109) `var'(110) `var'(111) ///
`var'(112) `var'(113), trunit(6) mseperiod(92(1)105) trperiod(114) figure 
}


foreach var of varlist teen_logwage  {

synth `var' ///
///`var'(88) `var'(89) `var'(90) `var'(91) /// 
`var'(92) `var'(93) `var'(94) `var'(95) `var'(96) ///
`var'(97) `var'(98) `var'(99) `var'(100) ///
`var'(101) `var'(102) `var'(103) ///
`var'(104) `var'(105) `var'(106) `var'(107) /// 
`var'(108) `var'(109) `var'(110) `var'(111) ///
`var'(112) `var'(113), trunit(6)  trperiod(114) figure 
}

preserve

use spec1_`var', clear 
gen CA_diff_`var'=_Y_synthetic-_Y_treated
rename _W_Weight s1w_`var'
sa spec1_`var', replace 

gen period_4q = floor((_time-114)/4)
collapse (mean) _Y_treated (mean) _Y_synthetic, by(period_4q)
/// More formating
*twoway (connected _Y_treated period_4q) (connected _Y_synthetic period_4q), xline(0)  scheme(s1color) xtitle("Event Time") 
graph export "2Bi`var'_spec1.png", as(png) replace

gen Yt_minus_Ys=_Y_treated-_Y_synthetic
/// More formating
*twoway (connected Yt_minus_Ys period_4q), xline(0) yline(0) scheme(s1color) xtitle("Event Time") 
graph export "2Bi`var'dif_spec1.png", as(png) replace
restore 
}
