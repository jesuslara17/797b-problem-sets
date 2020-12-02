//  		 ☭ 	797B Jesús Lara	☭	 ///
/// 	Critical Replication and extension of   ///
///DDCG by Acemoglu, Naidu, Restrepo & Robinson (2018) ///

clear all

global home "C:/Users/User/Documents/GitHub/797b-problem-sets/DDCG_Replication"
global tables "$home/tables"
global figures "$home/figures"
global auxdata "$home/auxdata"
global numbers "$home/numbers"

cd $home

use DDCGdata_final, clear


xtset wbcode2 year

/// Dem is the measure of democracy


********************
****** PART 1*******
********************

/// Replication of table 2

/*
quiet {
xtreg y dem l.y i.yy*, fe r cluster(wbcode2) 
eststo T2_1
nlcom (longrun: _b[dem]/(1-_b[l1.y])) (persistence: _b[l1.y]) , post
eststo T2_1a

xtreg y dem l(1/2).y i.yy*, fe r cluster(wbcode2)
eststo T2_2

nlcom (longrun: _b[dem]/(1-_b[l1.y]-_b[l2.y])) (persistence: _b[l1.y]+_b[l2.y]) , post
eststo T2_2a

xtreg y dem l(1/4)y i.yy*, fe r cluster(wbcode2)
eststo T2_3
nlcom (longrun: _b[dem]/(1-_b[l1.y]-_b[l2.y]-_b[l3.y]-_b[l4.y])) (persistence: _b[l1.y]+_b[l2.y]+_b[l3.y]+_b[l4.y]) , post
eststo T2_3a
}

esttab T2_1 T2_2 T2_3 using "$tables/Table2.tex", keep(dem  L.y  L2.y  L3.y  L4.y) varlabels(dem "Democracy" L.y "Log GDP, first lag" L2.y "Log GDP, second lag" L3.y "Log GDP, third lag" L4.y "Log GDP, fourth lag") fragment nonum noobs nonotes mlabels("(1)" "(2)" "(3)") b(3) se ty replace nostar  wrap gaps

esttab T2_1a T2_2a T2_3a using "$tables/Table2.tex", varlabels (longrun "Long-run effect of democracy" persistence "Persistence") fragment nonum nonotes  b(3) se ty append nostar nomtitles nolines wrap gaps
*/

///////////////////////////////
// Replication of table 5/////
//////////////////////////////


// 2) Now generate our dependent variables: \Delta y^s_ct= y^s_ct -y_t-1
// t: period of democratization, s: years after democratization 
// We consider: 15 years before democratization, 30 years after democratizationuse DDCGdata_final, clear

// Generate Treatment democracy variable td

use DDCGdata_final, clear
gen tdemoc=.  
replace tdemoc=1 if dem==1 & l.dem==0 // captures the exact moment when a country transitions to democracy
replace tdemoc=0 if dem==0 & l.dem==0  // countries that have never been a democracy

// 4 lags in GDP. These will be covariates in our regression
forvalues i=1/4{
gen lag`i'y=l`i'.y
}
keep if lag1y!=. & lag2y!=. & lag3y!=. & lag4y!=.

// 15 lags
forvalues i=0/15{
local k =15-`i'
gen ydep`i'=L`k'.y-L.y
}

// 30 leads
forvalues i=16/45{
local k=`i'-15
gen ydep`i'=F`k'.y-L.y
}
order country_name tdemoc year y ydep*
keep if tdemoc!=.
// tdemoc is missing value if country is democracy and was democracy the period before
// We will have only observations of: nondemocracy and exact transition (including reversals)


set seed 12345                       

//// A. Adjustment regression 

xtset, clear
bootstrap _b, reps(10) cluster(wbcode2): tefpas_ra 
parmest, format(estimate min95 max95) saving("$auxdata/ra", replace)
eststo main_ra
// Get 5 year averages
nlcom (ra: (_b[c11]+_b[c12]+_b[c13]+_b[c14]+_b[c15])/5), post
eststo ra1

estimates restore main_ra
nlcom (ra: (_b[c16]+_b[c17]+_b[c18]+_b[c19]+_b[c20])/5), post
eststo ra2 

estimates restore main_ra
nlcom (ra: (_b[c21]+_b[c22]+_b[c23]+_b[c24]+_b[c25])/5), post
eststo ra3 

estimates restore main_ra
nlcom (ra: (_b[c26]+_b[c27]+_b[c28]+_b[c29]+_b[c30])/5), post
eststo ra4 

estimates restore main_ra
nlcom (ra: (_b[c31]+_b[c32]+_b[c33]+_b[c34]+_b[c35])/5), post
eststo ra5 

estimates restore main_ra
nlcom (ra: (_b[c36]+_b[c37]+_b[c38]+_b[c39]+_b[c40])/5), post
eststo ra6

estimates restore main_ra
nlcom (ra:  (_b[c41]+_b[c42]+_b[c43]+_b[c44]+_b[c45])/5), post
eststo ra7 

esttab ra* using "$tables/T5_ra.tex", replace varlabels (ra "Avg. effect on log GDP") fragment nostar nonum nonotes se nomtitles nolines wrap gaps ty  noobs b(3)

/// B. Inverse propensity score reweighting

xtset, clear
bootstrap _b, reps(10) cluster(wbcode2): tefpas_ipw 
parmest, format(estimate min95 max95) saving("$auxdata/psr", replace)
eststo main_psr 
// Get 5 year averages
nlcom (psr: (_b[c11]+_b[c12]+_b[c13]+_b[c14]+_b[c15])/5), post
eststo psr1

estimates restore main_psr 
nlcom (psr: (_b[c16]+_b[c17]+_b[c18]+_b[c19]+_b[c20])/5), post
eststo psr2 

estimates restore main_psr 
nlcom (psr: (_b[c21]+_b[c22]+_b[c23]+_b[c24]+_b[c25])/5), post
eststo psr3 

estimates restore main_psr 
nlcom (psr: (_b[c26]+_b[c27]+_b[c28]+_b[c29]+_b[c30])/5), post
eststo psr4 

estimates restore main_psr 
nlcom (psr: (_b[c31]+_b[c32]+_b[c33]+_b[c34]+_b[c35])/5), post
eststo psr5 

estimates restore main_psr 
nlcom (psr: (_b[c36]+_b[c37]+_b[c38]+_b[c39]+_b[c40])/5), post
eststo psr6

estimates restore main_psr 
nlcom (psr:  (_b[c41]+_b[c42]+_b[c43]+_b[c44]+_b[c45])/5), post
eststo psr7 

esttab psr* using "$tables/T5_psr.tex", replace varlabels (psr "Avg. effect on log GDP") fragment nostar nonum nonotes se nomtitles nolines wrap gaps ty  noobs b(3)


// C. Doubly robust estimator

xtset, clear
bootstrap _b, reps(10) cluster(wbcode2): tefpas_ipwra 
parmest, format(estimate min95 max95) saving("$auxdata/dr", replace)
eststo main_dr 
// Get 5 year averages
nlcom (dr: (_b[c11]+_b[c12]+_b[c13]+_b[c14]+_b[c15])/5), post
eststo dr1

estimates restore main_dr 
nlcom (dr: (_b[c16]+_b[c17]+_b[c18]+_b[c19]+_b[c20])/5), post
eststo dr2 

estimates restore main_dr 
nlcom (dr: (_b[c21]+_b[c22]+_b[c23]+_b[c24]+_b[c25])/5), post
eststo dr3 

estimates restore main_dr 
nlcom (dr: (_b[c26]+_b[c27]+_b[c28]+_b[c29]+_b[c30])/5), post
eststo dr4 

estimates restore main_dr 
nlcom (dr: (_b[c31]+_b[c32]+_b[c33]+_b[c34]+_b[c35])/5), post
eststo dr5 

estimates restore main_dr 
nlcom (dr: (_b[c36]+_b[c37]+_b[c38]+_b[c39]+_b[c40])/5), post
eststo dr6

estimates restore main_dr 
nlcom (dr:  (_b[c41]+_b[c42]+_b[c43]+_b[c44]+_b[c45])/5), post
eststo dr7 

esttab dr* using "$tables/T5_dr.tex", replace varlabels (dr "Avg. effect on log GDP") fragment nostar nonum nonotes se nomtitles nolines wrap ty  noobs b(3)


*/
********************
****** PART 2*******
********************

/// Now it is correct

use DDCGdata_final, clear

gen tdemoc=.  
replace tdemoc=1 if dem==1 & l.dem==0 // captures the exact moment when a country transitions to democracy
replace tdemoc=0 if dem==0 & l.dem==0 
// 1) 4 lags in GDP. These will be covariates in our regression
forvalues i=1/4{
gen lag`i'y=l`i'.y
}

forvalues i=0/15{
local k =15-`i'
gen ydep`i'=L`k'.y-L.y
}


forvalues i=16/45{
local k=`i'-15
gen ydep`i'=F`k'.y-L.y
}


order country_name tdemoc year y ydep*
keep if tdemoc!=.

/// 4 lags 
forvalues s=0/45{
quiet reg ydep`s' tdemoc  lag*, cluster(wbcode2)
if `s'==0{
mat ests=(_b[tdemoc], r(table)[5,1],r(table)[6,1])
}
else{
mat ests=ests\(_b[tdemoc], r(table)[5,1],r(table)[6,1])
mat colnames ests="b" "ll" "ul"
}
}

drop ests*
svmat ests

cap drop yad
gen yad=. 
forvalues i=0/45{
local k=`i'-14
local j=`i'+1
replace yad=`k' in `j'
}

line ests1 yad, lcolor(black) scheme(s1color) xtitle(Years around democratization) ytitle(Change in GDP per capita log points) yline(0, lcolor(black) lpattern(dash))||line ests2 yad,  lcolor(gray) lpattern(dash)||line ests3 yad,  lcolor(gray) lpattern(dash) legend(off)
graph export "$figures/part2a.png", as(png) replace



/// No lags
forvalues s=0/45{
quiet reg ydep`s' tdemoc, cluster(wbcode2)
if `s'==0{
mat ests=(_b[tdemoc], r(table)[5,1],r(table)[6,1])
}
else{
mat ests=ests\(_b[tdemoc], r(table)[5,1],r(table)[6,1])
mat colnames ests="b" "ll" "ul"
}
}

drop ests*
svmat ests
cap drop yad
gen yad=. 
forvalues i=0/45{
local k=`i'-14
local j=`i'+1
replace yad=`k' in `j'
}

line ests1 yad, lcolor(black) scheme(s1color) xtitle(Years around democratization) ytitle(Change in GDP per capita log points) yline(0, lcolor(black) lpattern(dash))||line ests2 yad,  lcolor(gray) lpattern(dash)||line ests3 yad,  lcolor(gray) lpattern(dash) legend(off) ylabel(#6) 
graph export "$figures/part2b.png", as(png) replace



/*
coefplot (ldid*, lcolor(black) mcolor(black)), vertical keep(tdemoc)  aseq swapnames scheme(s1color)  title ("DID estimates") ciopts(recast(rline) lcolor(gray) lpattern(dash)) recast(line) yline(0, lcolor(black) lpattern(dash) ) xtitle(Years around democratization) ytitle(Change in GDP per capita log points) xlabel(#10)  
graph export "$figures/part2b.png", as(png) replace
*/ 
 

********************
****** PART 3*******
********************

/// Replicate the figures?

* ¯\_(ツ)_/¯


foreach type in psr dr{

use "$auxdata/`type'.dta",clear

gen event=.

forvalues i=1/46{
local k=`i'-16
replace event=`k' in `i'
}

line estimate event, lcolor(black) || line min95 event, lcolor(gray) lpattern(dash) || line max95 event, lcolor(gray) lpattern(dash) ytitle(Change in GDP per capita log points) xlabel(#10) xtitle(Years around democratization) scheme(s1color) legend(off) yline(0, lpattern(dash))
graph export  "$figures/part3`type'.png", replace
}

********************
****** PART 4*******
********************



use ${home}/DDCGdata_final, clear

// The code below identifies treated units (countries that transitioned to democracy with no reversals) and the clean controls associated to each treated unit (countries that did not have a transition before t0 and at least 20 year after)
// We will use tunit for part 6, so it will assign a missing value to all countries which experienced a kind of transition 
// Este loop me quedó bien culey!!! ¯\_(ツ)_/¯

egen countrynum= group(country_name)
gen tdemoc=.
replace tdemoc=1 if dem==1 & l.dem==0
order tdemoc countrynum year
gen tunit=.
quiet su countrynum 
local mincountry=r(min)
local maxcountry=r(max)

forvalues i=`mincountry'/`maxcountry'{
quiet  su year if countrynum==`i' & tdemoc==1
if r(N)==1{
di `i'
di "Only one transition to democracy"
local t0=r(mean)
quiet su dem if year >=`t0' & countrynum==`i'

if r(min)==r(max){
di " `i' is treated unit"
quietly replace tunit=1 if countrynum==`i'

//Event i
gen event_`i'_=.
// Prepare dependent variables for regressions. 

forvalues j=`mincountry'/`maxcountry'{
if `j'==`i'{
quietly replace event_`i'_=0 if year<`t0' & countrynum==`j'
quietly replace event_`i'_=1 if year==`t0' & countrynum==`j'
}

else{
local t20= `t0'+20
quietly su dem if countrynum==`j' & year <=`t20'
if r(max)==0{

// di "`j' is a clean control of `i'"
quietly replace event_`i'_=0 if countrynum==`j'
}
else{
quietly replace event_`i'_=. if countrynum==`j'
}
}
}
}
else{
di "Experienced a reversal"
//quietly replace tunit=. if countrynum==`i'
}
}

else {
di "`i' had more than one transition to democracy"
//quietly replace tunit=. if countrynum==`i'
}
}
quietly replace tunit=0 if tunit==.

save "DDCG_with_events.dta",replace

// Generate variables and prepare events datasets

forvalues i=1/4{
gen lag`i'y=l`i'.y
}

// Gen all leads, we may only use a couple 
forvalues i=16/45{
local k=`i'-15
gen ydep`i'=F`k'.y-L.y
}


foreach var of varlist event_*{
preserve 
drop if `var'==.
keep `var' ydep* country_name countrynum dem tdemoc lag* year y
order year countrynum country_name `var' tdemoc dem y ydep* lag* 
quiet su year if `var'==1
local t0=r(mean)
keep if year==`t0'
save "$auxdata/`var'.dta", replace
restore
}
// One wide data set for each event



/*
forvalues i=`mincountry'/`maxcountry'{
quiet su dem if countrynum==`i'
if r(max)==r(min){
di "`i' is a clean control: same political regime"
quietly replace tunit=0 if countrynum==`i'
}
else{
}
}
*/


****** 4 (a) *******
********************

/// Proceed with estimation...
// Preliminary code
*List of all treated units, how to get it?


//Store countrynum s.t. tunit=1 in a matrix

quiet su countrynum 
local mincountry=r(min)
local maxcountry=r(max)
mat T=.
forvalues k=`mincountry'/`maxcountry'{
quiet su tunit if countrynum==`k'
if r(mean)==1{
if T[1,1]==. {
mat T=(`k')
}
else {
mat T=(T \ `k')
}
}
else {
}
}

use DDCG_with_events, clear 

*** 

// This one works. Exactly 35 events
foreach var of varlist event_*{
use "$auxdata/`var'.dta",replace

forvalues s=30/33{
quiet su ydep`s' if `var'==1
di "`var'" `s' 
di r(mean)

// Define first for the first event=18
if "`var'"=="event_18_"{
if r(mean)!=.{
quiet reg ydep`s' `var' lag*, cluster(countrynum)
mat b`s'=(_b[`var'])
}
else {
mat b`s'=(.)
}
}
else{
if r(mean)!=.{
quiet reg ydep`s' `var' lag*, cluster(countrynum)
mat b`s'=b`s' \ (_b[`var'])
}
else{
mat b`s'=b`s' \ (.)
}
}
}
}


forvalues s=30/34{
local k=`s'-15
svmat b`s'
replace b`s'=. if b`s'==0 // If the coefficient is exactly=0 it is because of problems of colinearity, idk why that happens ¯\_(ツ)_/¯
}

egen average_b= rowmean(b*)

hist average_b, title (Effect of Democratization on Growth: Event Analysis) subtitle(Average 15-19 years after democratization) freq w(8) scheme(s1color) xtitle(Estimate) xlabel(#20) 
graph export "$figures/4a.png",replace

// b= r(table)[1,1]
// se= r(table) [1,2]
// bmin95= r(table)[1,4]
// bmax95== r(table)[1,5]  


****** 4 (b) ********
*********************

// Ferman & Pinto 

****** 4 (c) ********
*********************

// Stack in wide format? 
use DDCG_with_events, clear 

foreach var of varlist event_*{
use DDCG_with_events, clear 
if "`var'"=="event_18_"{
use "$auxdata/`var'.dta",replace
}
else{
append using "$auxdata/`var'.dta"
}

order year country_name event*

foreach var of varlist event_*{
replace `var'=0 if `var'==.

}
reg ydep20 i.dem i.event_* lag*
}
********************
****** PART 5*******
********************
use DDCG_with_events, clear 
xtset countrynum year

foreach var of varlist  event_*{

preserve
su year if `var'==1
local t0 = r(mean)
sca t0= r(mean)
sca t10=t0-10
su countrynum if `var'== 1
sca tcountry=r(mean)
replace `var'=1 if `var'==. & countrynum==tcountry
drop if `var'==.
drop if year<t10

egen countrynum2=group(country_name)
order countrynum2 
quiet su countrynum2 
local mincountry=r(min)
local maxcountry=r(max)

forvalues i=`mincountry'/`maxcountry'{
quiet su year if countrynum2==`i'
local N=r(N)
local N5= `N'-5
quiet su y if countrynum2==`i'
di r(N)
di `N5'
if r(N)<`N' {
drop if countrynum2==`i' 
}
else{
di "jeje"
}
}

order y

forvalues k=1/10{
local t`k'=t0-`k'
}

local t0=t0
local tcountry=tcountry
//Controls must have y data for at least 

cap: synth y  y(`t1') y(`t2') y(`t3') y(`t4'), trunit(`tcountry')  trperiod(`t0') keep("$auxdata/s1_`var'", replace) 


cap: use "$auxdata/s1_`var'.dta", clear
cap: rename _Co_Number countrynum   
cap: rename _W_Weight s1w_`var' 
cap: keep countrynum s1w_`var'
cap: drop if countrynum==. & s1w_`var'==.
cap: sa  "$auxdata/s1_`var'.dta", replace

cap: synth y y y(`t1') y(`t2') y(`t3') y(`t4') y(`t5') y(`t6') y(`t7') y(`t8') y(`t9') y(`t10'), trunit(`tcountry')  trperiod(`t0') keep("$auxdata/s2_`var'", replace) 

cap: use "$auxdata/s2_`var'", clear
cap: rename _Co_Number countrynum   
cap: rename _W_Weight s2w_`var'  
cap: keep countrynum s2w_`var'
cap: drop if countrynum==. & s2w_`var'==.
cap: sa  "$auxdata/s2_`var'.dta", replace 
restore
}

// WTF do we do with the weights???
// Pooled regression??? 
// do the same than before??? WTF!!!

*I will do the same alv

use "$auxdata/event_18_.dta", clear

merge 1:1 countrynum using "$auxdata\s1_event_18_.dta"
drop _merge
merge 1:1 countrynum using "$auxdata\s2_event_18_.dta"
drop _merge

replace s1w_event_18_=0 if s1w_event_18_==.
replace s2w_event_18_=0 if s2w_event_18_==.
replace s1w_event_18_=1 if event_18_==1
replace s2w_event_18_=1 if event_18_==1

reg ydep30 event_18_  [aw=s1w_event_18_], robust
reg ydep31 event_18_  [aw=s1w_event_18_], robust
reg ydep32 event_18_  [aw=s1w_event_18_], robust
reg ydep33 event_18_  [aw=s1w_event_18_], robust
reg ydep34 event_18_  [aw=s1w_event_18_], robust


********************
****** PART 6*******
********************