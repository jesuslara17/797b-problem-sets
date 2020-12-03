/// This do-file helped me clarify what does it mean to have delta ys as dependent variable and how to conduct a diff-in-diff estimation///

clear all

global home "C:/Users/User/Documents/GitHub/797b-problem-sets/DDCG_Replication"
global tables "$home/tables"
global figures "$home/figures"
global auxdata "$home/auxdata"
global numbers "$home/numbers"

cd $home

use DDCGdata_final, clear


xtset wbcode2 year


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

gen f10y=F10.y
order country_name tdemoc year y ydep*
keep if tdemoc!=.
//We keep only countries the transitioned to democracy in the years that this happened, and observations of nondemocracies


keep if ydep25!=.
keep year country_name wbcode2 tdemoc dem ydep25 y f10 lag*
order f10 lag1y y ydep25 tdemoc year country_name

// Diff-in-diff with regression and manually
reg ydep25 tdemoc, cluster(wbcode2)
sca dd_reg=_b[tdemoc]

// Get means pre and post for treated and non-treated
//pre
quiet su lag1y if tdemoc==0
sca ypre0=r(mean)
quiet su lag1y if tdemoc==1
sca ypre1=r(mean)

//post
quiet su f10 if tdemoc==0
sca ypost0=r(mean)
quiet su f10 if tdemoc==1
sca ypost1=r(mean)

sca DD=(ypost1-ypost0)-(ypre1-ypre0)
di DD
di dd_reg


*They are the equal. It should be obvious... but not for me! ☭ ¯\_(ツ)_/¯ ☭


use "$auxdata/event_44_.dta", clear

reg ydep30 event_44_ lag*, cluster(countrynum)

use "$auxdata/event_183_.dta",clear

forvalues s=25/29{

reg ydep`s' event_183_
eststo g`s'
}

suest g*

nlcom (event183: [g25_mean]_b[event_183_]+[g26_mean]_b[event_183_]+[g27_mean]_b[event_183_]+[g28_mean]_b[event_183_]+[g29_mean]_b[event_183_])

sca b_event183= r(b)[1,1]
di b_event183

//na. rowmeans 

*** Synthetic controls

use DDCG_with_events, clear 
xtset countrynum year


su year if event_18_==1
local t0 = r(mean)
sca t0= r(mean)
sca t10=t0-10
su countrynum if event_18_== 1
sca tcountry=r(mean)
replace event_18_=1 if event_18_==. & countrynum==tcountry
drop if event_18_==.
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
}
di "jeje"
}

order y

forvalues k=1/10{
local t`k'=t0-`k'
}
local t0=t0
local tcountry=tcountry
//Controls must have y data for at least 

synth y  y(`t1') y(`t2') y(`t3') y(`t4'), trunit(`tcountry')  trperiod(`t0') keep("$auxdata/s1_event_18_", replace) figure

use "$auxdata/s1_event_18_.dta", clear
rename _Co_Number countrynum   
rename _W_Weight weight  
keep countrynum s1w_event_18_
sa  "$auxdata/s1_event_18_.dta", replace

synth y y y(`t1') y(`t2') y(`t3') y(`t4') y(`t5') y(`t6') y(`t7') y(`t8') y(`t9') y(`t10'), trunit(`tcountry')  trperiod(`t0') keep("$auxdata/S2_event_18_", replace) figure

use "$auxdata/synth_event_18_", clear
rename _Co_Number countrynum   
rename _W_Weight weight  
keep countrynum s1w_event_18_
sa  "$auxdata/s1_event_18_.dta", replace 



////////////////////////////////////////////////////
////////////////////////////////////////////////////

use DDCG_with_events, clear 
xtset countrynum year


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
}
di "jeje"
}

order y

forvalues k=1/10{
local t`k'=t0-`k'
}
local t0=t0
local tcountry=tcountry
//Controls must have y data for at least 

synth y  y(`t1') y(`t2') y(`t3') y(`t4'), trunit(`tcountry')  trperiod(`t0') keep("$auxdata/s1_`var'", replace) figure

use "$auxdata/s1_`var'.dta", clear
rename _Co_Number countrynum   
rename _W_Weight weight  
keep countrynum s1w_event_18_
sa  "$auxdata/s1_`var'.dta", replace

synth y y y(`t1') y(`t2') y(`t3') y(`t4') y(`t5') y(`t6') y(`t7') y(`t8') y(`t9') y(`t10'), trunit(`tcountry')  trperiod(`t0') keep("$auxdata/s2_`var'", replace) figure

use "$auxdata/synth_`var'", clear
rename _Co_Number countrynum   
rename _W_Weight weight  
keep countrynum s1w_event_18_
sa  "$auxdata/s1_`var'.dta", replace 

/////////////////////////////////////
/////////////////////////////////////

local var event_18_
use "$auxdata/conley_`var'.dta", clear
xtset countrynum year 
su year if tdemoc==1 & l.tdemoc==.
local t0=r(mean)
local tpre=`t0'-1
local tpost15=`t0'+15
local tpost19=`t0'+19
*drop if year> `tpost19' & year<`tpre' 


areg y `var' lag* i.yy*, absorb(countrynum)  



*** Conley test

//  https://www.ssc.wisc.edu/~ctaber/DD/diffdiff.html https://www.ssc.wisc.edu/~ctaber/742/contab12.pdf 
//Prepare dataset
local var event_183_
use "$auxdata/conley_`var'.dta", clear
xtset countrynum year 
su year if `var'==1 & tdemoc==1 & l.tdemoc==.
local t0=r(mean)
di `t0'
local tpre=`t0'-1
local tpost15=`t0'+15
local tpost19=`t0'+19
/*
drop if year>`tpost19'   
drop if year< `tpost15' & year!=`tpre'
*/
egen countrynum2=group(country_name)
xtset countrynum2 year 
quiet su countrynum2 
local N=r(N)
local mincountry=r(min)
local maxcountry=r(max)




	/* Create dummies for state+year interactions*/
		egen Dstyr=group(year countrynum2)
		reg y i.Dstyr //not using controls like example (asian etc.)

		predict beta, xb

	/* Predict residuals from regression */
		quietly reg beta `var' i.countrynum2 i.year, noc
			quietly predict y_res, res
			quietly replace y_res=y_res+_b[`var']*`var'

		/* Create d tilde variable*/
		quietly bysort year: egen y_djt`var'=mean(`var') if `var'==1
		quietly bysort year: egen y_djt=sum(y_djt`var') 
		quietly bysort countrynum2: egen y_meandjt=mean(y_djt)
		qui: g y_dtil=y_djt-y_meandjt

		/* Obtain difference in differences coefficient*/
		qui: reg y_res y_dtil if `var'==1, noc
		matrix y_alpha=e(b)

		
		/* Simulations*/


			forvalues i=`mincountry'/`maxcountry'{
				capture {
				reg y_res y_dtil if countrynum2==`i' & `var'!=1, noc
				matrix y_alpha = y_alpha\e(b)
			}
			}
		

		matrix y_asim = y_alpha[2...,1]
		matrix y_alpha = y_alpha[1,1]

		/* Confidence intervals */
		qui: svmat y_alpha 
		qui: svmat y_asim

		qui: sum y_alpha
		qui: gen y_alpha=r(mean)
		qui: g y_ci=y_alpha - y_asim

		
		/* form confidence intervals */
		local numst=36
		local i05=floor(0.050*(`N'-1))
		local i95=ceil(0.950*(`N'-1))

	quietly sum y_alpha
		display as text "Difference in Differences coefficient=" as result _newline(2) r(mean)
			scalar y_DiD = r(mean)

		di " "	
		di " "

		sort y_asim
		sum y_ci if _n==`i05'|_n==`i95' 
		display as text "90% Confidence interval=" as result _newline(2) r(min) _col(15) r(max)
			scalar y_LCI = r(min)
			scalar y_UCI = r(max)
		cap drop beta Dstyr

su y_ci
 

 
 use "$auxdata/event_50_", clear
 
 
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
