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
order f10 lag1y ydep25 tdemoc year country_name

// Diff-in-diff with regression and manually
reg ydep25 tdemoc, cluster(wbcode2)
sca dd_reg=_b[tdemoc]

// Get means pre and post for treated and non-treated
quiet su lag1y if tdemoc==0
sca ypre0=r(mean)
quiet su lag1y if tdemoc==1
sca ypre1=r(mean)

quiet su f10 if tdemoc==0
sca ypost0=r(mean)
quiet su f10 if tdemoc==1
sca ypost1=r(mean)

sca DD=(ypost1-ypost0)-(ypre1-ypre0)
di DD
di dd_reg


*They are the equal. It should be obvious... but not for me! ☭ ¯\_(ツ)_/¯ ☭


sca DD=(ypost1-ypre1)-(ypost0-ypre0)
di DD
di ddreg

