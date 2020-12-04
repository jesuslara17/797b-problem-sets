//  		  	797B Jesús Lara	☭	 ///
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



*******PART 1****************************
****************************************
****************************************
*******PART 1****************************
****************************************
****************************************
****************************************
****** PART 1***************************
****************************************
****************************************
****************************************
********PART 1***************************
****************************************
****************************************
****************************************
****************************************
****************************************
********PART 1*************************
****************************************
****************************************
xtset wbcode2 year

/// Program to calculate LR effect of democracy 

capture program drop dynamiceffects
		program define dynamiceffects, eclass
		quietly: nlcom (effect1: _b[democracy]) ///
			  (democracy: _b[democracy]) ///
			  (lag1: _b[lag1]) ///
			  (lag2: _b[lag2]) ///
			  (lag3: _b[lag3]) ///
			  (lag4: _b[lag4]) ///
			  , post

		quietly: nlcom (effect2: _b[effect1]*_b[lag1]+_b[democracy]) ///
			  (effect1: _b[effect1]) ///
			  (democracy: _b[democracy]) ///
			  (lag1: _b[lag1]) ///
			  (lag2: _b[lag2]) ///
			  (lag3: _b[lag3]) ///
			  (lag4: _b[lag4]) ///
			  , post

		quietly: nlcom (effect3: _b[effect2]*_b[lag1]+_b[effect1]*_b[lag2]+_b[democracy]) ///
			  (effect2: _b[effect2]) ///
			  (effect1: _b[effect1]) ///
			  (democracy: _b[democracy]) ///
			  (lag1: _b[lag1]) ///
			  (lag2: _b[lag2]) ///
			  (lag3: _b[lag3]) ///
			  (lag4: _b[lag4]) ///
			  , post
			  
		quietly: nlcom (effect4: _b[effect3]*_b[lag1]+_b[effect2]*_b[lag2]+_b[effect1]*_b[lag3]+_b[democracy]) ///
			  (effect3: _b[effect3]) ///
			  (effect2: _b[effect2]) ///
			  (effect1: _b[effect1]) ///
			  (democracy: _b[democracy]) ///
			  (lag1: _b[lag1]) ///
			  (lag2: _b[lag2]) ///
			  (lag3: _b[lag3]) ///
			  (lag4: _b[lag4]) ///
			  , post	  
		
		forvalues j=5(1)25{	  
		local j1=`j'-1
		local j2=`j'-2
		local j3=`j'-3
		local j4=`j'-4

		quietly: nlcom (effect`j': _b[effect`j1']*_b[lag1]+_b[effect`j2']*_b[lag2]+_b[effect`j3']*_b[lag3]+_b[effect`j4']*_b[lag4]+_b[democracy]) ///
			  (effect`j1': _b[effect`j1']) ///
			  (effect`j2': _b[effect`j2']) ///
			  (effect`j3': _b[effect`j3']) ///
			  (democracy: _b[democracy]) ///
			  (lag1: _b[lag1]) ///
			  (lag2: _b[lag2]) ///
			  (lag3: _b[lag3]) ///
			  (lag4: _b[lag4]) ///
			  , post	  	  

		}

		quietly: nlcom (effect25: _b[effect25]) ///
			  (longrun: _b[democracy]/(1-_b[lag1]-_b[lag2]-_b[lag3]-_b[lag4])) ///
			  (democracy: _b[democracy]) ///
			  (persistence: _b[lag1]+_b[lag2]+_b[lag3]+_b[lag4]) ///
			  (lag1: _b[lag1]) ///
			  (lag2: _b[lag2]) ///
			  (lag3: _b[lag3]) ///
			  (lag4: _b[lag4]) ///
			  , post
		ereturn display
		end

/// Replication of table 2
/////////////////////////////

quietly{
////////////
//1 lag////
xtreg y dem l.y i.yy*, fe r cluster(wbcode2) 
eststo T2_1
// export number of obs and countries
local N1=e(N)
local C1=e(N_clust)
file open myfile using ${numbers}/N1.tex, ///
 write text replace 
file write myfile "`N1'" 
file close myfile

file open myfile using ${numbers}/C1.tex, ///
 write text replace 
file write myfile "`C1'" 
file close myfile

quiet nlcom  (democracy: _b[dem]) (lag1: _b[L.y])  (lag2: 0)  (lag3: 0)  (lag4: 0), post
dynamiceffects
eststo T2_1aa
estimates restore T2_1
nlcom (longrun: _b[dem]/(1-_b[l1.y])) (persistence: _b[l1.y]) , post
eststo T2_1a

////////////
//2 lags//////
xtreg y dem l(1/2).y i.yy*, fe r cluster(wbcode2)
eststo T2_2
// export number of obs and countries
local N2= e(N)
local C2= e(N_clust)
file open myfile using ${numbers}/N2.tex, ///
 write text replace 
file write myfile "`N2'" 
file close myfile

file open myfile using ${numbers}/C2.tex, ///
 write text replace 
file write myfile "`C2'" 
file close myfile

nlcom (democracy: _b[dem])  (lag1: _b[L.y])  (lag2: _b[L2.y])  (lag3: 0)  (lag4: 0), post
dynamiceffects
eststo T2_2aa
estimates restore T2_2
nlcom (longrun: _b[dem]/(1-_b[l1.y]-_b[l2.y])) (persistence: _b[l1.y]+_b[l2.y]) , post
eststo T2_2a

//4 lags 
xtreg y dem l(1/4)y i.yy*, fe r cluster(wbcode2)
eststo T2_3
// export number of obs and countries
local N3= e(N)
local C3= e(N_clust)
file open myfile using ${numbers}/N3.tex, ///
 write text replace 
file write myfile "`N3'" 
file close myfile

file open myfile using ${numbers}/C3.tex, ///
 write text replace 
file write myfile "`C3'" 
file close myfile

nlcom (democracy: _b[dem])  (lag1: _b[L.y])  (lag2: _b[L2.y])  (lag3: _b[L3.y])  (lag4: _b[L4.y]), post
dynamiceffects
eststo T2_3aa
estimates restore T2_3
nlcom (longrun: _b[dem]/(1-_b[l1.y]-_b[l2.y]-_b[l3.y]-_b[l4.y])) (persistence: _b[l1.y]+_b[l2.y]+_b[l3.y]+_b[l4.y]) , post
eststo T2_3a
}

esttab T2_1 T2_2 T2_3 using "$tables/Table2.tex", keep(dem  L.y  L2.y  L3.y  L4.y) varlabels(dem "Democracy" L.y "Log GDP, first lag" L2.y "Log GDP, second lag" L3.y "Log GDP, third lag" L4.y "Log GDP, fourth lag") fragment nonum noobs nonotes mlabels("(1)" "(2)" "(3)") b(3) se ty replace nostar  wrap gaps

esttab T2_1a T2_2a T2_3a using "$tables/Table2.tex", varlabels (longrun "Long-run effect of democracy" persistence "Persistence") fragment noobs nonum nonotes  b(3) se ty append nostar nomtitles nolines wrap gaps

esttab T2_1aa T2_2aa T2_3aa using "$tables/Table2.tex", keep(effect25) varlabels(effect25 "Effect of democracy: 25 years") fragment noobs nonum nonotes  b(3) se ty append nostar nomtitles nolines wrap gaps

////////////////////////
/// UNIT ROOTS TESTS ///
////////////////////////
capture program drop unitroot
program define unitroot, rclass
syntax anything[, lags(integer 3) varalt mumain(real -.53796) sigmamain(real .85408)]
	local 0 `anything' 
	gettoken yvar 0 : 0
	gettoken excov 0 : 0, match(par) 
	gettoken bpvar 0 : 0, match(par) 
	
quietly: if `lags'>=1{
reg d.`yvar' l(1/`lags').d.`yvar' yy* i.wbcode2 `excov'
}
quietly: if `lags'==0{
reg d.`yvar' yy* i.wbcode2 `excov'
}
quietly: predict e if e(sample), resid

if `lags'==0{
return scalar SA=1
local SA=1
}
if `lags'==1{
return scalar SA=1/abs(1-(_b[LD.`yvar']))
local SA=1/abs(1-(_b[LD.`yvar']))
}
if `lags'==2{
return scalar SA=1/abs(1-(_b[LD.`yvar']+_b[L2D.`yvar']))
local  SA=1/abs(1-(_b[LD.`yvar']+_b[L2D.`yvar']))
}
if `lags'==3{
return scalar SA=1/abs(1-(_b[LD.`yvar']+_b[L2D.`yvar']+_b[L3D.`yvar']))
local SA=1/abs(1-(_b[LD.`yvar']+_b[L2D.`yvar']+_b[L3D.`yvar']))
}
if `lags'==4{
return scalar SA=1/abs(1-(_b[LD.`yvar']+_b[L2D.`yvar']+_b[L3D.`yvar']+_b[L4D.`yvar']))
local SA=1/abs(1-(_b[LD.`yvar']+_b[L2D.`yvar']+_b[L3D.`yvar']+_b[L4D.`yvar']))
}

quietly: if `lags'>=1{
reg l.`yvar' l(1/`lags').d.`yvar' yy* i.wbcode2 `excov'
}
quietly: if `lags'==0{
reg l.`yvar'  yy* i.wbcode2 `excov'
}
quietly: predict v if e(sample), resid

quietly: if `lags'>=1{
reg d.`yvar' l.`yvar' l(1/`lags').d.`yvar' yy* i.wbcode2
}
quietly: if `lags'==0{
reg d.`yvar' l.`yvar' yy* i.wbcode2
}
quietly: predict residual if e(sample), resid
quietly: gen res2=residual^2
quietly: bysort wbcode2: egen vari=mean(res2)
quietly: gen sigmai=sqrt(vari)

quietly: gen e_est=e/sigmai
quietly: gen v_est=v/sigmai

quietly: reg e_est v_est, noconstant
return scalar tdelta=_b[v_est]/_se[v_est]
return scalar stddelta=_se[v_est]
local tdelta=_b[v_est]/_se[v_est]
local stddelta=_se[v_est]

quietly: predict z if e(sample), resid
quietly: gen z2=z^2
quietly: sum z2
return scalar sigma2=r(mean)
return scalar num=r(N)
local sigma2=r(mean)
local num=r(N)

local tadjmain=(`tdelta'-`num'*`SA'*`stddelta'*`mumain'/`sigma2')/`sigmamain'
return scalar pvalmain=normal(`tadjmain')
return scalar  tadjmain=(`tdelta'-`num'*`SA'*`stddelta'*`mumain'/`sigma2')/`sigmamain'

drop e v residual res2 vari sigmai e_est v_est z z2
end

foreach i of numlist 0 1 3{

unitroot y (dem) (), lags(`i')
local t`i'lag= r(tadjmain)
local t`i'lagc: display %9.2f `t`i'lag' 

file open myfile using ${numbers}/t`i'lagc.tex, ///
 write text replace 
file write myfile "`t`i'lagc'" 
file close myfile

}

foreach i of numlist 0 1 3{

unitroot y (dem) (), lags(`i')
local p`i'lag= r(pvalmain)
local p`i'lagc: display %9.2f `p`i'lag' 

file open myfile using ${numbers}/p`i'lagc.tex, ///
 write text replace 
file write myfile "`p`i'lagc'" 
file close myfile
}

///////////////////////////////
// Replication of table 5/////
//////////////////////////////

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

// 15 lags (15 years previous dem)
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

////////////////////////
// CREATE PROGRAMS/////
///////////////////////

/////////////////////////////
/// A. REGRESSION ADJUSTMENT PROGRAM
capture program ra, eclass
quietly: tab year if ydep0!=. & tdemoc!=., gen(dumyears)
/* year -15 */
cap: teffects ra (ydep0 lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc), atet iterate(7)
if _rc==0{
matrix b=_b[ATET: r1vs0.tdemoc]
}
else{
matrix b=(.)
}
quietly: drop dumyears*
/*years -14 to -5 */
forvalues s=1(1)10{
quietly: tab year if ydep`s'!=. & tdemoc!=., gen(dumyears)
cap: teffects ra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc), atet iterate(7)
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears*
}
/*Years: -4, -3, -2, -1 */
matrix b=(b, 0, 0, 0, 0)
/*  years 0 to 30 */
forvalues s=15(1)45{
quietly: tab year if ydep`s'!=. & tdemoc!=., gen(dumyears)
cap: teffects ra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc), atet iterate(7)
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears*
}
ereturn post b
end
////////////
////////////

// B. PSR PROGRAM/////
//////////////////////
capture program psr, eclass
/* year -15 */
quietly: gen temp=tdemoc if ydep0!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)
cap: teffects ipw (ydep0) (tdemoc lag1y lag2y lag3y lag4y dumyears*, noconstant probit), atet iterate(7)
if _rc==0{
matrix b=_b[ATET: r1vs0.tdemoc]
}
else{
matrix b=(.)
}
quietly: drop dumyears* temp mtemp
/* year -14 to -2*/
forvalues s=1(1)13{
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)
cap: teffects ipw (ydep`s') (tdemoc lag1y lag2y lag3y lag4y dumyears*, noconstant probit), atet iterate(7)
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}
/* year -1  */
matrix b=(b, 0)
/*  year 0 to 30 */
forvalues s=15(1)45{
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)
cap: teffects ipw (ydep`s') (tdemoc lag1y lag2y lag3y lag4y dumyears*, noconstant probit), atet iterate(7)
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}
ereturn post b
end

//////////////////////////
//C. DOUBLY ROBUST PROGRAM
//////////////////////////

capture program drop dr
program dr, eclass
/* year -15 */
quietly: gen temp=tdemoc if ydep0!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)
cap: teffects ipwra (ydep0 lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc lag1y lag2y lag3y lag4y dumyears*, probit), atet iterate(7)
if _rc==0{
matrix b=_b[ATET: r1vs0.tdemoc]
}
else{
matrix b=(.)
}
quietly: drop dumyears* temp mtemp
/* years -14 to -5 */
forvalues s=1(1)10{
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)
cap: teffects ipwra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc lag1y lag2y lag3y lag4y dumyears*, probit), atet iterate(7)
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}
/*  years -4, -3, -2, -1  */
matrix b=(b, 0, 0, 0, 0)
/* year 0 to 30 */
forvalues s=15(1)45{
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)
cap: teffects ipwra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc lag1y lag2y lag3y lag4y dumyears*, probit), atet iterate(7)
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}
ereturn post b
end

//////////////////
////ESTIMATION ///
//////////////////
set seed 12345
//// A. Adjustment regression 
xtset, clear
bootstrap _b, reps(10) cluster(wbcode2): ra 
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
set seed 12345
xtset, clear
bootstrap _b, reps(10) cluster(wbcode2): psr
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
set seed 12345
xtset, clear
bootstrap _b, reps(10) cluster(wbcode2): dr
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
*******PART 2****************************
****************************************
****************************************
*******PART 2****************************
****************************************
****************************************
****************************************
****** PART 2***************************
****************************************
****************************************
****************************************
********PART 2***************************
****************************************
****************************************
****************************************
****************************************
****************************************
********PART 2*************************
****************************************
****************************************


/// Now it is correct

use DDCGdata_final, clear
gen tdemoc=.  
replace tdemoc=1 if dem==1 & l.dem==0 // captures the exact moment when a country transitions to democracy
replace tdemoc=0 if dem==0 & l.dem==0 

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

// Controlling for 4 GDP lags
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


*******PART 3****************************
****************************************
****************************************
*******PART 3****************************
****************************************
****************************************
****************************************
****** PART 3***************************
****************************************
****************************************
****************************************
********PART 3***************************
****************************************
****************************************
****************************************
****************************************
****************************************
********PART 3*************************
****************************************
****************************************


/// Replicate the figures

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

*******PART 4****************************
****************************************
****************************************
*******PART 4****************************
****************************************
****************************************
****************************************
****** PART 4***************************
****************************************
****************************************
****************************************
********PART 4***************************
****************************************
****************************************
****************************************
****************************************
****************************************
********PART 4*************************
****************************************
****************************************

use ${home}/DDCGdata_final, clear

// The code below identifies treated units (countries that transitioned to democracy with no reversals) and the clean controls associated to each treated unit (countries that did not have a transition before t0 and at least 20 year after)
// We will use tunit for part 6, so it will assign a missing value to all countries which experienced a kind of transition 
// ¯\_(ツ)_/¯

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
if r(min)==r(max) { // 1992 
di " `i' is treated unit, democratized in `t0'"
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



// Generate variables and prepare events datasets
forvalues i=1/4{
gen lag`i'y=l`i'.y
}

// 15 lags (15 years previous dem)
forvalues i=0/15{
local k =15-`i'
gen ydep`i'=L`k'.y-L.y
}
// 30 leads
forvalues i=16/45{
local k=`i'-15
gen ydep`i'=F`k'.y-L.y
}

save "DDCG_with_events.dta", replace
/// Save datasets

foreach var of varlist event_*{
preserve 
drop if `var'==.
keep `var' ydep* country_name countrynum dem tdemoc lag* year y yy*
order year countrynum country_name `var' tdemoc dem y ydep* lag* 
quiet su year if `var'==1
local t0=r(mean)
keep if year==`t0'
save "$auxdata/`var'.dta", replace
restore
}

********4(a)********
****** 4 (a) *******
********************
********************
********************
********************
********************
*******4 (a)********
********************
/// Proceed with estimation...


use DDCG_with_events, clear 

*** 

// This one works. 
foreach var of varlist event_*{
use "$auxdata/`var'.dta",replace

forvalues s=30/34{
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
replace b`s'=. if b`s'==0 // If the coefficient is exactly=0 it is because of problems of colinearity, idk why that happens 
}

egen average_b= rowmean(b*)

hist average_b, title (Effect of Democratization on Growth: Event Analysis) subtitle(Average 15-19 years after democratization) freq w(8) scheme(s1color) xtitle(Estimate) xlabel(#20) 
graph export "$figures/4a.png",replace

label var average_b "Average Effect 15-19 years"
estpost su average_b,d
eststo summary4a
esttab summary4a using "$tables/summary4a.tex", ///
     label noobs nonumbers nomtitles ty  booktabs replace        ///
     cell((                                        ///
            count(fmt(%9.0fc)  label(Obs.))        ///
             mean(fmt(%10.2fc) label(Mean))        ///
              p50(fmt(%8.1fc)  label(Median))      ///
               sd(fmt(%10.2fc) label(Std. Dev.))   ///
          )) ///
title(Summary of Event-Estimates) 

********4(b)********
****** 4 (b) *******
********************
********************
********************
********************
********************
*******4 (b)********
********************

/*The basic  steps are described in Ferman and Pinto’s paper (p. 457-458), which is in our syllabus Yes, you’re right it’s Figure D1, I’ll make that correction. You can use the population of the country as the M, or can simply set M=1 for all countries for simplicity (in which case the estimates are basically like Conley and Taber and don’t correct for heteroscedasticty). 

If you can’t implement the FP SEs, at the minimum, show the distribution of estimates from the events even if the standard errors aren’t “correct”  [make a note of that in your writeup.]*/
use DDCG_with_events, clear 

foreach var of varlist event_*{
di "`var'"
use "$auxdata/`var'.dta", clear
egen ydepmean= rowmean(ydep30 ydep31 ydep32 ydep33 ydep34)
rename `var' treatment
if "`var'"=="event_18_"{
quiet reg ydepmean treatment lag*, cluster(countrynum)
mat ests=(_b[treatment],r(table)[5,1],r(table)[6,1])
}


else{
cap: quiet reg ydepmean treatment lag*, cluster(countrynum)
if _rc==0{
mat ests= ests \ (_b[treatment],r(table)[5,1],r(table)[6,1])
}
else{
di "No observations"
}
}
}


svmat ests 
drop if ests2==. | ests3==.
label var ests1 "Average Effect 15-19 years"

gen evening=.
forvalues i=1/35{
replace evening=`i' in `i'
}

twoway ///
rbar ests2 ests3 evening , xline(0) scheme(s1color) horizontal color(gs12) ||  ///
scatter evening  ests1 if ests2>0 | ests3<0, mcolor(blue) || ///
scatter evening  ests1 if ests2<0 & ests3>0, mcolor(red) ///
ytitle(Event) xtitle(Estimate 15-19 years) legend(label(1 "Lower/Upper 95%"  ) label(2 "Significant") label(3 "Non-significant"))
graph export "$figures/4b.png", replace

estpost su ests1, d
eststo summary4b

	
esttab summary4b using "$tables/summary4b.tex", ///
     label noobs nonumbers nomtitles ty  booktabs replace        ///
     cell((                                        ///
            count(fmt(%9.0fc)  label(Obs.))        ///
             mean(fmt(%10.2fc) label(Mean))        ///
              p50(fmt(%8.1fc)  label(Median))      ///
               sd(fmt(%10.2fc) label(Std. Dev.))   ///
          )) ///
title(Summary of Event-Estimates) 




**** Do Conley Taber ¯\_(ツ)_/¯

****************
****** Conley Taber

// Prepare dataset, same than as the beginning of par 4 but keeping all years for treated countries

********4(c)********
********4 (c) *******
********************
********************
********************
********************
********************
********4 (c)********
********************

// Stack in wide format
use DDCG_with_events, clear 


// Generate FE
foreach var of varlist event_*{
use "$auxdata/`var'.dta", clear
replace `var'=1 if `var'==0
replace tdemoc=0 if tdemoc==.
drop year
sa "$auxdata/fe_`var'.dta", replace
}

use DDCG_with_events, clear 
foreach var of varlist event_*{

if "`var'"=="event_18_"{
use "$auxdata/fe_`var'.dta", clear
}
else{
append using "$auxdata/fe_`var'.dta"
}
}

foreach var of varlist event_*{
replace `var'=0 if `var'==.
}
order event_*

capture program drop  fe_event 
program fe_event, eclass 
forvalues s=30/34{
local k= `s'-29
quiet reg ydep`s' tdemoc lag*  event_*, cluster (countrynum)
di _b[tdemoc]
est sto r`k'
if `s'==30 {
mat b=(_b[tdemoc])
local v`k'=e(V)[1,1]
} 
else{
mat b=(b, _b[tdemoc])
local v`k'=e(V)[1,1]
}
}
matrix V=J(5,5,1)
matrix V[1,1]=`v1'
matrix V[2,2]=`v2'
matrix V[3,3]=`v3'
matrix V[4,4]=`v4'
matrix V[5,5]=`v5'
matrix rownames V= "c1" "c2" "c3" "c4" "c5"
mat list V
mat list b

ereturn post b V
*ereturn post V
end

//bootstrap standard errors
set seed 12345
bootstrap _b: fe_event
nlcom (tdemoc: (_b[c1]+_b[c2]+_b[c3]+_b[c4] +_b[c5])/5), post
eststo avb

// cluster standard errors
fe_event
nlcom (tdemoc: (_b[c1]+_b[c2]+_b[c3]+_b[c4]+_b[c5])/5), post
eststo avc   

esttab r1 r2 r3 r4 r5 avc avb using "$tables/4c.tex", replace keep(tdemoc)  se nostar varlabels (tdemoc "Avg. effect on log GDP") b(3)   wrap nonotes ty mlabels("15 years" "16 years" "17 years" "18 years" "19 years" "Av." "Av.(Bootstrap)")

///  GET A FIGURE LIKE 4 WITHOUT WEIGHTS USING ALL EVENTS////////
/// ////////////////////////////////////////////////////////////

/// Data win Wide format with FE
use DDCG_with_events, clear 
foreach var of varlist event_*{

if "`var'"=="event_18_"{
use "$auxdata/fe_`var'.dta", clear
}
else{
append using "$auxdata/fe_`var'.dta"
}
}

foreach var of varlist event_*{
replace `var'=0 if `var'==.
}
order event_*

//// Estimation

forvalues s=0/45{
quiet reg ydep`s' tdemoc i.event_*  lag*, cluster(countrynum)
if `s'==0{
mat ests=(_b[tdemoc], r(table)[5,1],r(table)[6,1])
}
else{
mat ests=ests\(_b[tdemoc], r(table)[5,1],r(table)[6,1])
mat colnames ests="b" "ll" "ul"
}
}

svmat ests
cap drop yad
gen yad=. 
forvalues i=0/45{
local k=`i'-14
local j=`i'+1
replace yad=`k' in `j'
}

line ests1 yad, lcolor(black) scheme(s1color) xtitle(Years around democratization) ytitle(Change in GDP per capita log points) yline(0, lcolor(black) lpattern(dash))||line ests2 yad,  lcolor(gray) lpattern(dash)||line ests3 yad,  lcolor(gray) lpattern(dash) legend(off)
graph export "$figures/part5no_weights.png", replace


*******PART 5***************************
****************************************
****************************************
*******PART 5***************************
****************************************
****************************************
****************************************
****** PART 5***************************
****************************************
****************************************
****************************************
********PART 5**************************
****************************************
****************************************
****************************************
****************************************
****************************************
********PART 5**************************
****************************************
****************************************


///////////// Get synth_events ///////////
//////////////////////////////////////////

use DDCG_with_events, clear

foreach var of varlist event_*{
preserve
di "`var'" 
quiet su year if `var'==1
local t0 = r(mean)
sca t0= r(mean)
sca t10=t0-10
sca t15=t0-15
quiet su countrynum if `var'== 1
sca tcountry=r(mean)
quietly drop if `var'==.
quietly drop if year<t10 // Critical 1

egen countrynum2=group(country_name)
quiet su countrynum2 
local mincountry=r(min)
local maxcountry=r(max)
forvalues i=`mincountry'/`maxcountry'{
quiet su year if countrynum2==`i'
local N=r(N)
local N5= `N'-10 // Critical 2
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
rename `var'  synth_`var'

keep synth_`var' ydep* country_name countrynum dem tdemoc lag* year y yy*
order year synth_`var' countrynum country_name  tdemoc dem y ydep* lag* 
keep if year==`t0'
replace synth_`var'=1 
replace tdemoc=0 if tdemoc==.
save "$auxdata/synth_`var'.dta", replace
restore
}


//// Get weights and merge with events ////
use DDCG_with_events, clear 
xtset countrynum year
foreach var of varlist  event_*{
preserve
quiet su year if `var'==1
local t0 = r(mean)
sca t0= r(mean)
sca t10=t0-10
sca t15=t0-15
quiet su countrynum if `var'== 1
sca tcountry=r(mean)
replace `var'=1 if `var'==. & countrynum==tcountry
quietly drop if `var'==.
quietly drop if year<t10 // Critical 1
egen countrynum2=group(country_name)
order countrynum2 
quiet su countrynum2 
local mincountry=r(min)
local maxcountry=r(max)
forvalues i=`mincountry'/`maxcountry'{
quiet su year if countrynum2==`i'
local N=r(N)
local N5= `N'-10 // Critical 2
quiet su y if countrynum2==`i'
if r(N)<`N' {
quietly drop if countrynum2==`i' 
}
else{
}
}
*/
forvalues k=1/10{
local t`k'=t0-`k'
}
local t0=t0
local tcountry=tcountry
//Controls must have y data for at least 10 years before treatment
cap: quiet synth y  y(`t1') y(`t2') y(`t3') y(`t4'), trunit(`tcountry')  trperiod(`t0') keep("$auxdata/s1_`var'", replace) 
if _rc==0{
use "$auxdata/s1_`var'.dta", clear
rename _Co_Number countrynum   
rename _W_Weight weight 
quietly keep countrynum weight
quietly drop if countrynum==. & weight==.
quietly merge 1:1 countrynum using "$auxdata/synth_`var'.dta"
quietly drop _merge 
save "$auxdata/synth_weight_`var'.dta", replace
}
else{
di "s1 didn't run `var'"
}
restore
}
/////
/// Repeat for S2 with 10 pre-treatment outcomes as predictors (couldn't) find a way to do it more economically
use DDCG_with_events, clear 
xtset countrynum year

foreach var of varlist  event_*{
preserve
quiet su year if `var'==1
local t0 = r(mean)
sca t0= r(mean)
sca t10=t0-10
sca t15=t0-15
quiet su countrynum if `var'== 1
sca tcountry=r(mean)
replace `var'=1 if `var'==. & countrynum==tcountry
quietly drop if `var'==.
quietly drop if year<t10 // Critical 1
egen countrynum2=group(country_name)
order countrynum2 
quiet su countrynum2 
local mincountry=r(min)
local maxcountry=r(max)
forvalues i=`mincountry'/`maxcountry'{
quiet su year if countrynum2==`i'
local N=r(N)
local N5= `N'-10 // Critical 2
quiet su y if countrynum2==`i'
if r(N)<`N' {
quietly drop if countrynum2==`i' 
}
else{
}
}
*/
forvalues k=1/10{
local t`k'=t0-`k'
}
local t0=t0
local tcountry=tcountry
//Controls must have y data for at least 10 years before treatment
cap: quiet  synth  y y(`t1') y(`t2') y(`t3') y(`t4') y(`t5') y(`t6') y(`t7') y(`t8') y(`t9') y(`t10') , trunit(`tcountry')  trperiod(`t0') keep("$auxdata/s2_`var'", replace) 
if _rc==0{
di "s2 did run `var'"
use "$auxdata/s2_`var'", clear
rename _Co_Number countrynum   
rename _W_Weight weight 
quietly keep countrynum weight
quietly drop if countrynum==. & weight==.
quietly merge 1:1 countrynum using "$auxdata/synth_`var'.dta"
quietly drop _merge 
save "$auxdata/synth2_weight_`var'.dta", replace 
}
else{
di "s2 didn't run `var'"
}
restore
}

////////////////////////////////
//// ESTIMATE USING WEIGHTS 1///
////////////////////////////////

use DDCG_with_events, clear 
foreach var of varlist event_*{

if "`var'"=="event_18_"{
use "$auxdata/synth_weight_`var'.dta", clear
}
else{
cap: append using "$auxdata/synth_weight_`var'.dta"
}
}

foreach var of varlist synth_event_*{
replace `var'=0 if `var'==.
}

replace weight=1 if weight==.
//// Estimation

forvalues s=0/45{
quiet reg ydep`s' tdemoc lag* i.synth_event_*   [aw=weight], cluster(countrynum)
if `s'==0{
mat ests=(_b[tdemoc], r(table)[5,1],r(table)[6,1])
}
else{
mat ests=ests\(_b[tdemoc], r(table)[5,1],r(table)[6,1])
mat colnames ests="b" "ll" "ul"
}
}

svmat ests
cap drop yad
gen yad=. 
forvalues i=0/45{
local k=`i'-14
local j=`i'+1
replace yad=`k' in `j'
}

line ests1 yad, lcolor(black) scheme(s1color) xtitle(Years around democratization) ytitle(Change in GDP per capita log points) yline(0, lcolor(black) lpattern(dash))||line ests2 yad,  lcolor(gray) lpattern(dash)||line ests3 yad,  lcolor(gray) lpattern(dash) legend(off)
graph export "$figures/part5s1_weights.png", replace

////////////////////////////////
//// ESTIMATE USING WEIGHTS 2///
////////////////////////////////


use DDCG_with_events, clear 
foreach var of varlist event_*{

if "`var'"=="event_18_"{
use "$auxdata/synth2_weight_`var'.dta", clear
}
else{
cap: append using "$auxdata/synth2_weight_`var'.dta"
}
}

foreach var of varlist synth_event_*{
replace `var'=0 if `var'==.
}

replace weight=1 if weight==.
//// Estimation

forvalues s=0/45{
quiet reg ydep`s' tdemoc lag* i.synth_event_*   [aw=weight], cluster(countrynum)
if `s'==0{
mat ests=(_b[tdemoc], r(table)[5,1],r(table)[6,1])
}
else{
mat ests=ests\(_b[tdemoc], r(table)[5,1],r(table)[6,1])
mat colnames ests="b" "ll" "ul"
}
}

svmat ests
cap drop yad
gen yad=. 
forvalues i=0/45{
local k=`i'-14
local j=`i'+1
replace yad=`k' in `j'
}

line ests1 yad, lcolor(black) scheme(s1color) xtitle(Years around democratization) ytitle(Change in GDP per capita log points) yline(0, lcolor(black) lpattern(dash))||line ests2 yad,  lcolor(gray) lpattern(dash)||line ests3 yad,  lcolor(gray) lpattern(dash) legend(off)
graph export "$figures/part5s2_weights.png", replace


********************
****** PART 6*******
********************