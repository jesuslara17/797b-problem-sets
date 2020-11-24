/// 			797B Jesús Lara				 ///
///  Critical Replication and extension of   ///
///DDCG by Acemoglu, Naidu, Restrepo & Robinson///

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

///////////////////////////////
// Replication of table 5/////
//////////////////////////////

use DDCGdata_final, clear

// Generate Treatment democracy variable td
g tdemoc=.  
replace tdemoc=1 if dem==1 & l.dem==0 // captures the exact moment when a country transitions to democracy
replace tdemoc=0 if dem==0 & l.dem==0 


// 1) 4 lags in GDP. These will be covariates in our regression
forvalues i=1/4{
gen lag`i'y=l`i'.y
}
// 2) Now generate our dependent variables: \Delta y^s_ct= y^s_ct -y_t-1
// t: period of democratization, s: years after democratization 
// We consider: 15 years before democratization, 30 years after democratization
// Democratization happens at g=15. So s=15-g 

gen ydep0=L15.y-L.y
gen ydep1=L14.y-L.y
gen ydep2=L13.y-L.y
gen ydep3=L12.y-L.y
gen ydep4=L11.y-L.y
gen ydep5=L10.y-L.y
gen ydep6=L9.y-L.y
gen ydep7=L8.y-L.y
gen ydep8=L7.y-L.y
gen ydep9=L6.y-L.y
gen ydep10=L5.y-L.y
gen ydep11=L4.y-L.y
gen ydep12=L3.y-L.y
gen ydep13=L2.y-L.y
gen ydep14=0                        

/* Creates leads of GDP */
gen ydep15=y-L.y
gen ydep16=F1.y-L.y
gen ydep17=F2.y-L.y
gen ydep18=F3.y-L.y
gen ydep19=F4.y-L.y
gen ydep20=F5.y-L.y
gen ydep21=F6.y-L.y
gen ydep22=F7.y-L.y
gen ydep23=F8.y-L.y
gen ydep24=F9.y-L.y
gen ydep25=F10.y-L.y
gen ydep26=F11.y-L.y
gen ydep27=F12.y-L.y
gen ydep28=F13.y-L.y
gen ydep29=F14.y-L.y
gen ydep30=F15.y-L.y
gen ydep31=F16.y-L.y
gen ydep32=F17.y-L.y
gen ydep33=F18.y-L.y
gen ydep34=F19.y-L.y
gen ydep35=F20.y-L.y
gen ydep36=F21.y-L.y
gen ydep37=F22.y-L.y
gen ydep38=F23.y-L.y
gen ydep39=F24.y-L.y
gen ydep40=F25.y-L.y
gen ydep41=F26.y-L.y
gen ydep42=F27.y-L.y
gen ydep43=F28.y-L.y
gen ydep44=F29.y-L.y
gen ydep45=F30.y-L.y        



//Remove observations of democracies after the democratization ? 
keep if tdemoc!=.
// Question: by doing this we are removing all observations of countries that democratized after the democratization... how are 
set seed 12345                       

//// A. Adjustment regression 

xtset, clear
bootstrap _b, reps(2) cluster(wbcode2): tefpas_ra 
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
bootstrap _b, reps(2) cluster(wbcode2): tefpas_ipw 
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
bootstrap _b, reps(2) cluster(wbcode2): tefpas_ipwra 
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



********************
****** PART 2*******
********************

********************
****** PART 3*******
********************

********************
****** PART 4*******
********************

****** 4 (a)********
********************

****** 4 (b)********
********************

****** 4 (c)********
********************


********************
****** PART 5*******
********************

********************
****** PART 6*******
********************