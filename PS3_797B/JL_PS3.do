//// 797B Problem Set 3 /////
////// Jesús Lara    ////////

*Problem 1: weak first stage simulations - OLS, 2SLS, CLR, LIML and LASSO 

/// Create program that generates our data
clear

cap program drop simulation
program simulation, eclass
args Q

clear
cap drop eta eps
drawnorm eta eps, n(1000) cov (1, 0.8 \ 0.8, 1)

forvalues j=1/`Q'{
cap drop z_`j'
gen z_`j'= rnormal(0,1) 
}

cap drop x 
gen x=0.1*z_1+eps
cap drop y
gen y=x+eta 

end

/// Test: generate estimators, no loop


///////////////////////////////////
///////////////////////////////////
///////////////////////////////////
/// Loop 100 times/////////////////
///////////////////////////////////
///////////////////////////////////

set seed 12345

forvalues i=1/1000{

foreach Q in 1 10 20  { //Q values

di "iteration `i' for Q=`Q'" //will display the interation and Q value 

quiet{
simulation `Q'

/// OLS
regress y x, robust
*bias:
sca ols_bias_`Q'_`i'= _b[x]-1
* Type 1 error? 1 if yes 0 if not
test x=1
if r(p)<0.05{
scalar ols_error_`Q'_`i'=1
}
else{
scalar ols_error_`Q'_`i'=0
}

/// 2SLS 
ivreg2 y (x=z_*), robust 
*bias:
sca iv_bias_`Q'_`i'=_b[x]-1
* Type 1 error? 1 if yes 0 if not
test x=1
if r(p)<0.05{
scalar iv_error_`Q'_`i'=1
}
else{
scalar iv_error_`Q'_`i'=0
}

/// CLR weakiv
quiet ivreg2 y (x=z_*), robust 
sca clr_bias_`Q'_`i'=_b[x]-1

weakiv, null(1)
if e(clr_p)<0.05{
scalar clr_error_`Q'_`i'=1
}
else{
scalar clr_error_`Q'_`i'=0
}

/// LIML

ivreg2  y (x=z_*), robust liml
sca liml_bias_`Q'_`i'= _b[x]-1

test x=1
if r(p)<0.05{
scalar liml_error_`Q'_`i'=1
}
else{
scalar liml_error_`Q'_`i'=0
}


/// Lasso 
poivregress y (x=z_*)

if e(k_inst_sel)>=1{
sca lasso_noinstr_`Q'_`i'=0

sca lasso_bias_`Q'_`i'= _b[x]-1
test x=1
if r(p)<0.05{
scalar lasso_error_`Q'_`i'=1
}
else{
scalar lasso_error_`Q'_`i'=0
}
}
else {
sca lasso_noinstr_`Q'_`i'=1
sca lasso_bias_`Q'_`i'= .
scalar lasso_error_`Q'_`i'=.

}
}
}
}


/// Generate variables with my scalars
clear
set obs 1000

global esps ols iv clr liml lasso
global vars bias error

foreach  esp in $esps{
foreach v in $vars{
foreach Q in 1 10 20  { 


cap drop  `esp'_`v'_`Q'
gen `esp'_`v'_`Q'=.
 
forvalues i=1/1000{
replace  `esp'_`v'_`Q'= `esp'_`v'_`Q'_`i' in `i'
} 
 
} 
}
}

/// Lasso no instr 
foreach Q in 1 10 20  { 

cap drop  lasso_noinstr_`Q'
gen lasso_noinstr_`Q'=.
 
forvalues i=1/1000{
replace  lasso_noinstr_`Q'= lasso_noinstr_`Q'_`i' in `i'
} 
} 



/// Get means and medians

foreach esp in $esps{
foreach v in $vars{
foreach Q in 1 10 20  { 


su `esp'_`v'_`Q', detail
di r(mean)
di r(p50)
sca a_`esp'_`v'_`Q'= r(mean)*100
sca m_`esp'_`v'_`Q'= r(p50)

}

}
}

/// Get means of lasso no instr 

foreach Q in 1 10 20  { 

su lasso_noinstr_`Q'
sca a_lasso_noinstr_`Q'=r(mean)
}

///_`Q'= (m_ols_bias_`Q', a_ols_error_`Q' \ m_iv_bias_`Q', a_iv_error_`Q' \ m_clr_bias_`Q', a_clr_error_`Q'\ m_liml_bias_`Q', a_liml_error_`Q'\ m_lasso_bias_`Q', a_lasso_error_`Q')

}


mat table1= (table1_1, table1_10, table1_20)
mat rownames table1= "OLS" "2SLS" "CLR" "LIML" "Lasso^a"
mat colnames table1= "Bias" "Type 1 Error (\%)" "Bias" "Type 1 Error (\%)" "Bias" "Type 1 Error (\%)"

mat list table1


cd "C:\Users\User\Documents\GitHub\797b-problem-sets\PS3_797B"



esttab m(table1, fmt(%9.3f)) using "Table1B.tex", replace title(Different estimations) nomtitles booktabs gaps fragment


// Export numbers of Lasso don't picking any instrument

foreach Q in  1 10 20 {
 

local lasso`Q' = a_lasso_noinstr_`Q'*100
local cleaned_lasso`Q': display %9.2f `lasso`Q'' 
display "`lasso`Q'' "

file open myfile using cleaned_lasso`Q'.tex, ///
 write text replace 
file write myfile "`cleaned_lasso`Q''" 
file close myfile
}