//// 797B Problem Set 3 /////
////// Jesús Lara    ////////

*Problem 1: weak first stage simulations - OLS, 2SLS, CLR, LIML and LASSO 

/// Create program that generates our data

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

/// Test: generate estimators

simulation 20


/// OLS
regress y x, robust
*bias:
sca bias_ols= _b[x]-1
* Type 1 error? 1 if yes 0 if not
test x=1
if r(p)<0.05{
scalar ols_error=1
}
else{
scalar ols_error=0
}

/// 2SLS 
ivreg2 y (x=z_*), robust 
*bias:
sca bias_iv=_b[x]-1
* Type 1 error? 1 if yes 0 if not
test x=1
if r(p)<0.05{
scalar iv_error=1
}
else{
scalar iv_error=0
}

/// CLR weakiv
quiet ivreg2 y (x=z_*), robust 
weakiv, null(1)
if e(clr_p)<0.05{
scalar clr_error=1
}
else{
scalar clr_error=0
}

/// LIML

ivreg2  y (x=z_*), robust liml
sca bias_liml= _b[x]-1

test x=1
if r(p)<0.05{
scalar liml_error=1
}
else{
scalar liml_error=0
}


/// Lasso 
poivregress y (x=z_*)

if e(k_inst_sel)>=1{
sca lasso_noinstr=0

sca bias_lasso= _b[x]-1
test x=1
if r(p)<0.05{
scalar lasso_error=1
}
else{
scalar lasso_error=0
}

else {
sca lasso_noinstr=1
}

}






