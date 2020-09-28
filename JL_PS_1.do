*DO-FILE PS 1

* Jesús Lara Jáuregui

		

clear
global home "C:\Users\User\Documents\GitHub\797b-problem-sets"
cd "$home" 
set more off

texdoc init 797B_PS1_JL, replace


/*tex
\documentclass[12pt]{article}
\usepackage{stata}
\usepackage{graphicx}
\usepackage{geometry}
\usepackage{rotating}
\begin{document}







\author{Jesús Lara Jáuregui}
\title{Problem Set 1}
\maketitle


tex*/






		*********************
		***** PROBLEM 1 *****
		*********************

/*tex 
\section{Problem 1. OLS in MATA}
\subsection{Part 1}
tex*/


/*Part 1*****. Write an e-class program “myreg1” which takes in a varlist and performs an OLS regression of y on X
on as follows*/





use census_sample_30_50, clear 

label variable lnwage "Wage (log)"
label variable exp Experience
label variable exp2 Experience2
//(a)


* My program
cap program drop myreg1
program myreg1, eclass
syntax varlist 
tokenize `varlist'
* (a) define locals "y" and "X" using macro shift
  local y "`1'"    
  macro shift 1
  local X "`*'"
  
*using MATA to define y and X matrices
   
* b) open MATA, and using “st_view”, define the y and X matrices within MATA

   mata: M=y=X=V=.
   mata:st_view(M, .,("`y'" , "`X'"), 0) 
   mata:st_subview(y,M,.,1)
   mata:st_subview(X,M,.,(2\.)) 
   mata:n=rows(X)
   mata:k=cols(X)
   mata: c=J(rows(X),1,1)
   


   // c) calculating the coefficent vector (beta hat) using matrix operations

   mata:XX = cross(X,1  ,  X,1)
   mata:Xy = cross(X,1  , y,0)
   mata:b=invsym(XX)*Xy

 // (d) calculate OLS variance estimate of β under homoscedasticity :  V (β) = 1

   mata: e=y-(X,c)*b
   
   mata: s2=cross(e,e)/(n-k) 
   mata: V=s2*invsym(XX)

   // (e) pass the vector βˆ and matrix ˆ V (β) back to Stata
   
   mata: st_matrix("V",V)
   mata:st_matrix( "b" , b) 
 // show the b matrix
//(f) post vector βˆ and matrix ˆ V (β) uisng ereturn post in the e(b) and e(V) matrices

   matrix list b
   matrix list V
   
  
end

// (g) confirm your answers are correct by checking answer vis a vis the command: regress y x1 x2 x3

tex\ Results with myreg1

texdoc stlog
myreg1 lnwage hieduc exp exp2 
texdoc stlog close

tex\ Results with Stata OlS command

texdoc stlog
quiet reg lnwage hieduc exp exp2
matrix list e(b)
matrix list e(V)
texdoc stlog close





********

*****PART 2

/*tex
\subsection{Part 2}
tex*/

cap program drop myreg2
program myreg2, eclass
syntax varlist [if] [in]
tokenize `varlist'

// a) define locals “y” and “X” using macro shift

  local y "`1'"
  macro shift 1
  local X "`*'"
  
//(b) open MATA, and using “st_view”, define the y and X matrices within MATA
   
   mata: M=y=X=V=.
   mata:st_view(M, .,("`y'" , "`X'"), 0)
   mata:st_subview(y,M,.,1)
   mata:st_subview(X,M,.,2\.)
   mata: c=J(rows(X),1,1)
   mata: X=(X,c)

   mata: myols(y, X)

   //f) post vector βˆ and matrix ˆ V (β) uisng ereturn post in the e(b) and e(V) matrices
   
 ereturn post `b'
   matrix list b
   matrix list V
end

// (c) calculate OLS coefficient vector βˆ 

cap mata mata drop myols()
mata:
void myols(matrix y, matrix X){

b=SXX=SXy=G=.

// (d) calculate OLS variance estimate of βˆ under arbitrary heteroscedasticity

// X[i,.] Dimension 1x(k+1) 
// X[i,.]' Dimension (k+1)x1 

for(i=1; i<=rows(X); i++){
			if(i==1){
				SXX= X[i,.]'*X[i,.] // Dimension (k+1)x(k+1)
				SXy= X[i,.]'*y[i,1] // Dimension (k+1)x1
			}
else {

				SXX=SXX + (X[i,.]')*X[i,.] // Dimension (k+1)x(k+1)
				SXy=SXy + (X[i,.]')*y[i,1] // Dimension (k+1)x1
			}
		}
b=invsym(SXX)*(SXy)  // Dimension (k+1)x1	
e=y-X*b
//Dimension nx1
		
for(i=1; i<=rows(X); i++){
			if(i==1){
				G=(e[i,1]*e[i,1])*(X[i,.]')*X[i,.] // Dimension (k+1)x(k+1)
			}
else {
				G=G+(e[i,1]*e[i,1])*(X[i,.]')*X[i,.] //
				
			}
		}

		
		 
		 
		r=(rows(X)/(rows(X)-rows(b))) // Dim (1x1)
	    V=invsym(SXX)*(r*G)*invsym(SXX) // 
		
//(e) pass the vector βˆ and matrix ˆ V (β) back to Stata
st_matrix("b", b)
st_matrix("V",V)

}


end

// (g) confirm your answers are correct by checking answer vis a vis the command: regress y x1 x2 x3, robust

tex \ Results with myreg2

texdoc stlog

myreg2 lnwage hieduc exp exp2 
quiet reg lnwage hieduc exp exp2, robust

texdoc stlog close

tex \ Results with Stata's OLS and robust standard errors

texdoc stlog

matrix list e(b)
matrix list e(V)

texdoc stlog close

*****************
*** PROBLEM 2 ***
*****************


/*tex

\section{Problem 2. Poisson using Maximum Likelihood}

If $y_i$ is distributed Poission with mean $exp(X^{'}_i \beta)$, hence the likelihood function for a sample of $N$ observations is given by:

$$ L(\beta)=\prod_{i=1}^{N}\frac{1}{y_{i}!}exp((X^{'}\beta)y_i)exp(-exp(X^{'}\beta))$$


And taking logs we get:

$$lnL(\beta)=\sum_{i}^{N}[-exp(X^{'}\beta)+y_i exp(X^{'}\beta)-ln(y_i !)]$$

Which is the form we use for pur maximum-likelihood estimation
tex*/

*1Program an .ado file called “mypois.ado” that estimates poisson regression using maximum likelihood

cap program drop mypois_eval
program mypois_eval , eclass
args todo b lnf

tempvar theta

mleval `theta' = `b'

local t "$ML_y1"


mlsum `lnf' = (-1)*exp(`theta') + `t'*`theta' + (-1)*lnfactorial(`t')

end 


cap program drop  mypois 
program mypois,eclass
syntax varlist(numeric fv) 
tokenize `varlist'
  local y "`1'"
  macro shift 1
  local X "`*'"

ml model d0 mypois_eval (`y' = `X')
ml maximize
end
 

 
*2. Using data from http://www.ats.ucla.edu/stat/stata/dae/poisson_sim , do the following:
use https://stats.idre.ucla.edu/stat/stata/dae/poisson_sim, clear

*3. Show a histogram of the outcome (num_awards), and report the mean and variance. 

texdoc stlog 
hist(num_awards), title("Number of Awards") color("orange")
texdoc stlog close 
texdoc graph

quiet summarize(num_awards)

sca mean=r(mean)
sca variance= r(Var)

mat P2_MV=(mean,variance)
mat colnames P2_MV="Mean" "Variance"
mat rownames P2_MV="Number of awards"

esttab m(P2_MV,fmt(%9.2f)) using P2_MV.tex, replace title(Number of Awards) nomtitles booktabs

tex \input{P2_MV.tex}
*5. Estimate coefficients using built in command: poisson num_awards i.prog math Confirm 4 and 5 give the same results

mypois num_awards i.prog math
estimates store mypois

*3 :O

*4
poisson num_awards i.prog math 
estimates store Stata_Poisson

esttab Stata_Poisson mypois using Table_P2.tex, b(%7.4f) se(%7.4f) label replace compress mtitle("Stata Poisson" "mypois") title(Poisson Estimation)

tex \input{Table_P2.tex}


****************
****************
****Problem3****
****************
****************
/*tex 
\section{Problem 3. Mean Squared Error simulation - Sample Size and Distribution}

tex*/


* 3 Part 1
clear 

cap program drop OLSPOIS
program OLSPOIS, eclass 
args N sigma


matrix Sigma=(`sigma', `sigma'/2 \ `sigma'/2, `sigma')

drawnorm X1 X2, n(`N') cov(`Sigma')

gen y=rpoisson(exp(2*X1-X2))
gen ly=ln(y)


quiet regress ly X1 X2

sca MSE_OLS= 0.5*((_b[X1]-2)^2 + (_b[X2]-(-1))^2)

quiet poisson y X1 X2 

sca MSE_POIS= 0.5*((_b[X1]-2)^2 + (_b[X2]-(-1))^2)

sca list MSE_OLS MSE_POIS

drop X1 X2 y ly 
end


*******Running the program



set seed 5555 

local j=1
quiet{

foreach N of numlist 50 1000{
foreach sigma of numlist 0.01 0.1 1{

forvalues i=1/10{
di `i'

OLSPOIS `N' `sigma'

sca MSE_OLS_`i'= MSE_OLS
sca MSE_POIS_`i'= MSE_POIS 

if `i'==1 {
matrix sim_`j'= (MSE_OLS_`i',MSE_POIS_`i')
}

else {
matrix sim_`j'= sim_`j' \ [MSE_OLS_`i', MSE_POIS_`i']
}


}
local j=`j'+1
}

}


}

*1-3 N=50 (.01, .1 1)
*4_6 N=1000 (.01, .1 1)
  

*** Getting matrix of averages averages
forvalues i=1/6{
mata: sim_`i'=st_matrix("sim_`i'")
mata: MOLS_`i'=mean(sim_`i'[.,1])
mata: MPOIS_`i'=mean(sim_`i'[.,2])
}

*rows N(50,1000) *columns(0.01,0.1,1)

mata: MS=(MOLS_1,MPOIS_1,MOLS_2,MPOIS_2,MOLS_3,MPOIS_3 \ MOLS_4,MPOIS_4,MOLS_5,MPOIS_5,MOLS_6,MPOIS_6)

mata: st_matrix("MS",MS)

matrix rownames MS= "N=50" "N=1000"
matrix colnames MS= "0.01 OLS" "0.01 POIS" "0.1 OLS" "0.1 POIS" "1 OLS" "1 POIS"

esttab m(MS) using MS.tex, replace title(Average of the squared error (MSE): OLS and Poisson) nomtitles booktabs

tex \input{MS.tex}


****************
****************
*** PROBLEM 4***
****************
****************

/*tex 
\section{Problem 4. Small number of clusters - Wild Bootstrap}
tex*/

**** Making the program 


cap program drop randsim
program randsim, rclass
syntax varlist(numeric fv) 
tokenize `varlist'
  local y "`1'"
  
  local unit "`2'"
 
  local t "`3'"


*randomization 
quiet{
egen `unit'_id= group(`unit')
gen random=.


forvalues i=1/25 {
sca r_`i'=runiform(0,1)
replace random = r_`i' if `unit'_id==`i'
}
gen treatment=0
replace treatment=1 if random<=0.25
}
*Generate placebo 

gen `y'2= `y'+0.05*treatment

***Run regressions

foreach var of varlist `y' `y'2{

quiet su treatment
if r(mean)>0 {

sca no_treated=0
 
quiet reg `var' treatment i.`unit' i.`t', vce(cluster `unit') 
quiet boottest treatment, nogr
local pvalue_`var'= 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
local bpvalue_`var'=r(p)

if `pvalue_`var''<0.05 {
   sca sig_`var'=1
}
else{
   sca sig_`var'=0
}

if `bpvalue_`var''<0.05 {
	sca bsig_`var'=1
}
else{
 
sca bsig_`var'=0
}

sca list sig_`var' bsig_`var'
}

else {

sca no_treated=1
sca sig_`var'=0
sca bsig_`var'=0
}
}
drop cbsa_id `y'2 random treatment

end 


use 25_MSA_panel, clear
keep if naics==10
xtset cbsa time 

*4.1


randsim lnemp cbsa time   


*** (b) Run the loop

* remember to put 1000 sims

forvalues i=1/10{
di `i'
randsim lnemp cbsa time 

sca sig_lnemp_`i'= sig_lnemp
sca bsig_lnemp_`i'= bsig_lnemp
sca sig_lnemp2_`i'= sig_lnemp2
sca bsig_lnemp2_`i'= bsig_lnemp2
sca no_treated_`i'=no_treated

if `i'== 1 {
matrix S=(sig_lnemp_`i',bsig_lnemp_`i',sig_lnemp2_`i',bsig_lnemp2_`i',no_treated_`i')
} 
else {

matrix S= S \ [sig_lnemp_`i',bsig_lnemp_`i',sig_lnemp2_`i', bsig_lnemp2_`i',no_treated_`i']
}

}


mata: S=st_matrix("S")
mata: s_1r=sum(S[.,1])
mata: s_1b=sum(S[.,2])
mata: s_2r=sum(S[.,3])
mata: s_2b=sum(S[.,4])
mata: s_nt=sum(S[.,5])

*rows: lnemp and lnemp2
*columns: cluster vs bootstrap
mata: F_1=(s_1r,s_1b\s_2r,s_2b)
mata: F_1 

mata: F_0=J(2,2,rows(S))-(F_1+J(2,2,s_nt))
mata: F_0


mata:st_matrix("F_1",F_1)
mata:st_matrix("F_0",F_0)

mat rownames F_1="lnemp" "lnemp2"
mat colnames F_1="Cluster" "Bootstrap"

mat rownames F_0="lnemp" "lnemp2"
mat colnames F_0="Cluster" "Bootstrap"

esttab m(F_1) using F_1.tex, replace title(Coefficient of treatment significant? Frecuency) nomtitles booktabs

esttab m(F_0) using F_0.tex, replace title(Coefficient of treatment insignificant? Frecuency) nomtitles booktabs

tex \input{F_1.tex}
tex \input{F_0.tex}
*Rows: lnemp lnemp2
*Columns: robust vs bootstrap
 

*(c) just keep a few cbsa
/*New York, Los Angeles, Boston, Dallas, Miami, Chicago, Pittsburgh, Philadelphia*/
use 25_MSA_panel, clear
keep if naics==10
keep if cbsa== 35620 | cbsa== 31100 | cbsa== 14460 | cbsa== 19100 | cbsa== 33100 | cbsa==16980 | cbsa==38300 | cbsa==37980

***repeat
forvalues i=1/10{
di `i'
randsim lnemp cbsa time 

sca sig_lnemp_`i'= sig_lnemp
sca bsig_lnemp_`i'= bsig_lnemp
sca sig_lnemp2_`i'= sig_lnemp2
sca bsig_lnemp2_`i'= bsig_lnemp2
sca no_treated_`i'=no_treated

if `i'== 1 {
matrix S=(sig_lnemp_`i',bsig_lnemp_`i',sig_lnemp2_`i',bsig_lnemp2_`i',no_treated_`i')
} 
else {

matrix S= S \ [sig_lnemp_`i',bsig_lnemp_`i',sig_lnemp2_`i', bsig_lnemp2_`i',no_treated_`i']
}

}


mata: S=st_matrix("S")
mata: s_1r=sum(S[.,1])
mata: s_1b=sum(S[.,2])
mata: s_2r=sum(S[.,3])
mata: s_2b=sum(S[.,4])
mata: s_nt=sum(S[.,5])

*rows: lnemp and lnemp2
*columns: cluster vs bootstrap
mata: F_1=(s_1r,s_1b\s_2r,s_2b)
mata: F_1 

mata: F_0=J(2,2,rows(S))-(F_1+J(2,2,s_nt))
mata: F_0

mata:st_matrix("F_1",F_1)
mata:st_matrix("F_0",F_0)

mat rownames F_1="lnemp" "lnemp2"
mat colnames F_1="Cluster" "Bootstrap"

mat rownames F_0="lnemp" "lnemp2"
mat colnames F_0="Cluster" "Bootstrap"

esttab m(F_1) using F_1_R.tex, replace title(Coefficient of treatment significant? Frecuency (few clusters)) nomtitles booktabs

esttab m(F_0) using F_0_R.tex, replace title(Coefficient of treatment insignificant? Frecuency (few clusters)) nomtitles booktabs

tex \input{F_1_R.tex}
tex \input{F_0_R.tex}


*/


/*tex

\end{document}

tex*/

**********************
******* PROBLEM 5*****
**********************

tex \section{Problem 5.  OLS, matching, propensity scores}

use census_sample_30_50,clear

*Generate treatment variable
gen FB=0
replace FB=1 if bpld>15000

gen english=0
replace english=1 if speakeng==3 | speakeng==4

texdoc close