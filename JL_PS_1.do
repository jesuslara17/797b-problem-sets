*DO-FILE PS 1

* Jesús Lara Jáuregui

*log using "C:\Users\User\Documents\GitHub\797b-problem-sets\JLPS1.log", replace		
*texdoc do "C:\Users\User\Documents\GitHub\797b-problem-sets\JL_PS_1.do", replace

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
\section{Problem One. OLS in MATA}
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

/*tex 
In this problem I created the program e-class program myreg1. This program takes as input a dependent variable $y$ and a set of independent variables or regressors $X_k$. The program transform these variables into a vector and matrix respectively and performs the operations necessary to get our OLS estimates and the associated variance-covariance matrix. the output is thus a $b_{(k+1)x1 vector}$ (OLS estimates)  and a $V_(k+1)x(k+1)$ matrix (Variance-covariance).

The log below shows that the results of myreg1 are the same as those obtained using Stata's embed
tex*/


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

tex \input{P5_T2.tex}






*****PART 2

/*tex
\subsection{Part 2}
tex*/

tex \pagebreak

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

/*tex 
In this part I created the program myreg2 which takes the same inputs as myreg1 and gives the same vector of OLS estimates $b$. myreg2 takes the variables from Stata and then implements a second program called myols(X,Y), which is the one that actually calculates the OLS estimates and the variance-covariance Matrix $V$ adjusted for arbitrary heteroscedasticity. With respect to the OLS estimates, instead of calculating them with the cross product (and inverse) of the whole $X$ matrix and $y$ vector, it performs the sum of the cross product of each row (observations). The same approach is used for calculating the matrix $V$.

The log below shows that my results are exactly the same as those obtained using Stata's regress command and "robust" option. 

tex*/

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

tex \pagebreak


/*tex

\section{Problem 2. Poisson using Maximum Likelihood}

If $y_i$ is distributed Poission with mean $exp(X^{'}_i \beta)$, hence the likelihood function for a sample of $N$ observations is given by:

$$ L(\beta)=\prod_{i=1}^{N}\frac{1}{y_{i}!}exp((X^{'}\beta)y_i)exp(-exp(X^{'}\beta))$$


And taking logs we get:

$$lnL(\beta)=\sum_{i}^{N}[-exp(X^{'}\beta)+y_i exp(X^{'}\beta)-ln(y_i !)]$$

Which is the form we use for pur maximum-likelihood estimation
tex*/


*1Program an .ado file called “mypois.ado” that estimates poisson regression using maximum likelihood


 
/*tex
I made two .ado files, one containing the generation of the evaluator program and the other one that takes a dependent and independent variables from Stata and performs the Maximum Likelihood Estimation. Those .aso files are attached in the folder. 

I show the histogram of the number of awards as well as the mean and variance of the variable. The key assumption of Poisson distribution is that the parameter $\lambda$ is the mean and variance of y. However, we see that the variance is almost twice bigger than the mean, which may reduce the usefulness of Poisson distribution to analyze the behavior of the number of awards. 

tex*/
 
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

/*tex 
In the table below I show the results of the estimation using Stata's command and mypois. I get the same results.
tex*/


tex \input{P2_MV.tex}
*5. Estimate coefficients using built in command: poisson num_awards i.prog math Confirm 4 and 5 give the same results

mypois num_awards i.prog math
estimates store mypois

*3 

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

tex \pagebreak

/*tex 
\section{Problem 3. Mean Squared Error simulation - Sample Size and Distribution}

tex*/

/*tex

In the first part of the program I create the program OLSPOIS, whose inputs are a number of observations N and an scalar $\sigma$ that generates a matrix-covariance matrix. My program generates a variable $y$ distributed poisson with mean $exp(2X_{1i}-X_{2i})$. Then it estimates a Poisson and an OLS regression ($lny$ as dependent variable) and returns the average of the squared errors. 

tex*/
* 3 Part 1
clear 
eststo clear



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
quiet{
local j=1


foreach N of numlist 50 1000{
foreach sigma of numlist 0.01 0.1 1{

forvalues i=1/1000{
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

/*tex 

I made a loop to run the program 1000 times with N=50,1000 and $sigma=0.01,0.1,1$ I show the average of the squared error (MSE) obtained in the six cases in the in the table below. The most salient fact is that MSE is always substantially smaller when using Poisson than with OLS. Additionally, MSE is smaller in both cases when the number of observations is large $(N=1000)$. Also, the smaller sigma (covariance and variance of $X_1$ and $X_2$), the smaller the MSE. 


tex*/

tex \input{MS.tex}


tex \pagebreak

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
drop `unit'_id `y'2 random treatment

end 




*4.1
use 25_MSA_panel, clear
keep if naics==10
xtset cbsa time 



*** (b) Run the loop

* remember to put 1000 sims
set seed 5555 
quiet{
forvalues i=1/1000{
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

/*tex 
I generate the program randsim that takes as inputs a dependent variable $y$, a individual or cluster variable "unit" and a time variable "t". It randomly assigns treatment=1 to a unit with probability 0.25 and then generates a variable $y2=y+0.05*treatment$ If it happens that no unit is treated then it assigns the value one to a scalar called no_treated, zero otherwise. It then runs a regression with unit and time fixed effects using clustered standard errors at the unit level. a scalar sig_y takes the value of one if the coefficient of treatment is significant at the 5% level and zero otherwise. The program also calculates the estandard error following the wild bootstrap approach using the boottest command. If the coefficient of treatment is significant at the 5% level, the scalar bsig_y takes value 1, and zero otherwise. My program finally returns the 2 scalars.

I run the program 1000 times in two cases: with all (25) and with just a few number of clusters (8). The frecuencies of the 4 scalars are reported below. These frequencies allow us to see what happens with the reccurence of type 1 and 2 errors with the different standard errors techniques of estimation. 

I base my analysis in the following interpretation. Type 1 error means rejecting a null hypothesis that is actually true, whereas Type 2 means failing tu reject a false null hypothesis. Our null hypothesis is $H_0:\beta_{treatment}=0$. For $lnemp$ treatment is a placebo, so $H_0$ is actually true. Rejecting it means making the type 1 error. In contrast, for $lnemp2$, $H_0$ is false: there is a direct relationship between $lnemp2$ and treatment. So failing to reject $H_0$ would be the type 2 error. 

The tables below show the frecuencies of ones and zeros of our four scalars. From first row of table 4 we can see the Bootstrap is much better at avoiding type 1 errors than cluster. However, from the second row of table 5 we see that type 2 error is very frequent with bootstrap. 

The results for a small number of clusters are shown in tables 6 and 7. Whereas there are no major differences for type 1 error (first row of Table 6), in the second row of table 7 we can see that bootstrap is failing to find significance of treatment with lnemp2. That is, type 2 error becomes more frequent with a small number of clusters

tex*/


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
forvalues i=1/1000{
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
mata: F_1s=(s_1r,s_1b\s_2r,s_2b)

mata: F_0s=J(2,2,rows(S))-(F_1s+J(2,2,s_nt))

mata:st_matrix("F_1s",F_1s)
mata:st_matrix("F_0s",F_0s)

mat rownames F_1s="lnemp" "lnemp2"
mat colnames F_1s="Cluster" "Bootstrap"

mat rownames F_0s="lnemp" "lnemp2"
mat colnames F_0s="Cluster" "Bootstrap"

matrix list F_1
matrix list F_0

matrix list F_1s
matrix list F_0s


esttab m(F_1s) using F_1_R.tex, replace title(Coefficient of treatment significant? Frecuency (few clusters)) nomtitles booktabs

esttab m(F_0s) using F_0_R.tex, replace title(Coefficient of treatment insignificant? Frecuency (few clusters)) nomtitles booktabs

tex \input{F_1_R.tex}
tex \input{F_0_R.tex}

*/




**********************
******* PROBLEM 5*****
**********************

tex \bigskip

clear

use census_sample_30_50, clear

/*tex
\pagebreak
\section{Problem 5: Matching}

tex*/  


* Foreign born
gen FB=0
replace FB=1 if bpld>15000

* English proefficiency:

gen english=0
replace english=1 if speakeng==3 | speakeng==4

* 1. Bivariate OLS Regression
	regress lnwage FB, robust
eststo Bivariate
 		estadd scalar FB = _b[FB]
		estadd scalar se_FB = _se[FB]
		estadd local Estimator "OLS Bivariate"
 
* 2.OLS regression with simple controls

	regress lnwage FB exp exp2  i.married i.racesing i.hisp i.educ99 i.english i.gender, robust
eststo Simple
		estadd scalar FB = _b[FB]
		estadd scalar se_FB = _se[FB]
		estadd local Estimator "OLS Controls"
	

*3. OLS regression with fully saturated controls

	/*cap drop demogroup
	egen demogroup = group(exp exp2 married racesing hisp educ99 english gender)
	sum demogroup 

	areg lnwage FB ,   robust a(demogroup)
eststo Saturated
		estadd scalar FB = _b[FB]
		estadd scalar se_FB = _se[FB]
		estadd local Estimator "OLS Saturated"*/
	*or*	
			cap drop demogroup
	egen demogroup = group(exp exp2 married racesing hisp educ99 english gender)
	sum demogroup 

	reghdfe lnwage FB ,    a(demogroup)
eststo Saturated
		estadd scalar FB1 = _b[FB]
		estadd scalar se_FB1 = _se[FB]
		estadd local Estimator "OLS Saturated"
 
*4. Coarsened exact matching using CEM

	 cem exp exp2 married racesing hisp educ99 english sex, tr(FB)
	 reg lnwage FB [iweight=cem_weights]
eststo CEM
		 estadd scalar FB = _b[FB]
		 estadd scalar se_FB = _se[FB]
		 estadd local Estimator "CEM"
	
	esttab using P5_T1.tex, replace not nostar se keep( ) ///
	label nodepvar nonumber title(Estimates of FB) ///
	stats(FB se_FB N Estimator, fmt(3 3 0) layout(@ "(@)" @) ///
	label("FB" "    " "Obs."))  ///
	
 
tex \newgeometry{left=1.5cm,bottom=0.1cm}
tex \input{P5_T1.tex}
tex \restoregeometry

	

*5. Prosensity Score Matching - using simple covariates and 10 strata

* First stage - prediction using Probit * 
 
	cap drop fbprop
	probit FB exp exp2 married i.racesing hisp i.educ99 english sex    
	predict fbprop 
 
* Create 10 bins of fb propensity * 
cap drop fbprop_n
	xtile fbprop_n = fbprop, nq(10)


/*tex
The table shows that the covariates are reasonably balanced.	
tex*/

* Check covariate balance *
*I could not include all the covariates, it said too many stats. 
texdoc stlog
table fbprop_n FB, c(mean exp mean married mean racesing mean hisp mean educ99) row
texdoc stlog close
  

* Create within bin log wage for fb and not fb *
foreach j of numlist 0 1 {
	cap drop tr`j'
	cap drop lnwage_tr`j'
	gen tr`j' = 1 if FB==`j'
	egen lnwage_tr`j' = mean (lnwage*tr`j'), by(fbprop_n)	
}

  
* Create within bin log wage differential *
cap drop lnwdif
gen lnwdif = (lnwage_tr1-lnwage_tr0) if FB==1
sum lnwdif
 

* Create weights for bin based on how many of fb in that bin *

cap drop fbtot 
cap drop alltot
cap drop fbr
cap drop all

egen fbr = sum(FB), by(fbprop_n)
egen fbtot = sum(FB)

cap drop wt1  
gen wt1 = fbr/fbtot
 

cap drop first 
egen first = tag(fbprop_n) if FB==1 

* Show log wage differential within bins, and weighted average thereof *
 
table fbprop_n if FB==1 & first==1 [aw=wt1] , c(mean lnwdif) row  

eststo PScore
		estadd scalar FB = -0.077
		estadd scalar se_FB = 0.116
		estadd local Estimator "Match"


*6. Propensity Score Reweighting using psmatch2 simple covariates and 10 nearest neighbors

psmatch2 FB exp exp2 i.married i.racesing i.hisp i.educ99 i.english i.gender , out (lnwage) neighbor(10)  
*display r(att)
eststo Psmatch_1
		estadd scalar FB = r(att)
		estadd scalar se_FB = 0.032
		estadd local Estimator "Match"

 
*7. Propensity Score Reweighting using own calculations
 
cap drop fbprop
probit FB married exp exp2 hisp english gender educ99 racesing     
predict fbprop


summ FB
local p=r(mean)
cap drop w3
gen w3 = cond(FB, `p'/(1-`p'), fbprop/(1-fbprop))

texdoc stlog, nolog 
twoway (kdensity fbprop if FB==1)(kdensity fbprop if FB==0) ///
(kdensity fbprop if FB==0 [aw=w3]), ///
legend(on label(1 "Treated") label(2 "Control (unweighted)") ///
label(3 "Control (weighted)")) title("Comparison of distributions")
texdoc stlog close 
texdoc graph

reg lnwage FB [aw=w3]
eststo Own
		estadd scalar FB = _b[FB]
		estadd scalar se_FB = _se[FB]
		estadd local Estimator "Match"

 
*8. Propensity Score Reweighting Using teffects ipw
 
 teffects ipw (lnwage) (FB exp exp2 married racesing hisp educ99 english gender,probit), atet vce(robust)
eststo IPW
		estadd scalar FB = _b[r1vs0.FB]
		estadd scalar se_FB = _se[r1vs0.FB]
		estadd local Estimator "Match"

/*tex
I compare matching based on different 
methods. I report each method's estimates in the table below. 
tex*/		


esttab using P5_T2.tex, replace not nostar se keep( ) ///
	label nodepvar nonumber title(Estimates of FB) ///
	stats(FB se_FB N Estimator, fmt(3 3 0) layout(@ "(@)" @) ///
	label("FB" "    " "Obs."))  ///
	addnotes(Average Treatment On Treated for matching models)


 
tex \newgeometry{left=1.5cm,bottom=0.1cm}
tex \input{P5_T2.tex}
tex \restoregeometry



/*tex

\end{document}

tex*/
texdoc close