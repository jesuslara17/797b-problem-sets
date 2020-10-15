*prueba 

*DO-FILE PS 1

* Jesús Lara Jáuregui

		

clear
global home "C:\Users\User\Documents\GitHub\797b-problem-sets"
cd "$home" 
set more off

texdoc init prueba, replace


/*tex
\documentclass[12pt]{article}
\usepackage{stata}
\usepackage[utf8]{inputenc}
\usepackage{default}
\usepackage{pgfplots}
\pgfplotsset{/pgf/number format/use comma,compat=newest}
\usepackage{xcolor}
\usepackage{amsmath,amsfonts,amssymb}
\usepackage{hyperref}
\usepackage{tikz}
\usepackage{rotating}
\usepackage{geometry}

\begin{document}

\author{Jesús Lara Jáuregui}
\title{Problem Set 1}
\maketitle



tex*/


/*tex 
\section{Problem 1. OLS in MATA}
\subsection{Part 1}



tex*/



		*********************
		***** PROBLEM 1 *****
		*********************
		
/*Part 1*****. Write an e-class program “myreg1” which takes in a varlist and performs an OLS regression of y on X
on as follows*/

/*tex 
\section{Problem 1. OLS in MATA}
\subsection{Part 1}



tex*/
use census_sample_30_50, clear 

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

texdoc stlog
myreg1 lnwage hieduc exp exp2 
quiet reg lnwage hieduc exp exp2
matrix list e(b)
matrix list e(V)
texdoc stlog close







/* Part 2 Write an e-class program called “myreg2” which takes in a varlist and performs OLS regression of y on
X as follows: */

/*tex
/subsection{Part 2}
*/

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

texdoc stlog

myreg2 lnwage hieduc exp exp2 
quiet reg lnwage hieduc exp exp2, robust
matrix list e(b)
matrix list e(V)

texdoc stlog close

/*tex

\end{document}

tex*/

texdoc close