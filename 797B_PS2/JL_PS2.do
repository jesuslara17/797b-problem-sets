* Jesús Lara
* 797B Problem Set 2*
*********************


cd "C:\Users\User\Documents\GitHub\797b-problem-sets\797B_PS2"
eststo clear
use pointonepctsampleE, clear

**** PROBLEM ****
*****   1   *****
*****************

** PART 1A **
*************

//Define the 5 binary outcomes

foreach j of numlist    50 75 100 125 150  {
	cap drop underp`j'
	g underp`j' = poverty<`j'
}


** 1A i) **  // DID regression without covariates


g post = year==2006 // Generates a dummy equal to 1 if year is 2006. Including it= time effects
g lnMW = ln(minwage) 

// areg, regression with many dummy variables (doesn't show coefficients and I would guess it corrects for degrees of freedom for standard errors) In this case the large dummy are the individual effects (50 states), absorb 


// DID regression estimations without covariates

foreach i of varlist underp*{
quiet areg `i' lnMW post, cluster(state) absorb(state)
eststo DD`i'
}


** 1A ii) **

// Choose controls
// Bad controls
// Treatment: lnMW
// 1. "A control is bad when it is causally affected by treatment"
// 2. Colliders: "The control is affected by the treatment"
// Effect of School (treatment randomly assigned) on Height controlling for Income. The treatment (School) causally affects Income, the control. We are likely to find that school reduces height

//All controls:

// division: Not affected by treatment
//age: Not affected by treatment 
// sex: Not affected by treatment 
// married: Not affected by treatment
// racesingd: Not affected by treatment
// citizen: Not affected by treatment
// hieduc: Not affected by treatment | Probably it affects the decision of going to college?
// empstadt: Potentially causally affected by treatment 
// uhrswork: Potentially causally affected by treatment
// incwelfr: potentially causally affected by treatment


// Estimation 

foreach i of varlist underp*{
quiet areg `i' lnMW post age i.sex i.married i.racesingd i.citizen hieduc, cluster(state) absorb(state)
}



** 1A iii) **

// Now including division-specific effects

//Two different ways of specifying interactions between census division and time
xi i.division*post, pre(_d) // generate a dummy for each census division interacted with year (12 combinations in total)
egen d_div_time = group(post division) // The variable d_div_time assigns the same value to a division interacted 

areg underp50  lnMW  post i._ddiv* , cluster(state) absorb(state)

areg underp50  lnMW  post i.d_div_time , cluster(state) absorb(state) 
//They give the same result

** 1A iv) **



//Make the table here



** PART 1B **
*************

** 1B i) **

** 1B ii) **


** 1B iii) **




**** PROBLEM ****
*****   2   *****
*****************

** PART 2A **
*************

** 2A i) **

** 2A ii) **


** 2A iii) **


** 2A iv) **



** PART 2B **
*************

** 2B i) **

** 2B ii) **


** 2B iii) **

** 2B iv) **

** 2B v) **


** 2B vi) **