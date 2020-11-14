********************************************************************************
* 							Problem Set Three Review						   *
*								November 12, 2020							   *
*  								   Carly McCann								   *
********************************************************************************

	
****************************
* Creating Simulated data: *
****************************

sample size= 1,000 (doesn't change)

generate z vars (multiple instruments) with std normal distribution 
	(number of instruments changes based on value of Q: 1, 10 ,20)
	- can use drawnorm command (used in PS1)

generate the errors from the specified multivariate normal distribution
		err_mean=(0\0) 
		err_corr=(1,.8\.8,1)

generate x and y as described in problem setup


***************************
* Running the Simmulation *
***************************

Running the simulation 1000 times for each Q(=1,10,20)

- OLS
- 2SLS (conventional confidence interval)
- 2SLS (CLR CI using weakiv)
- LIML: limited-information maximum likelihood estimator
- (post) LASSO for instrument selection in first stage and reduced form /// Lasso may not pick any instrument: report when that happens. 

NEED TO FIND/STORE:
- the bias (B^ - 1) -> report median 
- type 1 error (prob of incorrectly rejecting the null B=1)
- share of simulations that don't pick up any instrument for LASSO
and report bias/Type I error for those that do pick up some instruments

-> Create one table which shows the median bias and type 1 error for the 3 
Q values and 4(or 5 if you also do LASSO) specificaitons 
 
 
foreach Q in 1 10 20  { //Q values

	forvalues i=1/1000 { //reps
		di "iteration `i' for Q=`Q'" //will display the interation and Q value 
		
	*Create dataset*

	*OLS*
		reg y x, robust 
		scalar bias_ols = _b[x]-1
		
	*2SLS (conventional)*
		ivreg2 
	
	*2SLS (CLR using weakiv)*
		point estimate will be same as 2SLS above (so bias will be same) 
		but use weakiv command to get p vlaue to find type 1 error rate

	*LIML*
		see ,liml option in ivreg2 command

	*LASSO* 
		lassoregress command to pick the instruments
		then use ivreg2 with those chosen instruments

		- sometimes LASSO won't pick any instruments - keep track of how many times that
		happens. Report bias/type 1 for cases that do pick instruments.

}
	
