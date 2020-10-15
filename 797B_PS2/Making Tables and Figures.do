********************************************************************************
* 				      			E797B Programming Module           		       *                         
* 									Carly McCann                               *
*         This file shows examples of how to create tables/figures             *
*                                 to use with latex                            *
*                               September 2, 2020                              *
********************************************************************************
clear
eststo clear
set more off 


* Set globals 
global home "/Users/ACT/Desktop/E797B_TA/stata_examples/Making_Tables/" //Change this for your computer

	global data "${home}data"
	global do "${home}dofiles"
	global figures "${home}figures" 
	global tables "${home}tables"
	cd "${home}"


use "$data/pointonepctsampleE.dta", clear 


/*/ This file uses packages from the estout package. If you haven't already, download the estout pacakge
Esttab is a user-friendly wrapper for the estout command (I typically use esttab though there 
are other options for making tables such as tabout, outtable, etc.)

	ssc install estout,replace 
	
	
*/	
							**************************
							*** Descriptive Tables ***
							**************************	

							
//1. This example shows how to make a basic descriptive table using estpost with esttab 

/*
  estpost posts results from various Stata commands (sum, tab, corr) in e() 
  so that they can be tabulated using esttab or estout.
*/

*1. Simple Table, no formatting

estpost sum age wage inctot,d
	eststo descriptive1 
	
	
esttab descriptive1 using "$tables/descriptive.tex" ,replace ///
	 cell((mean (fmt(1)) sd (fmt(1)) p50 (fmt(1)))) ///
title(Sample Descriptive Statistics Table\label{descriptive})

*Simple Table, formatted
	
esttab descriptive1 using "$tables/descriptive1.tex", ///
     label noobs nonumbers nomtitles booktabs replace        ///
     cell((                                        ///
            count(fmt(%9.0fc)  label(Obs.))        ///
             mean(fmt(%10.2fc) label(Mean))        ///
              p50(fmt(%8.1fc)  label(Median))      ///
               sd(fmt(%10.2fc) label(Std. Dev.))   ///
          )) ///
title(Sample Descriptive Statistics Table\label{descriptive1}) 


/**Key formatting options:

	1. label: use vairable labels rather than variable names
	2. noobs:suppresses displaying information on the number of observations. 
		The default is to report the number of observations for each model in the table footer.
	3. nonumber: supress printing the model number
	4. nomtitles: suppresses printing of model titles
	5. booktabs: specifies formatting of table (NOTE: must load \usepackage{booktabs} in latex preamble
	6. cell: specify the cell content and formatting. (NOTE: double parentheses is necessary) 
	7.\label{ } add label to table to be referenced in Latex
*/


*2. Table with models (subsamples)

estpost sum age wage inctot if sex==1,d
	eststo male

estpost sum age wage inctot if sex==2,d
	eststo female

		
esttab male female using "$tables/descriptive_by_sex.tex", ///
label nonumbers booktabs gaps  replace ///
	cells( (mean(fmt(2 2 %18.0fc))  )   sd(par fmt(2)))  /// 
stats(N, fmt(%18.0fc) labels("Observations")) ///		  
mtitle("Male" "Female") ///
title(Mean and Std. Dev By Sex\label{sex}) 	
	
	
/**Key formatting options:
  * the 3 formatting options within the mean specifcy the formatting for each variable mean
  * the par option intside the sd places the sd in parentheses in same column as mean
  *mtitle specifies column names
  * the stats line allows me to specify format (i.e add commas) for observations
  * gaps provides space in between the rows are variables
  *NOTE: use of parenthese in cell is what allowed sd to be place below mean. If I wanted to 
  add additional statistics in row they would be placed inside parenthsis with mean.
		*suppose we wanted to bold "male" and "female" we could add in the latex code 
		to code (\textbf{}) directly to mtitle "\textbf{Male}" "\textbf{Female}"
		

*/
	

*3. Table using Groups and Models (by sex and year)
quietly estpost sum  age wage inctot if sex==1 & year==1990 ,d
		eststo male1
		
quietly estpost  sum age wage inctot if sex==1 & year==2006 ,d
		eststo male2		


quietly estpost sum age wage inctot if sex==2 & year==1990,d
	eststo female1
	
quietly estpost sum age wage inctot if sex==2 & year==2006,d
	eststo female2
	

		
esttab male1 female1 male2 female2 using "$tables/descriptive_by_sex_and_year.tex", ///
label nonumbers booktabs gaps replace ///
	cells( (mean(fmt(2 2 %18.0fc) )) sd(fmt(2) par)) collabels(none) /// 
stats(N, fmt(%18.0fc) labels("Observations")) ///	
mgroups(“1990” “2006”, pattern(1 0 1 0)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
mtitle("Male" "Female" "Male" "Female") ///
title(Mean and Std. Dev By Sex and Year\label{sexyear}) ///
note(footnote alerting you to something about the data) 		
	
/**Key formatting options:

	* collabels(none) removes the mean/std column label name
	* mgroups model, pattern specifies how models are to be grouped.
		pattern is list of ones and zeros where ones indicate the start of a new group
	* span causes labels to extend label across several columns	
		eprepeat specifes a string that is repeated for each group. Here it is telling
		the group name to center over the 2 columns and add the underline
	* note: add footnote to table

*/	
	
	
*3. table with panels


foreach v of varlist age married black hisp employ unemploy uhrswork wage self private nonprofit federal state local unpaid   {
	label variable `v' `"\hspace{0.1cm} `: variable label `v''"'
	}
// this adds a small space in front of the label so that in the table they look indented under each category 

estpost sum age married black hisp employ unemploy uhrswork wage if sex==1
eststo A

estpost sum age married black hisp employ unemploy uhrswork wage if sex==2
eststo B


	esttab A B  using "$tables/table2.tex", replace ///
		refcat(age "\emph{Demographics}" employ "\emph{Employment}", nolabel) ///
		mtitle("Male" "Female") ///
		cells(mean(fmt(2))) label booktabs nonum  collabels(none) gaps noobs  fragment ///
		
		
estpost sum  self private nonprofit federal state local unpaid if sex==1		
	eststo C
	estadd matrix pc=e(mean)*100 //transform to percent
		
		
estpost sum self private nonprofit federal state local unpaid if sex==2		
	eststo D
	estadd matrix pc=e(mean)*100 //transform to percent
		

		
esttab C D  using "$tables/table2.tex", append  nomtitles  ///
		cells(pc(fmt(2))) posthead("\addlinespace \emph{Type of Work} (Percent) \\ \addlinespace")  label booktabs nonum collabels(none) gaps noobs  fragment plain 
	
		/**Key formatting options:
		
		*fragment: supresses the table opening and closing in Latex. Allows "panels" to
		be appended and look like one table (rather than 2 tables stacked)
		
		*refact: option to add a table row containing the (omitted) reference 
		category of a categorical variable. NOTE: only works for coeff, so
		in second panel had to do work around with posthead option which
		allows you to add text after the table heading
		
		
		*/
		
			
/*	NOTE: can also use tab for categorical variables, but the categories have
different spacing than the other variables. You can play around with this to experiment

estpost tab classwkd if sex==1 & empstatd==10, nototal
eststo C

estpost tab classwkd if sex==1 & empstatd==10, nototal
eststo D


esttab C D  using "$tables/table2.tex", append  nomtitles  ///
		refcat(classwkd "\emph{Type of Work}", nolabel)  ///
		cells(pct (fmt(2))) label booktabs nonum collabels(none) gaps noobs  varlabels(`e(labels)') f plain
*/

						*****************************
						*** Tables from Matrices  ***
						*****************************		
/*		
Note: esttab can also export estimates saved in matrices. While saving estimes in 
matrices is sometimes easier when looping over multiple variables/models, formatting
estimates from matrcies is more limited		
*/		
		
foreach x of numlist 50 75 100 125 150{
gen I_`x' = cond(poverty< `x',1,0)
}
*


* example:
	forval cj = 50(25)150 {
		qui: areg I_`cj' trt_change d_2006 , a(statefips) vce(cluster statefips)
			mat mat_`cj' = _b[trt_change] \  _se[trt_change] 
	}
	
*

mat Basic=[mat_50 \mat_75 \ mat_100 \mat_125 \ mat_150] 


forval cj = 50(25)150 {
	qui: areg I_`cj' trt_change d_2006 ///
		age age2 sex married i.racesingd i.citizen hieduc ///	
		, a(statefips) vce(cluster statefips)
	mat mati_`cj' = _b[trt_change] \ _se[trt_change]
}
*
mat Covariates=[mati_50 \mati_75 \ mati_100 \mati_125 \ mati_150] 


mat Poverty = [Basic , Covariates]
mat colnames Poverty= "Basic" "Covariates" 
mat rownames Poverty="Poverty 50" "se" "Poverty 75" "se " "Poverty 100" "se " "Poverty 125" "se " "Poverty 150" " se"

esttab m(Poverty,fmt(%9.2f)) using "$tables/matrix.tex", replace title(Estimates of Poverty) nomtitles booktabs

		
	
		
		
						**************************
						*** Regression Tables  ***
						**************************		
		
foreach v of varlist age married black hisp employ unemploy uhrswork wage self private nonprofit federal state local unpaid   {
	
	
  local varlabel : var label `v'
  local newname : subinstr local varlabel "\hspace{0.1cm}" "", all
  label variable `v' "`newname'"
		}	//remove that indent I added from eariler table 
	

	
		
		
//. Regression tables

*1. Basic Regression table, no formatting
global indiv_controls "age female married exp exp2 black hisp" // define global
reg lnwage $indiv_controls // to call upon global use $
	eststo reg
	
	
esttab reg  using "$tables/reg1.tex",   ///
title("Sample Regression Table" \label{reg1}) replace 


* Regression Table with models, added scalars, and with formatting

quietly reg lnwage $indiv_controls, robust
	eststo reg1
	estadd local State_Effect "No"
	estadd scalar R e(r2)

 reg lnwage $indiv_controls i.statefips, robust
 
	eststo reg2
	estadd local State_Effect "Yes"
	estadd scalar R e(r2)



esttab reg1 reg2  using "$tables/reg2.tex", not b(%8.2f) se(%8.2f) ///
title("Sample Regression Table with Multiple Models" \label{reg2}) replace  label nonumber booktabs ///
keep ($indiv_controls ) stats(N  R State_Effect, fmt(%9.0gc  2) label("Observations"  "R-squared" "State Fixed Effects" ))  ///
mtitle("Model 1" "Model 2")	
	
	
/**Key formatting options:
	* not: supress t statistic (default is to display t statistic)
	* b(fmt) format display of beta, se(fmt) format display of standard errors
	* keep(): specifes which coefficents to keep [can also use drop()]
	* stats: format and display of stats (added using estadd)
	
Other potential useful formatting changes:
		*order (): allows you specify the order of the cofficients
		* no star: supress significance stars
			*brackets: uputs se in [brackets] rather than (parentheses)
			* you can set your own star thresholds for example using star ( )
				for exmaple, star(* 0.10 ** 0.05 + 0.01) 
				NOTE: that the thresholds must lie in the (0,1] interval 
				and must be specified in descending order.
	
*/
						

						**************************
						***      Figures	   ***
						**************************		

//3. Graphs 

* Basic coefplot, no formatting

coefplot reg1,  keep($indiv_controls) label  


 graph export "$figures/fig1.png", as(png) replace


 *Coefplot with multiple models, more formatting

 
 coefplot (reg1, label(Model 1) ) ///
		  (reg2, label (Model 2) ) , ///	
   keep($indiv_controls) ///
xline(0) mlabel format(%9.2f) label  ///
scheme(s1color) //  stata preset schemes

  graph export "$figures/fig2.png", as(png) replace

 
 /**Key formatting options:
	* options after model names are specific to that model, options after comma
		apply to  graph overall.
	xline(0): adds vertical line at 0
	mlabel: adds marker labels
	scheme(): uses stata graph scheme
	
*NOTE: There are a LOT of formatting options (marker style, colors, etc)
see here: for brief i ntro: http://repec.sowi.unibe.ch/stata/coefplot/getting-started.html

 */
 
 
 
 
*Fake event study:
set seed 13579
foreach x of numlist 1/3{
gen lead`x' = cond(runiform() < 0.5, 0, 1)
}

gen event = cond(runiform() < 0.5, 0, 1)

foreach x of numlist 1/3{
gen lag`x' = cond(runiform() < 0.5, 0, 1)
}


reg lnwage lead1 lead2 lead3 event lag1 lag2 lag3  $indiv_controls, robust
	eststo reg3

reg lnwage lead1 lead2 lead3 event lag1 lag2 lag3  $indiv_controls i.statefips, robust
	eststo reg4


coefplot reg3, vertical  keep(lead1 lead2 lead3 event lag1 lag2 lag3) ///
yline(0) mlabel format(%9.2g) addplot(line @b @at,lpattern(dash)) ///
xtitle("Time passage relative to year of policy adoption") ///
ytitle("Change in Wage",size(medium)) ///
title(Fake Event Study) ///
scheme(s1color) 

 graph export "$figures/event_study.png", as(png) replace

 
  /**Key formatting options:
	*vertical puts lead/lag vars on horizontal axis
	*yline(0)" adds horizontal line at 0
	*addplot(line @b @at, lpattern(dash): adds dashed line connecting the points
 
 */
 
 
 

gen time=.
replace time=-3 if lead3==1
replace time=-2 if lead2==1
replace time=-1 if lead1==1
replace time=0 if event==1
replace time=1 if lag1==1
replace time=2 if lag2==1
replace time=3 if lag3==1


 
 egen mean_wage = mean(wage), by(time)
 egen mean_wage_female= mean(wage) if female==1, by(time) 
 egen mean_wage_male= mean(wage) if male==1, by(time) 
 
 
 // Sometimes its nice to put the graph formats in locls
 
 
	local legendoptions pos(12) ring(0) cols(1) size(small) region(lcolor(white)) order(1 2 3) label(1 "Overall Wage") label(2 "Female Wage") label(3 "Male Wage") 
	local yrange ylabel(10(.25)12.5, glcolor(gs15) labsize(small) gmin gmax)
	local xrange xlabel(-3(1)3, labsize(medium))
	local ystyle size(medsmall) color(black) margin(r=2)) ysize(3) xsize(6.5)
	local xtitle "Years since treatment"
	local xstyle size(medsmall) color(black) margin(t=2)
	local xlinestyle 0,lpattern(l) lcolor(gs11)) yline(0,lpattern(l) lcolor(gs11)
	local ylinestyle 0,lpattern(l) lcolor(gs11)
	local style graphregion(color(white)) plotregion(style(none) margin(l=0 r=2 b=0 t=1))
	
	
	line mean_wage time, sort lcolor(navy navy) lpattern(solid) ///
	|| line mean_wage_female time, sort lcolor(navy maroon) lpattern(dash) ///
	|| line mean_wage_male time, sort lcolor(navy maroon) lpattern(dot) ///
	legend(`legendoptions') `yrange'  `xrange' ///
	`style' xline(`xlinestyle') yline(`ylinestyle') ///
	xtitle(`xtitle', `xstyle') ///
	ytitle(Mean Wage, `ystyle'
	*
	
	
 graph export "$figures/event_study2.png", as(png) replace
	

 


 
