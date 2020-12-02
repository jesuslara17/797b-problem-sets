capture program tefpas_ra, eclass

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: tab year if ydep0!=. & tdemoc!=., gen(dumyears)

/* Estimates for year -15 */
cap: teffects ra (ydep0 lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=_b[ATET: r1vs0.tdemoc]
}
else{
matrix b=(.)
}
quietly: drop dumyears*

/* Estimates for years -14 to -5 */
forvalues s=1(1)10{

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: tab year if ydep`s'!=. & tdemoc!=., gen(dumyears)

/* Estimates for year s */
cap: teffects ra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears*
}

/* Estimates for years -4, -3, -2, -1 are zero by construction */
matrix b=(b, 0)
matrix b=(b, 0)
matrix b=(b, 0)
matrix b=(b, 0)

/* Estimates for years 0 to 30 */
forvalues s=15(1)45{

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: tab year if ydep`s'!=. & tdemoc!=., gen(dumyears)

/* Estimates for year s */
cap: teffects ra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears*
}

/* Post results in matrix b, variance computed by bootstrapping later */
ereturn post b
end

/*******************************************************************************************************************************/
/*******************************************************************************************************************************/
/************ DEFINE PROGRAM THAT COMPUTES THE INVERSE PROPENSITY SCORE REWEIGHTING ESTIMATOR FOR ALL YEARS ********************/
/*******************************************************************************************************************************/
/*******************************************************************************************************************************/
capture program tefpas_ipw, eclass

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: gen temp=tdemoc if ydep0!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)

/* Estimates for year -15 */
cap: teffects ipw (ydep0) (tdemoc lag1y lag2y lag3y lag4y dumyears*, noconstant probit), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=_b[ATET: r1vs0.tdemoc]
}
else{
matrix b=(.)
}

quietly: drop dumyears* temp mtemp

/* Estimates for year -14 to -2*/
forvalues s=1(1)13{

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)

cap: teffects ipw (ydep`s') (tdemoc lag1y lag2y lag3y lag4y dumyears*, noconstant probit), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}

/* Estimates for year -1 is zero by construction */
matrix b=(b, 0)

/* Estimates for year 0 to 30 */
forvalues s=15(1)45{

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)

cap: teffects ipw (ydep`s') (tdemoc lag1y lag2y lag3y lag4y dumyears*, noconstant probit), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}

/* Post results in matrix b, variance computed by bootstrapping later */
ereturn post b
end

/*******************************************************************************************************************************/
/*******************************************************************************************************************************/
/************************ DEFINE PROGRAM THAT COMPUTES THE DOUBLY ROBUST ESTIMATOR FOR ALL YEARS *******************************/
/*******************************************************************************************************************************/
/*******************************************************************************************************************************/
capture program tefpas_ipwra, eclass

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: gen temp=tdemoc if ydep0!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)

cap: teffects ipwra (ydep0 lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc lag1y lag2y lag3y lag4y dumyears*, probit), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=_b[ATET: r1vs0.tdemoc]
}
else{
matrix b=(.)
}
quietly: drop dumyears* temp mtemp

/* Estimates for years -14 to -5 */
forvalues s=1(1)10{

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)

cap: teffects ipwra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc lag1y lag2y lag3y lag4y dumyears*, probit), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}

/* Estimates for years -4, -3, -2, -1 are zero by construction */
matrix b=(b, 0)
matrix b=(b, 0)
matrix b=(b, 0)
matrix b=(b, 0)

/* Estimates for year 0 to 30 */
forvalues s=15(1)45{

/* Create year dummies and avoids using ones with no observation (program teffects gets stuck with variables it should drop) */
quietly: gen temp=tdemoc if ydep`s'!=. &tdemoc!=.
quietly: bysort year: egen mtemp=max(temp)
quietly: tab year if mtemp==1, gen(dumyears)

cap: teffects ipwra (ydep`s' lag1y lag2y lag3y lag4y dumyears*, noconstant) (tdemoc lag1y lag2y lag3y lag4y dumyears*, probit), atet iterate(7)

/* Save the results */
if _rc==0{
matrix b=(b, _b[ATET: r1vs0.tdemoc])
}
else{
matrix b=(b, .)
}
quietly: drop dumyears* temp mtemp
}

/* Post results in matrix b, variance computed by bootstrapping later */
ereturn post b
end