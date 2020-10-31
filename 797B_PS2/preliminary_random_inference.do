
//////// THIS

use s1placebos_teen_logwage, clear
rename CA_diff_teen_logwage diff_36 //Now state 36 is CALIFORNIA REMEMBER
forvalues i=1/36{
gen diff2_`i'=(diff_`i')^2
}
gen post=0
replace post=1 if _time>114
collapse (mean) diff*, by(post)


reshape long diff_ diff2_, i(post) j(statenum2)
reshape wide diff_ diff2_, i(statenum2) j(post)

forvalues i=0/1{
gen sqrtdiff2_`i'= sqrt(diff2_`i')

}
gen r=sqrtdiff2_1/sqrtdiff2_0
merge 1:1 statenum2 using statelabels, keepusing(stateabb) nogen
replace stateabb="CA" if statenum2==36
label values statenum2 stateabb

sort r
graph hbar r, over (stateabb) scheme(s1color) bar)
//// NOW I HAVE MY r and alpha for all states...

use statelabels,clear 
