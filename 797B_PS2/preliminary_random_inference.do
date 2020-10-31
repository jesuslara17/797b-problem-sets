
//////// THIS

use s1placebos_teen_logwage, clear
rename CA_diff_teen_logwage diff_36 //Now state 36 is CALIFORNIA REMEMBER
forvalues i=1/36{
gen diff2_`i'=(diff_`i')^2
}
gen post=0
replace post=1 if _time>113
collapse (mean) diff*, by(post)

reshape long diff_ diff2_, i(post) j(state)
reshape wide diff_ diff2_, i(state) j(post)
gen r=diff2_1/diff2_0


//// NOW I HAVE MY r and alpha for all states...


