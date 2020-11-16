/// 			797B Jesús Lara				 ///
///  Critical Replication and extension of   ///
///DDCG by Acemoglu, Naidu, Restrepo & Robinson///

global home "C:/Users/User/Documents/GitHub/797b-problem-sets/DDCG_Replication"
global tables "$home/tables"
global figures "$home/figures"
global auxdata "$home/auxdata"
global numbers "$home/numbers"

cd $home

use DDCGdata_final, clear


xtset wbcode2 year


tabulate dem, summarize(gdppercapita) o
tabulate dem, summarize(ginv) o
tabulate dem, summarize(tradewb) o
tabulate dem, summarize(prienr) o
tabulate dem, summarize(secenr) o
tabulate dem, summarize(taxratio) o
tabulate dem, summarize(mortnew) o
tabulate dem, summarize(unrest) o
tabulate dem, summarize(marketref) o 

/// Dem is the measure of democracy


********************
****** PART 1*******
********************
quiet {
xtreg y dem l.y i.yy*, fe r cluster(wbcode2) 
eststo T2_1
nlcom (longrun: _b[dem]/(1-_b[l1.y])) (persistence: _b[l1.y]) , post
eststo T2_1a

xtreg y dem l(1/2).y i.yy*, fe r cluster(wbcode2)
eststo T2_2

nlcom (longrun: _b[dem]/(1-_b[l1.y]-_b[l2.y])) (persistence: _b[l1.y]+_b[l2.y]) , post
eststo T2_2a

xtreg y dem l(1/4)y i.yy*, fe r cluster(wbcode2)
eststo T2_3
nlcom (longrun: _b[dem]/(1-_b[l1.y]-_b[l2.y]-_b[l3.y]-_b[l4.y])) (persistence: _b[l1.y]+_b[l2.y]+_b[l3.y]+_b[l4.y]) , post
eststo T2_3a
}

esttab T2_1 T2_2 T2_3 using "$tables/Table2.tex", keep(dem  L.y  L2.y  L3.y  L4.y) varlabels(dem "Democracy" L.y "Log GDP, first lag" L2.y "Log GDP, second lag" L3.y "Log GDP, third lag" L4.y "Log GDP, fourth lag") fragment nonum noobs nonotes mlabels("(1)" "(2)" "(3)") b(3) se ty replace nostar  wrap gaps

esttab T2_1a T2_2a T2_3a using "$tables/Table2.tex", varlabels (longrun "Long-run effect of democracy" persistence "Persistence") fragment nonum nonotes  b(3) se ty append nostar nomtitles nolines wrap gaps

 keep(lnMW) varlabels(lnMW "")

********************
****** PART 2*******
********************

********************
****** PART 3*******
********************

********************
****** PART 4*******
********************

****** 4 (a)********
********************

****** 4 (b)********
********************

****** 4 (c)********
********************


********************
****** PART 5*******
********************

********************
****** PART 6*******
********************