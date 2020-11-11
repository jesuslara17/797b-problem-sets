/// 			797B Jesús Lara				 ///
///  Critical Replication and extension of   ///
///DDCG by Acemoglu, Naidu, Restrepo & Robinson///

global home "C:/Users/User/Documents/GitHub/797b-problem-sets/DDCG_Replication"
global tables "$home/tables"
global figures "$home/figures"
global auxdata "$home/auxdata"

cd $home

use DDCGdata_final, clear



encode country_name, generate(id)
xtset id year


tabulate dem, summarize(gdppercapita )
tabulate dem, summarize(ginv)
tabulate dem, summarize(tradewb)
tabulate dem, summarize(prienr)
tabulate dem, summarize(secenr)
tabulate dem, summarize(taxratio)
tabulate dem, summarize(mortnew)
tabulate dem, summarize(unrest)
tabulate dem, summarize(marketref)

/// Dem is the measure of democracy


********************
****** PART 1*******
********************

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