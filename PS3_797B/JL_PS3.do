//// 797B Problem Set 3 /////
////// Jesús Lara    ////////

*Problem 1 

cap program drop simulation
program simulation, eclass
args Q

matrix Sigma=(1, 0.8 \ 0.8, 1)

cap drop eta xi
drawnorm eta xi, n(1000) cov (`Sigma')

forvalues 1/`Q'{



}