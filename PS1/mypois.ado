global home "C:\Users\User\Documents\GitHub\797b-problem-sets"
cd "$home" 


cap program drop  mypois
program mypois, eclass
syntax varlist(numeric fv) 
tokenize `varlist'
  local y "`1'"
  macro shift 1
  local X "`*'"

ml model d0 mypois_eval (`y' = `X')
ml maximize
end
 