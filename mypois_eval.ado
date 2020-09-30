global home "C:\Users\User\Documents\GitHub\797b-problem-sets"
cd "$home" 



cap program drop mypois_eval
program mypois_eval , eclass
args todo b lnf

tempvar theta

mleval `theta' = `b'

local t "$ML_y1"


mlsum `lnf' = (-1)*exp(`theta') + `t'*`theta' + (-1)*lnfactorial(`t')

end 
