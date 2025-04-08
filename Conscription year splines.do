****Sensitivity analysis modelling conscription year by splines

use "...dta"

drop std_rpg1 std_rpg2 std_rpg3 std_rpg4

label define outcome_diabetes 1 "diabetes" 2 "emigration" 3 "death" 4 "end of follow-up"
label value outcome_diabetes outcome_diabetes

replace FU_diabetes = FU_diabetes/365.24
gen v2_FU_diabetes = FU_diabetes + age_conscription

replace FU_diabetes2 = FU_diabetes2/365.24
gen v2_FU_diabetes2 = FU_diabetes2 + age_conscription

keep if monstar >=1972 & monstar <=1995

drop if FU_diabetes==0
keep if kon == "1"

gen wmax_missing = 1 if wmax ==.
replace wmax_missing = 0 if wmax_missing ==.
keep if wmax_missing == 0

gen education_mor = 1 if sun2020niva_old_mor == 1 | sun2020niva_old_mor ==2
replace education_mor = 2 if sun2020niva_old_mor == 3 | sun2020niva_old_mor == 4
replace education_mor = 3 if sun2020niva_old_mor == 5 
replace education_mor = 4 if sun2020niva_old_mor == 6 | sun2020niva_old_mor == 7

gen education_far = 1 if sun2020niva_old_far == 1 | sun2020niva_old_far ==2
replace education_far = 2 if sun2020niva_old_far == 3 | sun2020niva_old_far == 4
replace education_far = 3 if sun2020niva_old_far == 5 
replace education_far = 4 if sun2020niva_old_far == 6 | sun2020niva_old_far == 7

egen maxedu = rowmax(education_mor education_far)

egen maxincome = rowmax(quintile_income_mor quintile_income_far)

gen rounded_monstar=round(monstar, 5)
gen rounded_birthyear=round(birthyear, 5)

gen bmi_squared = bmi^2

gen bmi_missing = 1 if bmi ==.
replace bmi_missing = 0 if bmi_missing ==.
drop if bmi_missing==1
drop if maxedu ==.
drop if maxincome ==.

drop if wmax <100
drop if bmi <=15 | bmi>=60

xtile wmax_decile = wmax, nq(10)

tab wmax_decile, gen(dummywmax)
rename dummywmax1 wmax1
rename dummywmax2 wmax2
rename dummywmax3 wmax3
rename dummywmax4 wmax4
rename dummywmax5 wmax5
rename dummywmax6 wmax6
rename dummywmax7 wmax7
rename dummywmax8 wmax8
rename dummywmax9 wmax9
rename dummywmax10 wmax10

foreach covar in maxedu maxinco {
tab `covar', gen(`covar'_dum)
drop `covar'_dum1
}

ds *dum*
global covar = "`r(varlist)'"

mkspline monstarsplines = monstar, nknots(4) cubic displayknots

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stcox wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 monstarsplines* age_conscription bmi bmi_squared $covar

gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.

foreach var in bmi bmi_squared age_conscription {
egen `var'_bw = mean(`var'), by(famid)
}

egen wmax2_bw = mean(wmax2), by(famid)
egen wmax3_bw = mean(wmax3), by(famid)
egen wmax4_bw = mean(wmax4), by(famid)
egen wmax5_bw = mean(wmax5), by(famid)
egen wmax6_bw = mean(wmax6), by(famid)
egen wmax7_bw = mean(wmax7), by(famid)
egen wmax8_bw = mean(wmax8), by(famid)
egen wmax9_bw = mean(wmax9), by(famid)
egen wmax10_bw = mean(wmax10), by(famid)

egen monstarspline_bw1 = mean(monstarsplines1), by(famid)
egen monstarspline_bw2 = mean(monstarsplines2), by(famid)
egen monstarspline_bw3 = mean(monstarsplines3), by(famid)

stcox wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw monstarsplines* monstarspline_bw* bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw
