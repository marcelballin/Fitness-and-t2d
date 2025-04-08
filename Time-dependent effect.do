///Time-dependent effect analysis

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

foreach covar in rounded_monstar maxedu maxinco {
tab `covar', gen(`covar'_dum)
drop `covar'_dum1
}

ds *dum*
global covar = "`r(varlist)'"

****Cohort - assuming proportional hazards

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform
est store diabetes_decile

range time 0 65 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==65, noobs
	
drop *std* *contrast* time 

range time 0 60 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==60, noobs
	
drop *std* *contrast* time 

range time 0 55 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==55, noobs
	
drop *std* *contrast* time 

range time 0 50 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==50, noobs
	
drop *std* *contrast* time 

range time 0 45 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==45, noobs
	
drop *std* *contrast* time 

range time 0 40 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==40, noobs
	
drop *std* *contrast* time 

****Cohort - time-dependent effect 

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription) id(LopNr) 

stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) tvc(wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10) dftvc(3) nolog 


range time 0 65 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==65, noobs
	
drop *std *contrast* time 

range time 0 60 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==60, noobs
	
drop *std *contrast* time 

range time 0 55 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==55, noobs
	
drop *std *contrast* time 

range time 0 50 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==50, noobs
	
drop *std *contrast* time 

range time 0 45 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==45, noobs
	
drop *std *contrast* time 

range time 0 40 2

 standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(wmaxstd1 wmaxstd2 wmaxstd3 wmaxstd4 wmaxstd5 wmaxstd6 wmaxstd7 wmaxstd8 wmaxstd9 wmaxstd10) ci contrast(difference) contrastvars(wmaxcontrast1 wmaxcontrast2 wmaxcontrast3 wmaxcontrast4 wmaxcontrast5 wmaxcontrast6 wmaxcontrast7 wmaxcontrast8 wmaxcontrast9)
		  
	list *contrast* if time==40, noobs
	
drop *std *contrast* time 



****Siblings

gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.

egen wmax2_bw = mean(wmax2), by(famid)
egen wmax3_bw = mean(wmax3), by(famid)
egen wmax4_bw = mean(wmax4), by(famid)
egen wmax5_bw = mean(wmax5), by(famid)
egen wmax6_bw = mean(wmax6), by(famid)
egen wmax7_bw = mean(wmax7), by(famid)
egen wmax8_bw = mean(wmax8), by(famid)
egen wmax9_bw = mean(wmax9), by(famid)
egen wmax10_bw = mean(wmax10), by(famid)

foreach var in bmi bmi_squared rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 age_conscription {
egen `var'_bw = mean(`var'), by(famid)
}


****Siblings - assuming proportional hazards


stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

range time 0 65 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==65, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 60 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==60, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 55 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==55, noobs
	drop *_bwcontrast* *std* time

	range time 0 50 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==50, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 45 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==45, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 40 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==40, noobs
	drop *_bwcontrast* *std* time

****Siblings - time-dependent effect 


stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription) id(LopNr) 

stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) tvc(wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw) dftvc(3) nolog 

 range time 0 65 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==65, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 60 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==60, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 55 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==55, noobs
	drop *_bwcontrast* *std* time

	range time 0 50 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==50, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 45 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==45, noobs
	drop *_bwcontrast* *std* time
	
	range time 0 40 2

standsurv, failure ///
at1(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at2(wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at3(wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at4(wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at5(wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at6(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0) ///
at7(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0) ///
at8(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0) ///
at9(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0) ///
at10(wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1) ///
timevar(time) ///
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci contrast(difference) contrastvars(diabetes_bwcontrast1 diabetes_bwcontrast2 diabetes_bwcontrast3 diabetes_bwcontrast4 diabetes_bwcontrast5 diabetes_bwcontrast6 diabetes_bwcontrast7 diabetes_bwcontrast8 diabetes_bwcontrast9)
							
	list *_bwcontrast* if time==40, noobs
	drop *_bwcontrast* *std* time
