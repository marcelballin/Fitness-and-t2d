*****Overweight effect modification

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

gen bmiow = 1 if bmicat==1|bmicat==2
replace bmiow = 2 if bmicat==3|bmicat==4

tab bmiow, gen(dummybmi)
rename dummybmi1 bmi1
rename dummybmi2 bmi2

gen bmi1wmax2=bmi1*wmax2
gen bmi2wmax2=bmi2*wmax2

gen bmi1wmax3=bmi1*wmax3
gen bmi2wmax3=bmi2*wmax3

gen bmi1wmax4=bmi1*wmax4
gen bmi2wmax4=bmi2*wmax4

gen bmi1wmax5=bmi1*wmax5
gen bmi2wmax5=bmi2*wmax5
gen bmi1wmax6=bmi1*wmax6
gen bmi2wmax6=bmi2*wmax6

gen bmi1wmax7=bmi1*wmax7
gen bmi2wmax7=bmi2*wmax7

gen bmi1wmax8=bmi1*wmax8
gen bmi2wmax8=bmi2*wmax8

gen bmi1wmax9=bmi1*wmax9
gen bmi2wmax9=bmi2*wmax9

gen bmi1wmax10=bmi1*wmax10
gen bmi2wmax10=bmi2*wmax10

***Cohort analysis

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 bmi2 age_conscription bmi2wmax2 bmi2wmax3 bmi2wmax4 bmi2wmax5 bmi2wmax6 bmi2wmax7 bmi2wmax8 bmi2wmax9 bmi2wmax10 $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

range time 0 65 2
	
	///HR in those without overweight 
standsurv, hazard ///
at1(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at2(bmi2 0 wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at3(bmi2 0 wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at4(bmi2 0 wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at5(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at6(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at7(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at8(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at9(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at10(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
timevar(time) ci contrast(ratio) contrastvars(t2d_normcontrast1 t2d_normcontrast2 t2d_normcontrast3 t2d_normcontrast4 t2d_normcontrast5 t2d_normcontrast6 t2d_normcontrast7 t2d_normcontrast8 t2d_normcontrast9) 

	list *_normcontrast* if time==65, noobs
	
	drop *_at* *contrast*
	
	///HR in those with overweight 
standsurv, hazard ///
at1(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at2(bmi2 1 wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 1 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at3(bmi2 1 wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 1 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at4(bmi2 1 wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 1 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at5(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 1 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at6(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 1 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at7(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 1 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at8(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 1 bmi2wmax9 0 bmi2wmax10 0) ///
at9(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 1 bmi2wmax10 0) ///
at10(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 1) ///
timevar(time) ci contrast(ratio) contrastvars(t2d_owcontrast1 t2d_owcontrast2 t2d_owcontrast3 t2d_owcontrast4 t2d_owcontrast5 t2d_owcontrast6 t2d_owcontrast7 t2d_owcontrast8 t2d_owcontrast9) 

	list *_owcontrast* if time==65, noobs
	
		drop *_at* *contrast*

		///HR in total population 
	standsurv, hazard ///
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
ci contrast(ratio) contrastvars(total_t2dcontrast1 total_t2dcontrast2 total_t2dcontrast3 total_t2dcontrast4 total_t2dcontrast5 total_t2dcontrast6 total_t2dcontrast7 total_t2dcontrast8 total_t2dcontrast9) 

	list *total_t2dcontrast* if time==65, noobs
	
	drop *_at* *contrast*
	
	drop *contrast*
	
****Sibling analysis

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

foreach var in bmi2 bmi2wmax2 bmi2wmax3 bmi2wmax4 bmi2wmax5 bmi2wmax6 bmi2wmax7 bmi2wmax8 bmi2wmax9 bmi2wmax10 rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 age_conscription {
egen `var'_bw = mean(`var'), by(famid)
}

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw bmi2 bmi2_bw bmi2wmax2 bmi2wmax3 bmi2wmax4 bmi2wmax5 bmi2wmax6 bmi2wmax7 bmi2wmax8 bmi2wmax9 bmi2wmax10 bmi2wmax2_bw bmi2wmax3_bw bmi2wmax4_bw bmi2wmax5_bw bmi2wmax6_bw bmi2wmax7_bw bmi2wmax8_bw bmi2wmax9_bw bmi2wmax10_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform
	
	///HR in those wihtout overweight 
standsurv, hazard ///
at1(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at2(bmi2 0 wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at3(bmi2 0 wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at4(bmi2 0 wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at5(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at6(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at7(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at8(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at9(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at10(bmi2 0 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
timevar(time) ci contrast(ratio) contrastvars(t2d_normcontrast1 t2d_normcontrast2 t2d_normcontrast3 t2d_normcontrast4 t2d_normcontrast5 t2d_normcontrast6 t2d_normcontrast7 t2d_normcontrast8 t2d_normcontrast9) 

	list *_normcontrast* if time==65, noobs
	
	drop *_at* *contrast*
	
	///HR in those with overweight 
standsurv, hazard ///
at1(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at2(bmi2 1 wmax2 1 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 1 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at3(bmi2 1 wmax2 0 wmax3 1 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 1 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at4(bmi2 1 wmax2 0 wmax3 0 wmax4 1 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 1 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at5(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 1 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 1 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at6(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 1 wmax7 0 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 1 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at7(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 1 wmax8 0 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 1 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 0) ///
at8(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 1 wmax9 0 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 1 bmi2wmax9 0 bmi2wmax10 0) ///
at9(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 1 wmax10 0 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 1 bmi2wmax10 0) ///
at10(bmi2 1 wmax2 0 wmax3 0 wmax4 0 wmax5 0 wmax6 0 wmax7 0 wmax8 0 wmax9 0 wmax10 1 bmi2wmax2 0 bmi2wmax3 0 bmi2wmax4 0 bmi2wmax5 0 bmi2wmax6 0 bmi2wmax7 0 bmi2wmax8 0 bmi2wmax9 0 bmi2wmax10 1) ///
timevar(time) ci contrast(ratio) contrastvars(t2d_owcontrast1 t2d_owcontrast2 t2d_owcontrast3 t2d_owcontrast4 t2d_owcontrast5 t2d_owcontrast6 t2d_owcontrast7 t2d_owcontrast8 t2d_owcontrast9) 

	list *_owcontrast* if time==65, noobs
	
	drop *_at* *contrast*
		
		///HR in total population 
	standsurv, hazard ///
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
ci contrast(ratio) contrastvars(total_t2dcontrast1 total_t2dcontrast2 total_t2dcontrast3 total_t2dcontrast4 total_t2dcontrast5 total_t2dcontrast6 total_t2dcontrast7 total_t2dcontrast8 total_t2dcontrast9) 

	list *total_t2dcontrast* if time==65, noobs
	
	drop *_at* *contrast*
