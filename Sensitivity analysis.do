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


gen vo2max = 1.76 * (wmax * 6.12 / mass_i_kg) + 3.5

xtile vo2max_decile = vo2max, nq(10)

tab vo2max_decile, gen(dummyvo2max)
rename dummyvo2max1 vo2max1
rename dummyvo2max2 vo2max2
rename dummyvo2max3 vo2max3
rename dummyvo2max4 vo2max4
rename dummyvo2max5 vo2max5
rename dummyvo2max6 vo2max6
rename dummyvo2max7 vo2max7
rename dummyvo2max8 vo2max8
rename dummyvo2max9 vo2max9
rename dummyvo2max10 vo2max10

gen MET = vo2max/3.5

gen wmax_perkg = wmax/mass_i_kg

xtile wmax_perkg_decile = wmax_perkg, nq(10)

tab wmax_perkg_decile, gen(dummywmaxperkg)
rename dummywmaxperkg1 wmax_perkg1
rename dummywmaxperkg2 wmax_perkg2
rename dummywmaxperkg3 wmax_perkg3
rename dummywmaxperkg4 wmax_perkg4
rename dummywmaxperkg5 wmax_perkg5
rename dummywmaxperkg6 wmax_perkg6
rename dummywmaxperkg7 wmax_perkg7
rename dummywmaxperkg8 wmax_perkg8
rename dummywmaxperkg9 wmax_perkg9
rename dummywmaxperkg10 wmax_perkg10

foreach covar in rounded_monstar maxedu maxinco {
tab `covar', gen(`covar'_dum)
drop `covar'_dum1
}
ds *dum*
global covar = "`r(varlist)'"

///Cohort analysis

bysort wmax_decile: tab outcome_diabetes
bysort vo2max_decile: tab outcome_diabetes

gen age_diabetes = v2_FU_diabetes if outcome_diabetes==1
sum age_diabetes, d
bysort wmax_decile: sum age_diabetes if outcome_diabetes==1, d

range time 0 65 2

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform
est store diabetes_decile

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
atvar(diabetes_std1 diabetes_std2 diabetes_std3 diabetes_std4 diabetes_std5 diabetes_std6 diabetes_std7 diabetes_std8 diabetes_std9 diabetes_std10) ci contrast(difference) contrastvars(diabetes_contrast1 diabetes_contrast2 diabetes_contrast3 diabetes_contrast4 diabetes_contrast5 diabetes_contrast6 diabetes_contrast7 diabetes_contrast8 diabetes_contrast9)

	list *_contrast* if time==65, noobs

estout *decile* using HRscohort.xlsx, cells("b(fmt(2)) ci(fmt(2))") keep(*wmax*) eform

///With adjustment for HGRP
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared hgrp $covar if hgrp>=100, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

///With adjustment for KNST
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared knst $covar if knst>=100, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

///With adjustment for height
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared lngd_i_cm $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

///With adjustment for SBP 
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 syst age_conscription bmi bmi_squared $covar if syst>=100 & syst<=180, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

///Using estimated VO2max
stpm2 vo2max2 vo2max3 vo2max4 vo2max5 vo2max6 vo2max7 vo2max8 vo2max9 vo2max10 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

///Per 1-MET increase
stpm2 MET age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

///Using wmax per kg
stpm2 wmax_perkg2 wmax_perkg3 wmax_perkg4 wmax_perkg5 wmax_perkg6 wmax_perkg7 wmax_perkg8 wmax_perkg9 wmax_perkg10 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform



///COHORT ANALYSIS T2D NPR OUTCOME

stset v2_FU_diabetes2, fail(outcome_diabetes2==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform

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
atvar(diabetes_std1 diabetes_std2 diabetes_std3 diabetes_std4 diabetes_std5 diabetes_std6 diabetes_std7 diabetes_std8 diabetes_std9 diabetes_std10) ci contrast(difference) contrastvars(diabetes_contrast1 diabetes_contrast2 diabetes_contrast3 diabetes_contrast4 diabetes_contrast5 diabetes_contrast6 diabetes_contrast7 diabetes_contrast8 diabetes_contrast9)

			
	list *_contrast* if time==65, noobs
	
	
///RESTRICTED TO CONSCRIPTS FROM 1985 ONWARDS
keep if monstar>=1985

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 age_conscription bmi bmi_squared $covar, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) eform


clear 


****Siblings


///Sibling analysis

gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.

bysort wmax_decile: tab outcome_diabetes if antal >=2 & antal!=.

egen wmax2_bw = mean(wmax2), by(famid)
egen wmax3_bw = mean(wmax3), by(famid)
egen wmax4_bw = mean(wmax4), by(famid)
egen wmax5_bw = mean(wmax5), by(famid)
egen wmax6_bw = mean(wmax6), by(famid)
egen wmax7_bw = mean(wmax7), by(famid)
egen wmax8_bw = mean(wmax8), by(famid)
egen wmax9_bw = mean(wmax9), by(famid)
egen wmax10_bw = mean(wmax10), by(famid)

egen vo2max2_bw = mean(vo2max2), by(famid)
egen vo2max3_bw = mean(vo2max3), by(famid)
egen vo2max4_bw = mean(vo2max4), by(famid)
egen vo2max5_bw = mean(vo2max5), by(famid)
egen vo2max6_bw = mean(vo2max6), by(famid)
egen vo2max7_bw = mean(vo2max7), by(famid)
egen vo2max8_bw = mean(vo2max8), by(famid)
egen vo2max9_bw = mean(vo2max9), by(famid)
egen vo2max10_bw = mean(vo2max10), by(famid)


egen MET_bw = mean(MET), by(famid)


egen wmax_perkg2_bw = mean(wmax_perkg2), by(famid)
egen wmax_perkg3_bw = mean(wmax_perkg3), by(famid)
egen wmax_perkg4_bw = mean(wmax_perkg4), by(famid)
egen wmax_perkg5_bw = mean(wmax_perkg5), by(famid)
egen wmax_perkg6_bw = mean(wmax_perkg6), by(famid)
egen wmax_perkg7_bw = mean(wmax_perkg7), by(famid)
egen wmax_perkg8_bw = mean(wmax_perkg8), by(famid)
egen wmax_perkg9_bw = mean(wmax_perkg9), by(famid)
egen wmax_perkg10_bw = mean(wmax_perkg10), by(famid)

foreach var in syst lngd_i_cm hgrp knst bmi bmi_squared rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 age_conscription {
egen `var'_bw = mean(`var'), by(famid)
}


///With adjustment for HGRP
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw hgrp hgrp_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw if hgrp>=100, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

///With adjustment for KNST
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw knst knst_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw if knst>=100, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

///With adjustment for height
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw bmi bmi_bw bmi_squared bmi_squared_bw lngd_i_cm lngd_i_cm_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

///With adjustment for SBP
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw bmi bmi_bw bmi_squared bmi_squared_bw syst syst_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw if syst>=100 & syst <=180, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

///Using estimated VO2max
stpm2 vo2max2 vo2max3 vo2max4 vo2max5 vo2max6 vo2max7 vo2max8 vo2max9 vo2max10 vo2max2_bw vo2max3_bw vo2max4_bw vo2max5_bw vo2max6_bw vo2max7_bw vo2max8_bw vo2max9_bw vo2max10_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

////Per 1-MET increase
stpm2 MET MET_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

///Using wmax/kg
stpm2 wmax_perkg2 wmax_perkg3 wmax_perkg4 wmax_perkg5 wmax_perkg6 wmax_perkg7 wmax_perkg8 wmax_perkg9 wmax_perkg10 wmax_perkg2_bw wmax_perkg3_bw wmax_perkg4_bw wmax_perkg5_bw wmax_perkg6_bw wmax_perkg7_bw wmax_perkg8_bw wmax_perkg9_bw wmax_perkg10_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform

	

/// T2D NPR OUTCOME

stset v2_FU_diabetes2, fail(outcome_diabetes2==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform
est store diabetes_decilebw

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
	
	
///RESTRICTED TO CONSCRIPTS FROM 1985 ONWARDS
keep if monstar>=1985

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
stpm2 wmax2 wmax3 wmax4 wmax5 wmax6 wmax7 wmax8 wmax9 wmax10 wmax2_bw wmax3_bw wmax4_bw wmax5_bw wmax6_bw wmax7_bw wmax8_bw wmax9_bw wmax10_bw bmi bmi_bw bmi_squared bmi_squared_bw age_conscription age_conscription_bw rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 rounded_monstar_dum2_bw rounded_monstar_dum3_bw rounded_monstar_dum4_bw rounded_monstar_dum5_bw rounded_monstar_dum6_bw, knots(27.5 50 72.5) bknots(5 95) knscale(centile) scale(hazard) vce(robust) eform
