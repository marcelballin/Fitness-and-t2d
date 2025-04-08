use "...dta", clear
drop std_rpg1 std_rpg2 std_rpg3 std_rpg4

replace FU_diabetes = FU_diabetes/365.24
gen v2_FU_diabetes = FU_diabetes + age_conscription

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
gen bmiow = 1 if bmicat==1|bmicat==2
replace bmiow = 2 if bmicat==3|bmicat==4
replace bmiow =bmiow-1

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

foreach covar in rounded_monstar  {
tab `covar', gen(`covar'_dum)
drop `covar'_dum1
}
ds *dum*
global covar = "`r(varlist)'"

gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.

foreach var in rounded_monstar_dum2 rounded_monstar_dum3 rounded_monstar_dum4 rounded_monstar_dum5 rounded_monstar_dum6 age_conscription {
egen `var'_bw = mean(`var'), by(famid)
}

ds *_bw
global bw_terms = "`r(varlist)'"



stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)

stpm3 @ns(wmax,df(3) winsor(1 99))##i.bmiow , /// ingen justering för mätt bmi
     knots(27.5 50 72.5, percentile) bknots(5 95, percentile)  scale(lncumhazard) eform
 
 local i = 1
 foreach var in  _ns_f1_wmax1 _ns_f1_wmax2 _ns_f1_wmax3 {
egen bw_wmaxspline`i' = mean(`var'), by(famid)
 	local i = `i'+1
} 


tab bmiow, gen(bmiow_dum)
 drop bmiow_dum1 
 local i = 1
 foreach var in  _ns_f1_wmax1 _ns_f1_wmax2 _ns_f1_wmax3 {

gen int_bmi_term`i' = bmiow_dum2*`var'
 	local i = `i'+1
} 


foreach var in int_bmi_term1 int_bmi_term2 int_bmi_term3 {
egen `var'_bw = mean(`var'), by(famid)
} 


stpm3 @ns(wmax,df(3) winsor(1 99))##i.bmiow int_bmi_term1_bw int_bmi_term2_bw int_bmi_term3_bw bw_wmaxspline1-bw_wmaxspline3 age_conscription $covar $bw_terms, /// 
     knots(27.5 50 72.5, percentile) bknots(5 95, percentile)  scale(lncumhazard) eform
	 
	 
	 
	 
	 
	///HR in those without overweight 

	
gen time = 65 in 1/1 
standsurv, hazard at2(wmax 180 bmiow 0) ///
at3(wmax 190 bmiow 0) ///
at1(wmax 200 bmiow 0) /// ref
at4(wmax 210 bmiow 0) ///
at5(wmax 220 bmiow 0) ///
at6(wmax 230 bmiow 0) ///
at7(wmax 240 bmiow 0) ///
at8(wmax 250 bmiow 0) ///
at9(wmax 260 bmiow 0) ///
at10(wmax 270 bmiow 0) ///
at11(wmax 280 bmiow 0) ///
at12(wmax 290 bmiow 0) ///
at13(wmax 300 bmiow 0) ///
at14(wmax 310 bmiow 0) ///
at15(wmax 320 bmiow 0) ///
at16(wmax 330 bmiow 0) ///
at17(wmax 340 bmiow 0) ///
at18(wmax 350 bmiow 0) ///
at19(wmax 360 bmiow 0) ///
at20(wmax 370 bmiow 0) ///
at21(wmax 380 bmiow 0) ///
at22(wmax 390 bmiow 0) ///
at23(wmax 400 bmiow 0) ///
at24(wmax 410 bmiow 0) ///
at25(wmax 420 bmiow 0) ///
timevar(time)  contrast(ratio) contrastvars(bmiobw0_*)    ci
drop _at*
standsurv, hazard at2(wmax 180 bmiow 1) ///
at3(wmax 190 bmiow 1) ///
at1(wmax 200 bmiow 1) /// ref
at4(wmax 210 bmiow 1) ///
at5(wmax 220 bmiow 1) ///
at6(wmax 230 bmiow 1) ///
at7(wmax 240 bmiow 1) ///
at8(wmax 250 bmiow 1) ///
at9(wmax 260 bmiow 1) ///
at10(wmax 270 bmiow 1) ///
at11(wmax 280 bmiow 1) ///
at12(wmax 290 bmiow 1) ///
at13(wmax 300 bmiow 1) ///
at14(wmax 310 bmiow 1) ///
at15(wmax 320 bmiow 1) ///
at16(wmax 330 bmiow 1) ///
at17(wmax 340 bmiow 1) ///
at18(wmax 350 bmiow 1) ///
at19(wmax 360 bmiow 1) ///
at20(wmax 370 bmiow 1) ///
at21(wmax 380 bmiow 1) ///
at22(wmax 390 bmiow 1) ///
at23(wmax 400 bmiow 1) ///
at24(wmax 410 bmiow 1) ///
at25(wmax 420 bmiow 1) ///
timevar(time)  contrast(ratio) contrastvars(bmiobw1_*)    ci
drop _at*
standsurv, hazard at2(wmax 180) ///
at3(wmax 190) ///
at1(wmax 200) /// ref
at4(wmax 210) ///
at5(wmax 220) ///
at6(wmax 230) ///
at7(wmax 240) ///
at8(wmax 250) ///
at9(wmax 260) ///
at10(wmax 270) ///
at11(wmax 280) ///
at12(wmax 290) ///
at13(wmax 300) ///
at14(wmax 310) ///
at15(wmax 320) ///
at16(wmax 330) ///
at17(wmax 340) ///
at18(wmax 350) ///
at19(wmax 360) ///
at20(wmax 370) ///
at21(wmax 380) ///
at22(wmax 390) ///
at23(wmax 400) ///
at24(wmax 410) ///
at25(wmax 420) ///
timevar(time)  contrast(ratio) contrastvars(bmiobw_*)    ci
		 
	*br time _at* _contrast* if time!=.
	keep bmiobw*
	keep in 1/1

	gen i = 1
	reshape long  bmiobw0_@ bmiobw0_@_uci bmiobw0_@_lci ///
	bmiobw1_@ bmiobw1_@_uci bmiobw1_@_lci /// 
	bmiobw_@ bmiobw_@_uci bmiobw_@_lci, i(i) j(j)
	drop i j
	gen wmax_scale = .
	local i = 1
foreach num of numlist  180 190 210(10)420    {
	replace wmax_scale = `num' in `i'/`i'
	local i = `i'+1
}
set obs 25
replace wmax_scale = 200 if wmax_scale==.

foreach var of varlist  bmiobw0_-bmiobw__uci    {

replace `var' = 1 if `var'==.
}

sort wmax_scale
	
	
		tw (rarea bmiobw0__lci bmiobw0__uci wmax_scale, color(blue%25)) ///
		(rarea bmiobw1__lci bmiobw1__uci wmax_scale, color(red%25)) /// 
		(line bmiobw0_ wmax_scale, color(blue)) ///
		(line bmiobw1_ wmax_scale, color(red)) if inrange(wmax_scale, 180, 410) ///
		,  name(graph, replace) yscale(log) legend(order(1 2))
		


save "...dta", replace
	
