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

//Cohort analysuis 

range time 0 65 32

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
atvar(diabetes_std1 diabetes_std2 diabetes_std3 diabetes_std4 diabetes_std5 diabetes_std6 diabetes_std7 diabetes_std8 diabetes_std9 diabetes_std10) ci

keep *std* 
keep in 1/32
save "...dta", replace

clear 

///Sibling 
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

range time 0 65 32

stset v2_FU_diabetes, fail(outcome_diabetes==1) enter (age_conscription)
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
atvar(diabetes_bwstd1 diabetes_bwstd2 diabetes_bwstd3 diabetes_bwstd4 diabetes_bwstd5 diabetes_bwstd6 diabetes_bwstd7 diabetes_bwstd8 diabetes_bwstd9 diabetes_bwstd10) ci 

keep *std* 
keep in 1/32
save "...dta", replace

clear
use "...dta"
append using "...dta"
save "...dta", replace


****FIGURE 2 

use "...dta"
		
	
mylabels 0(5)35, local(labels) myscale(@/100) suffix("%")
 tw (line diabetes_std10 time if inrange(time, 20, 65), lc("21 101 192%10") lw(.4)) ///
(line diabetes_std9 time if inrange(time, 20, 65), lc("21 101 192%20") lw(.4)) ///
(line diabetes_std8 time if inrange(time, 20, 65), lc("21 101 192%30") lw(.4)) ///
(line diabetes_std7 time if inrange(time, 20, 65), lc("21 101 192%40") lw(.4)) ///
(line diabetes_std6 time if inrange(time, 20, 65), lc("21 101 192%50") lw(.4)) /// 
(line diabetes_std5 time if inrange(time, 20, 65), lc("21 101 192%60") lw(.4)) /// 
(line diabetes_std4 time if inrange(time, 20, 65), lc("21 101 192%70") lw(.4)) ///   
(line diabetes_std3 time if inrange(time, 20, 65), lc("21 101 192%80") lw(.4)) /// 
(line diabetes_std2 time if inrange(time, 20, 65), lc("21 101 192%90") lw(.4)) /// 
(line diabetes_std1 time if inrange(time, 20, 65), lc("21 101 192%100") lw(.4)), ///
scheme(tab2) ///
name(cohort, replace) ylabel(`labels', labsize(*1.5)) xtitle("")  ytitle("{bf:Standardised cumulative incidence}", size(medium))   xlabel(25(10)65, nogrid labsize(*1.5)) ///
 xscale(range(20 65))  xtitle("{bf:Age (years)}", size(medium)) ///
legend(order(10 "D1" 9 "D2" 8 "D3" 7 "D4" 6 "D5" 5 "D6" 4 "D7" 3 "D8" 2 "D9" 1 "D10") keygap(2.0) ring(0) pos(11)  yoffset(-8) xoffset(3) col(1) title("{bf:Deciles of fitness}", size(*.75) xoffset(0)) margin(0 0 0 0) rowgap(0.05) size(*1.5) nobox lcolor(black) region(fcolor(none) lcolor(black) lwidth(none)) lalign(outside) bmargin(tiny) forces) ///
 t1title("{bf:Cohort analysis}", size(large))  plotregion(margin(0 0 0 0))  
 
 mylabels 0(5)35, local(labels) myscale(@/100) suffix("%")
 tw (line diabetes_bwstd10 time if inrange(time, 20, 65), lc(red%10) lw(.4)) ///
(line diabetes_bwstd9 time if inrange(time, 20, 65), lc("255 195 0%20") lw(.4)) ///
(line diabetes_bwstd8 time if inrange(time, 20, 65), lc("255 195 0%30") lw(.4)) ///
(line diabetes_bwstd7 time if inrange(time, 20, 65), lc("255 195 0%40") lw(.4)) ///
(line diabetes_bwstd6 time if inrange(time, 20, 65), lc("255 195 0%50") lw(.4)) /// 
(line diabetes_bwstd5 time if inrange(time, 20, 65), lc("255 195 0%60") lw(.4)) /// 
(line diabetes_bwstd4 time if inrange(time, 20, 65), lc("255 195 0%70") lw(.4)) ///   
(line diabetes_bwstd3 time if inrange(time, 20, 65), lc("255 195 0%80") lw(.4)) /// 
(line diabetes_bwstd2 time if inrange(time, 20, 65), lc("255 195 0%90") lw(.4)) /// 
(line diabetes_bwstd1 time if inrange(time, 20, 65), lc("255 195 0%100") lw(.4)), ///
scheme(tab2) ///
name(sibs, replace) ylabel(`labels', labsize(*1.5)) xtitle("")  ytitle("{bf:Standardised cumulative incidence}", size(medium))   xlabel(25(10)65, nogrid labsize(*1.5)) ///
 xscale(range(20 65))  xtitle("{bf:Age (years)}", size(medium)) ///
legend(order(10 "D1" 9 "D2" 8 "D3" 7 "D4" 6 "D5" 5 "D6" 4 "D7" 3 "D8" 2 "D9" 1 "D10") keygap(2.0) ring(0) pos(11)  yoffset(-8) xoffset(3) col(1) title("{bf:Deciles of fitness}", size(*.75) xoffset(0)) margin(0 0 0 0) rowgap(0.05) size(*1.5) nobox lcolor(black) region(fcolor(none) lcolor(black) lwidth(none)) lalign(outside) bmargin(tiny) forces) ///
 t1title("{bf:Full-sibling analysis}", size(large))  plotregion(margin(0 0 0 0))  
 
 
 
  graph combine cohort sibs, ycommon row(1) xsize(14) ysize(6)  name(combined, replace) graphregion(margin(0 0 0 0))  /// 
  altshrink scheme(tab2) 

  
  
  input str20 outcome	str10	decile	rd_kohort	lb_kohort	ub_kohort	rd_sib	lb_sib	ub_sib	
"acm"	"d1"	.	.	.	.	.	.
"acm"	"d2"	-4.3	-4.8	-3.8	-2.3	-1.3	-3.3
"acm"	"d3"	-6.1	-5.7	-6.6	-3.3	-2.3	-4.2
"acm"	"d4"	-7.8	-7.3	-8.3	-4.1	-3.0	-5.1
"acm"	"d5"	-9.7	-9.2	-10.2	-5.6	-4.5	-6.6
"acm"	"d6"	-11.0	-10.5	-11.5	-6.5	-5.4	-7.6
"acm"	"d7"	-12.2	-11.6	-12.7	-7.7	-6.6	-8.8
"acm"	"d8"	-14.1	-13.6	-14.6	-9.1	-8.0	-10.3
"acm"	"d9"	-15.8	-15.3	-16.4	-9.7	-8.5	-10.9
"acm"	"d10"	-17.8	-17.3	-18.3	-10.9	-12.1	-9.7
end

egen rank = group(decile)

replace rd_kohort = 0 if decile == "d1"
replace rd_sib = 0 if decile == "d1"

gen rank_sib = rank + .125


replace rank = rank - .125

  
 tw  ///
 (rspike lb_kohort	ub_kohort rank  if outcome=="acm", lc(black) lw(.5)) ///
  (rspike lb_sib	ub_sib rank_sib  if outcome=="acm", lc(black) lw(.5)) ///
(scatter rd_kohort rank if outcome=="acm", mlcolor(black) mlwidth(.3) mfcolor("21 101 192") ms(O) msize(*1.5)) ///
 (scatter rd_sib rank_sib if outcome=="acm", mfcolor("255 195 0") mlwidth(.3) ms(S) msize(*1.5)), ///
 yscale(range(-21 1)) ylabel(-20 "-20%" -15 "-15%" -10 "-10%" -5 "-5%" 0, labsize(*1.5)) scheme(tab2) ///
 xscale(range(.75 10.25)) ///
 xlabel(1 `" "D1""(202 Watt)" "' 2 `" "D2""(222 Watt)" "' 3 `" "D3""(240 Watt)" "' 4 `" "D4""(251 Watt)" "' 5 `" "D5""(264 Watt)" "' 6 `" "D6""(279 Watt)" "' 7 `" "D7""(294 Watt)" "' 8 `" "D8""(313 Watt)" "' 9 `" "D9""(333 Watt)" "' 10 `" "D10""(367 Watt)" "', nogrid notick) ///
 name(rd, replace) xtitle("{bf:Deciles of fitness}", size(medium))  ytitle("{bf:Incidence difference at age 65 (95% CI)}", size(medium)) ///
 legend(order(3 "{bf:Cohort analysis}" 4 "{bf:Full-sibling analysis}") row(2) size(medium) ring (0) bplacement (2) yoffset(-7)) plotregion(margin(0 0 0 0))
  
 

  graph combine combined rd, row(2) name(rdinc, replace) xsize(8) ysize(8) graphregion(margin(0 0 0 0)) ///
  altshrink graphregion(margin(0 0 0 0)) ///
  scheme(tab2)
  

