///Cohort vs Sibs


use "...dta", clear
gen method = "cohort"
append using "...dta", 
replace method = "sibling" if method==""

rename wmax_scale diabetes_graph



twoway  ///
				(rarea bmiobw0__uci bmiobw0__lci diabetes_graph,  color("21 101 192")) ///
								(rarea bmiobw1__uci bmiobw1__lci diabetes_graph, color("21 101 192%20")) ///
				(line bmiobw0_ diabetes_graph, lc(black)) ///		
				(line bmiobw1_ diabetes_graph, lc(black) lp(-)) ///
								(pci 1 180 1 408, lp(-) lc(black)) ///
								if inrange(diabetes_graph, 181, 407) & method=="cohort", ///
                               xlabel(#5, nogrid) ///
								xscale(range(180 407)extend) ///
								yscale(log range(0.39 1.15)) ///
								ylabel(0.4 0.6 0.8 1 1.1,  notick labcolor(black) angle(horiz) format(%4.2fc) ) ///
                                ytitle("", size(mediumsmall)) ///
                                xtitle("{bf:Cardiorespiratory fitness, watt}", size(mediumsmall)) ///
								plotregion(margin (3 3 3 3)) ///
								name(cohort, replace) graphregion(margin(1 1 1 1)) xsize(11) ysize(11) ///
legend(order(1 "{bf: Body mass index <25 kg/m{sup:2}}" 2 "{bf: Body mass index {&ge}25 kg/m{sup:2}}") row(2) size(small) ring (0) bplacement (9) yoffset(-13)) ///
		t2title("{bf:Cohort analysis}", size(large)) plotregion(margin(0 0 0 0)) ///
  										scheme(tab2) 
twoway  ///
				(rarea bmiobw0__uci bmiobw0__lci diabetes_graph,  color("255 195 0")) ///
								(rarea bmiobw1__uci bmiobw1__lci diabetes_graph, color("255 195 0%20")) ///
				(line bmiobw0_ diabetes_graph, lc(black)) ///		
				(line bmiobw1_ diabetes_graph, lc(black)  lp(-)) ///
								(pci 1 180 1 408, lp(-) lc(black)) ///
								if inrange(diabetes_graph, 181, 407) & method=="sibling", ///
                               xlabel(#5, nogrid) ///
								xscale(range(180 407)extend) ///
								yscale(log range(0.39 1.15)) ///
								ylabel(0.4 0.6 0.8 1 1.1,  notick labcolor(black) angle(horiz) format(%4.2fc) ) ///
                                ytitle("", size(mediumsmall)) ///
                                xtitle("{bf:Cardiorespiratory fitness, watt}", size(mediumsmall)) ///
								plotregion(margin (3 3 3 3)) ///
								name(sibs, replace) graphregion(margin(1 1 1 1)) xsize(11) ysize(11) ///
legend(order(1 "{bf: Body mass index <25 kg/m{sup:2}}" 2 "{bf: Body mass index {&ge}25 kg/m{sup:2}}") row(2) size(small) ring (0) bplacement (9) yoffset(-13)) ///
		t2title("{bf:Full-sibling analysis}", size(large)) plotregion(margin(0 0 0 0)) ///
  										scheme(tab2) 
									
											graph combine cohort sibs, row(1) name(combined, replace) scheme(tab2) ///
										l1title("{bf:Hazard ratio (95% CI)}")
