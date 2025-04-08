///Figure 1


use "..."


twoway  ///
				(rarea ub_diabetes lb_diabetes diabetes_graph,  color("21 101 192")) ///
				(line diabetes_hr diabetes_graph, lc(black)) ///
				(rarea ub_diabetes_bw lb_diabetes_bw diabetes_graph_bw, color("255 195 0")) ///
					(line diabetes_hr_bw diabetes_graph_bw, lpattern(dash) lwidth(medthick) lc(black)) ///
								(pci 1 180 1 408, lp(-) lc(black)) ///
								if inrange(diabetes_graph, 181, 407) | inrange(diabetes_graph_bw, 181, 407), ///
                               xlabel(#5, nogrid) ///
								xscale(range(180 407)extend) ///
								yscale(log range(0.29 1.2)) ///
								ylabel(0.3 0.5 0.75 1 1.15,  notick labcolor(black) angle(horiz) format(%4.2fc) ) ///
                                ytitle("{bf:Hazard ratio (95% CI)}", size(medium)) ///
                                xtitle("{bf:Cardiorespiratory fitness, watt}", size(medium)) ///
								plotregion(margin (3 3 3 3)) ///
								name(diabetes, replace) graphregion(margin(1 1 1 1)) xsize(11) ysize(11) ///
legend(order(1 "{bf: Cohort analysis}" 3 "{bf: Full-sibling analysis}") row(2) size(small) ring (0) bplacement (9) yoffset(-18)) ///
 plotregion(margin(0 0 0 0))  ///
  										scheme(tab2) 

										clear
										
		
////Supp figure 3 

use "...dta"
append using "...dta"

twoway  ///
				(rarea ub_diabetes lb_diabetes diabetes_graph,  color("21 101 192")) ///
				(line diabetes_hr diabetes_graph, lc(black)) ///
				(rarea ub_diabetes_bw lb_diabetes_bw diabetes_graph_bw, color("255 195 0")) ///
					(line diabetes_hr_bw diabetes_graph_bw, lpattern(dash) lwidth(medthick) lc(black)) ///
								(pci 1 180 1 408, lp(-) lc(black)) ///
								if inrange(diabetes_graph, 181, 407) | inrange(diabetes_graph_bw, 181, 407), ///
                               xlabel(#5, nogrid) ///
								xscale(range(180 407)extend) ///
								yscale(log range(0.2 1.3)) ///
								ylabel(0.25 0.5 0.75 1 1.25,  notick labcolor(black) angle(horiz) format(%4.2fc) ) ///
                                ytitle("{bf:Hazard ratio (95% CI)}", size(medium)) ///
                                xtitle("{bf:Cardiorespiratory fitness, watt}", size(medium)) ///
								plotregion(margin (3 3 3 3)) ///
								name(diabetes, replace) graphregion(margin(1 1 1 1)) xsize(11) ysize(11) ///
legend(order(1 "{bf: Cohort analysis}" 3 "{bf: Full-sibling analysis}") row(2) size(small) ring (0) bplacement (9) yoffset(-18)) plotregion(margin(0 0 0 0)) ///
  										scheme(tab2) 

