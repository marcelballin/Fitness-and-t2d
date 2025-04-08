///Main outcome


use "...dta"


twoway  ///
				(rarea ub_diabetes lb_diabetes diabetes_graph,  color(blue%50)) ///
				(line diabetes_hr diabetes_graph, lc(black)) ///
				(line diabetes_hr_bw diabetes_graph_bw, lc(black)) ///
				(rarea ub_diabetes_bw lb_diabetes_bw diabetes_graph_bw, color(red%50)) ///
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
legend(order(1 "{bf:Cohort analysis}" 4 "{bf:Full-sibling analysis}") row(2) size(small) ring (0) bplacement (9) yoffset(-5)) plotregion(margin(0 0 0 0))  ///
  										scheme(tab2) 



////T2D NPR outcome

use "...dta"
append using "...dta"

twoway  ///
				(rarea ub_diabetes lb_diabetes diabetes_graph,  color(blue%50)) ///
				(line diabetes_hr diabetes_graph, lc(black)) ///
				(line diabetes_hr_bw diabetes_graph_bw, lc(black)) ///
				(rarea ub_diabetes_bw lb_diabetes_bw diabetes_graph_bw, color(red%50)) ///
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
legend(order(1 "{bf:Cohort analysis}" 4 "{bf:Full-sibling analysis}") row(2) size(small) ring (0) bplacement (9) yoffset(-7)) plotregion(margin(0 0 0 0)) ///
  										scheme(tab2) 

