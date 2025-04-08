***Calculate number of outcome-discordant

gen i = 1
bys famid: egen antal = total(i) if famid!=.
keep if antal >=2 & antal!=.

gen primary_outcome=1 if outcome_diabetes==1
replace primary_outcome=0 if outcome_diabetes!=1

bys famid: egen mean_outcome=mean(primary_outcome)
count if mean_outcome!=0 & mean_outcome!=1

duplicates report famid if mean_outcome!=0 & mean_outcome!=1

****Number of continuous exposure-discordant

egen wmax_bw = mean(wmax), by(famid)
gen diff_w_b = abs(wmax-wmax_bw)
count if diff_w_b!=0

****Number of doubly-discordant based on continus fitness
count if mean_outcome!=0 & mean_outcome!=1 & diff_w_b!=0

****Number of doubly-discordant based on fitness deciles 

gen crf1=1 if wmax1==1
replace crf1=0 if wmax1!=1
bys famid: egen mean_d1=mean(crf1)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d1!=0 & mean_d1!=1

gen crf2=1 if wmax2==1
replace crf2=0 if wmax2!=1
bys famid: egen mean_d2=mean(crf2)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d2!=0 & mean_d2!=1

gen crf3=1 if wmax3==1
replace crf3=0 if wmax3!=1
bys famid: egen mean_d3=mean(crf3)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d3!=0 & mean_d3!=1

gen crf4=1 if wmax4==1
replace crf4=0 if wmax4!=1
bys famid: egen mean_d4=mean(crf4)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d4!=0 & mean_d4!=1

gen crf5=1 if wmax5==1
replace crf5=0 if wmax5!=1
bys famid: egen mean_d5=mean(crf5)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d5!=0 & mean_d5!=1

gen crf6=1 if wmax6==1
replace crf6=0 if wmax6!=1
bys famid: egen mean_d6=mean(crf6)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d6!=0 & mean_d6!=1

gen crf7=1 if wmax7==1
replace crf7=0 if wmax7!=1
bys famid: egen mean_d7=mean(crf7)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d7!=0 & mean_d7!=1

gen crf8=1 if wmax8==1
replace crf8=0 if wmax8!=1
bys famid: egen mean_d8=mean(crf8)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d8!=0 & mean_d8!=1

gen crf9=1 if wmax9==1
replace crf9=0 if wmax9!=1
bys famid: egen mean_d9=mean(crf9)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d9!=0 & mean_d9!=1

gen crf10=1 if wmax10==1
replace crf10=0 if wmax10!=1
bys famid: egen mean_d10=mean(crf10)
count if mean_outcome!=0 & mean_outcome!=1 & mean_d10!=0 & mean_d10!=1








