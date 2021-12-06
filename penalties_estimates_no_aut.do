
global directory "....path of your directory here...." // Please enter here the name of the directory where you saved the dataset.
cd "$directory"

use "data.dta", clear

sort country y t

gen penalty = coefw - coefm

* Long-run penalties 5 to 10 years after childbirth

gen x=.
replace x=coefw-coefm if t>=5 & t<=10 
by country y: egen pnty510=mean(x)
replace pnty510=. if pnty510==0
drop x



// CHILD PENALTIES IN EARNINGS
// ===========================

* Scandinavia: Denmark, Sweden

sum pnty510 if t==1 & country == "Denmark" & y =="earnings"
global p510_earnings_dnk = -round(r(mean)*100, 1)

sum pnty510 if t==1 & country == "Sweden" & y =="earnings"
global p510_earnings_swe = -round(r(mean)*100, 1)

twoway (connected coefm coefw t if y=="earnings" & country=="Denmark", color(gs8 gs8)  msymbol(o o )  lpattern(dash solid) ) ///
	 (connected coefm coefw t if y=="earnings" & country=="Sweden", color(gs2 gs2) msymbol(t t) lpattern(dash solid)) ///
	 (scatteri -1 -0.5 0.2 -0.5 "", recast(line) lcolor(cranberry)) , ///
	 text( -.9 6 "{bf:Long-Run Penalty:}" "Denmark:  $p510_earnings_dnk%" "Sweden:  $p510_earnings_swe%", just(left) place(e)) ///
	 xlabel(-5(1)10) xtitle("Event Time (Years)") ylabel(-1(0.2)0.2) ytitle("Earnings Relative to Event Time -1") /// 
	 legend(order(1 2 3 4) size(small) col(2) label(1 "Men - Denmark") label(2 "Women - Denmark") label(3 "Men - Sweden") /// 
	 label(4 "Women - Sweden") /*pos(5) ring(0)*/ region(lwidth(none) fcolor(none))) ttext(0.15 1.2 "First Child Birth") graphregion(color(white)) 
graph export "ES_earnings_scandinavia.pdf", replace as(pdf)

	 
* German-speaking: Austria, Germany

sum pnty510 if t==1 & country == "Germany" & y =="earnings"
global p510_earnings_ger = -round(r(mean)*100, 1)

sum pnty510 if t==1 & country == "Austria" & y =="earnings"
global p510_earnings_aut = -round(r(mean)*100, 1)

twoway (connected coefm coefw t if y=="earnings" & country=="Germany", color(gs2 gs2) msymbol(t t) lpattern(dash solid) ) ///
	 /*(connected coefm coefw t if y=="earnings" & country=="Switzerland", color(teal teal) lpattern(dash solid) ) */ ///
	 (scatteri -1 -0.5 0.2 -0.5 "", recast(line) lcolor(cranberry)) , ///
	 text( -.9 6 "{bf:Long-Run Penalty:}" "Germany: $p510_earnings_ger%", just(left) place(e)) ///
	 xlabel(-5(1)10) xtitle("Event Time (Years)") ylabel(-1(0.2)0.2) ytitle("Earnings Relative to Event Time -1") /// 
	 legend(order(1 2 /*5 6*/) size(small) col(2) label(1 "Men - Germany") /// 
	 label(2 "Women - Germany") /*pos(5) ring(0)*/ region(lwidth(none) fcolor(none)) ) ttext(0.15 1.2 "First Child Birth") graphregion(color(white))
graph export "ES_earnings_Germany.pdf", replace as(pdf)
	 
* English-speaking: United Kingdom, US

sum pnty510 if t==1 & country == "United Kingdom" & y =="earnings"
global p510_earnings_uk = -round(r(mean)*100, 1)

sum pnty510 if t==1 & country == "United States" & y =="earnings"
global p510_earnings_us = -round(r(mean)*100, 1)

twoway (connected coefm coefw t if y=="earnings" & country=="United Kingdom", color(gs8 gs8)  msymbol(o o ) lpattern(dash solid)  ) ///
	 (connected coefm coefw t if y=="earnings" & country=="United States", color(gs2 gs2) msymbol(t t) lpattern(dash solid)) ///
	 (scatteri -1 -0.5 0.2 -0.5 "", recast(line) lcolor(cranberry)) , ///
	 text( -.9 5.8 "{bf:Long-Run Penalty:}" "United Kingdom:  $p510_earnings_uk%" "United States:  $p510_earnings_us%", just(left) place(e)) ///
	 xlabel(-5(1)10) xtitle("Event Time (Years)") ylabel(-1(0.2)0.2) ytitle("Earnings Relative to Event Time -1") /// 
	 legend(size(small) order(1 2 3 4) col(2) label(1 "Men - United Kingdom") label(2 "Women - United Kingdom") /// 
	 label(3 "Men - United States") label(4 "Women - United States") /*pos(5) ring(0)*/ region(lwidth(none) fcolor(none))) ttext(0.15 1.2 "First Child Birth") graphregion(color(white))
graph export "ES_earnings_anglo.pdf", replace as(pdf)
	 

	 
// CHILD PENALTIES IN LABOUR PARTICIPATION
// =======================================

sort country y t

* Scandinavia: Denmark, Sweden

sum pnty510 if t==1 & country == "Denmark" & y =="participation"
global p_participation_dnk = -round(r(mean)*100, 1)

sum pnty510 if t==1 & country == "Sweden" & y =="participation"
global p_participation_swe = -round(r(mean)*100, 1)

twoway (connected coefm coefw t if y=="participation" & country=="Denmark", color(gs8 gs8)  msymbol(o o )  lpattern(dash solid) ) ///
	 (connected coefm coefw t if y=="participation" & country=="Sweden", color(gs2 gs2) msymbol(t t) lpattern(dash solid)) ///
	 (scatteri -1 -0.5 0.2 -0.5 "", recast(line) lcolor(cranberry)) , ///
	 text( -.9 6 "{bf:Long-Run Penalty:}" "Denmark:  $p_participation_dnk% " "Sweden:  $p_participation_swe%", just(left) place(e)) ///
	 xlabel(-5(1)10) xtitle("Event Time (Years)") ylabel(-1(0.2)0.2) ytitle("Participation Rate Relative to Event Time -1") /// 
	 legend(order(1 2 3 4) size(small) col(2) label(1 "Men - Denmark") label(2 "Women - Denmark") label(3 "Men - Sweden") /// 
	 label(4 "Women - Sweden") /*pos(5) ring(0)*/ region(lwidth(none) fcolor(none))) ttext(0.15 1.2 "First Child Birth") graphregion(color(white)) 
graph export "ES_participation_scandinavia.pdf", replace as(pdf)

	 
* German-speaking: Austria, Germany

sum pnty510 if t==1 & country == "Austria" & y =="participation"
global p_participation_aut = -round(r(mean)*100,1)

sum pnty510 if t==1 & country == "Germany" & y =="participation"
global p_participation_ger = -round(r(mean)*100,1)

twoway (connected coefm coefw t if y=="participation" & country=="Austria", color(gs8 gs8)  msymbol(o o )  lpattern(dash solid) ) ///
		(connected coefm coefw t if y=="participation" & country=="Germany", color(gs2 gs2) msymbol(t t) lpattern(dash solid) ) ///
	 /*(connected coefm coefw t if y=="earnings" & country=="Switzerland", color(teal teal) lpattern(dash solid) ) */ ///
	 (scatteri -1 -0.5 0.2 -0.5 "", recast(line) lcolor(cranberry)) , ///
	 text( -.9 6 "{bf:Long-Run Penalty:}" "Austria:  $p_participation_aut% " "Germany:  $p_participation_ger%", just(left) place(e)) ///
	 xlabel(-5(1)10) xtitle("Event Time (Years)") ylabel(-1(0.2)0.2) ytitle("Participation Rate Relative to Event Time -1") /// 
	 legend(order(1 2 3 4 /*5 6*/) size(small) col(2) label(1 "Men - Austria") label(2 "Women - Austria") label(3 "Men - Germany") /// 
	 label(4 "Women - Germany") /*pos(5) ring(0)*/ region(lwidth(none) fcolor(none)) ) ttext(0.15 1.2 "First Child Birth") graphregion(color(white))
graph export "ES_participation_Germany.pdf", replace as(pdf)
	 
* English-speaking: United Kingdom, US

sum pnty510 if t==1 & country == "United Kingdom" & y =="participation"
global p_participation_uk = -round(r(mean)*100, 1)

sum pnty510 if t==1 & country == "United States" & y =="participation"
global p_participation_us = -round(r(mean)*100, 1)

twoway (connected coefm coefw t if y=="participation" & country=="United Kingdom", color(gs8 gs8)  msymbol(o o ) lpattern(dash solid)  ) ///
	 (connected coefm coefw t if y=="participation" & country=="United States", color(gs2 gs2) msymbol(t t) lpattern(dash solid)) ///
	 (scatteri -1 -0.5 0.2 -0.5 "", recast(line) lcolor(cranberry)) , ///
	 text( -.9 5.8 "{bf:Long-Run Penalty:}" "United Kingdom:  $p_participation_uk% " "United States:  $p_participation_us%", just(left) place(e)) ///
	 xlabel(-5(1)10) xtitle("Event Time (Years)") ylabel(-1(0.2)0.2) ytitle("Participation Rate Relative to Event Time -1") /// 
	 legend(size(small) order(1 2 3 4) col(2) label(1 "Men - United Kingdom") label(2 "Women - United Kingdom") /// 
	 label(3 "Men - United States") label(4 "Women - United States") /*pos(5) ring(0)*/ region(lwidth(none) fcolor(none))) ttext(0.15 1.2 "First Child Birth") graphregion(color(white))
graph export "ES_participation_anglo.pdf", replace as(pdf)



// CORRELATION NORMS - CHILD PENALTIES
// ===================================


keep if y=="earnings"
bysort country: gen n=_n
keep if n==1

gen comb_c=(wrkwom_preschkid_1 + wrkwom_youngestkidsch_1) / 2

label variable comb_c "Avg: wrkwom_preschkid_1, wrkwom_youngestkidsch_1"

replace pnty510=-pnty510

reg pnty510 comb_c
local slope =round(_b[comb_c],0.001)
local se =round(_se[comb_c],0.001)
local axistitle : variable label comb_c
egen min=min(comb_c)
egen max=max(comb_c)
local left=round(min-0.05,0.01)
local right=round(max+0.1,0.01)
global text=max-0.05
scatter pnty510 comb_c, mlabel(country) mlabposition(12) mlabcolor(black) mcolor(black) || lfit pnty510 comb_c, xtitle("Women With Children Under School Age" "or In School Should Stay at Home (Fraction Agreeing)") ytitle(Long-Run Child Penalty in Earnings) graphregion(color(white)) legend(off) xlabel($left $right) lcolor(gray) text(0.22 0.35 "Slope = `slope' (`se')")
graph export "earnings_home_preschoolkid.pdf", replace as(pdf)
drop min max



	 
