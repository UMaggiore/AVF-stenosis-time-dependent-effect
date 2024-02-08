///////////////////////////////////////////////////////////////////////////////
**# START PREPARING DATASET FOR ANALYSES
///////////////////////////////////////////////////////////////////////////////
cap log using  "C:\Documenti\Morganti\analysis_afv_`c(current_date)'.smcl", replace
clear
import excel "C:\Documenti\Morganti\File maggiore.xlsx", sheet("Foglio1") firstrow
summ time_m
gen fail = cond(time_m == 12 & restenosi_1_anno == 0, 0, 1)
tab fail
label define treat 1 "PTA" 2 "DCB"
label values treat treat
replace time_m = 1 / (365.25 / 12) if time_m == 0
stset time_m, fail(fail == 1)

label define yesno 0 "No" 1 "Yes"
label var eta_conf "Age, years"
label var sesso "Gender"
label define sesso 0 "M" 1 "F"
label values sesso sesso
label var nefr_di_base "Primary kiney disese"
label define nefr_di_base 1 "ADPKD" 2 "Diabetes" 3 "Unkonwn" 4 "Glomerulonephritis" 5 "Nephroangiosclerosis" 6 "Others"
label values nefr_di_base nefr_di_base
label var diabete "Diabetes"
label values diabete yesno
label var ipertensione "Hypertension"
label values ipertensione yesno
label var pat_cardiov "Cardiovascular disease"
label values pat_cardiov yesno
label var aocp "Peripheral artery disease"
label values aocp yesno
label var aocp "Amputation"
label values aocp yesno
label var fistola_1 "Previous AVF"
label values fistola_1 yesno
label var cvc_omo "omolateral CVC"
label values cvc_omo yesno
label var tipo_fav "AVF type"
label define tipo_fav 1 "Left distal" 2 "Left middle arm" 3 "Left proxymal" 4 "Right distal" 5 "Right middle arm" 6 "Right proxymal" 
label values tipo_fav tipo_fav
label var tipo_fav "AVF graft"
label values tipo_fav yesno
label var stent "Stent placement"
label values stent yesno
label var tipo_stenosi "Stenosis type"
label define tipo_stenosi 0 "Unifocal" 1 "Multifocal"
label values tipo_stenosi tipo_stenosi

gen quasiID = _n
gen ltime = time_m -1
replace ltime = 0 if ltime < 0
gen rtime = .
replace rtime = time_m if time_m > = 1
replace ltime  = 12 if time_m == 12 & fail == 0
replace rtime = . if rtime == 12 & fail == 0
replace rtime = 1 if time_m == 0.5


cd "C:\Documenti\Morganti"
save morganti_fav, replace


///////////////////////////////////////////////////////////////////////////////
**# END PREPARING DATASET FOR ANALYSES
///////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
**# START TABLE 1
///////////////////////////////////////////////////////////////////////////////
clear
cd "C:\Documenti\Morganti"
use morganti_fav
dtable eta_conf i.sesso i.nefr_di_base i.diabete i.ipertensione i.pat_cardiov  ////
	i.aocp  i.fistola_1 i.cvc_omo i.tipo_fav i.stent i.tipo_stenosi ///
	, ///	
	by(treat, nototals tests) ///
	factor(sesso nefr_di_base diabete ipertensione pat_cardiov  ////
	aocp  fistola_1 cvc_omo tipo_fav stent tipo_stenosi , test(fisher)) ///
	define(meansd = mean sd, delimiter(" ± ")) ///
	define(myiqr = p25 p75, delimiter("-")) ///
	define(myrange = min max, delimiter("-")) ///
    continuous(eta_conf , stat(meansd)) ///   
	continuous(eta_conf,  test(kwallis)) ///
	column(by(hide) test(p-value)) ///
	title(Table 1. Population characteristic by PTA use ) ///
	note(Mann-Whitney test for continuous variables (reported as mean ± standard deviation).) ///
    note(Fisher's exact test for categorical variables (reported as number (percentage)).) ///
	note(Baseline characteristics of the study population) ///
	sformat("%s" sd) ///
	nformat("%3.1f" mean sd median p25 p75) ///
	nformat("%3.1f" min max) ///
	sformat("(%s)" myiqr myrange) ///
    nformat("%3.0f" N count fvfrequency) ///
    nformat("%3.1f" fvpercent ) ///
    nformat("%6.3f" kwallis fisher) ///
	export(table1.html, replace)
collect export Table S1.xlsx, replace
collect export Table S1.docx, replace


//////////////////////////////////////////////////////////////////////////////
**# END TABLE 1
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
**# START SURVIVAL ANALYSIS
///////////////////////////////////////////////////////////////////////////////
clear
cd "C:\Documenti\Morganti"
use morganti_fav

* Draw Kaplan-Meier curves
#delimit ;
global stuff  "risktable(, title("N at risk", size(*.8))) 
     risktable(, rowtitle("PTA:  ") group(#1) size(*.8)) 
     risktable(, rowtitle("DCB:  ") group(#2) size(*.8)) 
	  legend(cols(1) position(3) symxsize(10) rowgap(0.5) size(*.7) 
	  lstyle(none) lab(1 "PTA") 
	  lab(2 "DCB")
	  order(1 2) 
	  )   
	  ysc(range(0 1)) 
	  ylab(0 "0" .2 "20" .40 "40" .6 "60" .80 "80" 1 "100", 
	  angle(horizontal) labsize(*.7) grid ) 
	  xsc(titlegap(5)) xlab(0(1)12, format(%3.0f) labsize(*1))";
#delimit cr



**# Interval censored Cox regression
stintcox treat, interval(ltime rtime) ///
  emhsgtolerance(1e-4) tvc(treat) vce(cluster ID) 
stcurve, survival at(treat=(0 1)) ///
	ylab(0 "0" .2 "20" .40 "40" .6 "60" .80 "80" 1 "100") ///
	xlab(0(1)12) ///
	ytitle("Patients with Patent AVF (%)") ///
	xtitle("Month Since the Procedure")  ///
	xsc(titlegap(5)) xlab(0(1)12, format(%3.0f)) ///
	legend(cols(1) position(3) symxsize(10) rowgap(0.5) size(*.7) ///
	  lstyle(none) lab(1 "PTA") ///
	  lab(2 "DCB") ///
	  order(1 2)  ///
	  )   ///
	  ysc(range(0 1)) ///
	  note("Crude AVF as estimated by interval censored Cox model")
graph export Figure1.png, replace

 

**# Crude Model
stintcox treat, interval(ltime rtime) tvc(treat)  vce(cluster ID)) 

**# Multivariable Adjusted
cap drop treat1
gen treat1 = treat
qui stcox i.treat i.pat_cardiov i.tipo_stenosi i.fistola_1 , tvc(treat1)
qui table () (command result), ///
          command(_r_b _r_ci _r_p ///
                  : qui stintcox treat i.pat_cardiov i.tipo_stenosi i.fistola_1, interval(ltime rtime) ///
							tvc(treat)  vce(cluster ID))  ///
          nformat(%5.2f  _r_b _r_ci ) ///
		  nformat(%4.3f  _r_p) ///
          sformat("(%s)"  _r_ci ) ///
          cidelimiter(" to")
collect label levels command 1 "Interval censored Cox regression with time-varying treatment effect on AVF closure", modify
collect label levels result _r_b "Hazard Ratio", modify
collect label levels result _r_ci "95% CI", modify
collect label levels result _r_p "P value", modify
collect style showbase off
collect style row stack, nobinder delimiter(" x ")
collect style cell border_block, border(right, pattern(nil))
collect label levels colname treat "HR at time 0", modify
collect label levels colname treat1 "HR Monthly change", modify
collect notes 1: "The HR is the rate of AVF closure after PTA divided by the rate of AVF closure after DCB"
collect notes 2: "HR, hazard ratio; CI, confidence interval; main, main effect (HR at time =0); tvc, time-varying coefficient; HR Monthly change: monthly change in the HR at time 0"
collect export Table S2.docx, replace
collect export Table S2.xlsx, replace
collect export Table S2.html, replace

 

**# Flexible parametric survival models
stpm2 treat i.pat_cardiov i.tipo_stenosi i.fistola_1, scale(hazard) df(3)  tvc(treat) dftvc(2)  eform 
predict hr, hrnumerator(treat 2) ci
predict survdiff, sdiff1(treat 2) ci
predict hazarddiff, hdiff1(treat 2) ci

sort _t
tw line survdiff _t, lcolor(black) || ///
   rarea survdiff_uci survdiff_lci _t,  fcolor(navy%30) lcolor(navy%30) || ///
	, ///
	ylab(-0.40 "-40" -.2 "-20" 0 "0" .20 "+20" .40 "+40" .6 "+60" .8 "+80") ///
	xlab(0(1)12) ///
	ytitle("Patency Difference (%)") ///
	xtitle("Month Since the Procedure") ///
	title("") ///
	legend(order(1 "Patency Difference (%)" 2 "95% Confidence Interval")) ///
	text(-.4 8 "Favor DCB") ///
	text(.8 8 "Favor PTA") ///
	yline(0, lpattern(dash) lcolor(maroon)) ///
	note("The difference is significant as long as the 95%CI does not include the horizontal dotted line." ///
	" Plot based on estimates from multivariable flexible parametric models for survival analysis") 
graph export FigureS1.png, replace



///////////////////////////////////////////////////////////////////////////////
**# END SURVIVAL ANALYSIS
/////////////////////////////////////////////////////////////////////////////

log close 
cap translate "C:\Documenti\Morganti\analysis_afv_`c(current_date)'.smcl" "C:\Documenti\Morganti\analysis_afv_`c(current_date)'.pdf", replace
