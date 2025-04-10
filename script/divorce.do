/* This file generates the results in 
"Did Unilateral Divorce Laws Raise Divorce Rates?  A Reconciliation and New Results"
by Justin Wolfers
The code has been updated to support comparative work with Modern DID estimators and Augmented Synthentic Control Method. In particulat, some states are removed. 
*/
cd "C:\Users\tiffa\OneDrive\Desktop\DSE4101\script"
pause off
clear
set mem 50m
set matsize 400
use "Divorce-Wolfers-AER.dta" 

*--------------------------------------------------*
* Create output directory
*--------------------------------------------------*
* Create divorce_output directory if it doesn't exist
capture mkdir "divorce_output"

*--------------------------------------------------*
* Identify states to remove based on treatment timing:
*   (1) Always treated: states with min(lfdivlaw) < 1956,
*   (2) Late treated: for states with lfdivlaw < 2000 & lfdivlaw != 9999, if (last_year - treatment_year) < 5,
*   (3) Problematic states: NV, LA, NM IN.
*--------------------------------------------------*

* Filter to years 1956-1988 to match the R code
keep if year >= 1956 & year <= 1988

* Create variables needed for filtering
egen min_lfdivlaw = min(lfdivlaw), by(st)
egen last_year = max(year), by(st)
gen treatment_year = min_lfdivlaw

* Define which states to remove
gen always_treated = (treatment_year < 1956)
gen post_periods = last_year - treatment_year
* Fix for late_treated - only consider states that were actually treated
gen late_treated = (treatment_year < 2000 & treatment_year != 9999 & post_periods < 5)
gen problematic = inlist(st, "NV", "LA", "NM", "IN")
gen remove_state = always_treated | late_treated | problematic

* Display which states are being removed
display "All states being removed:"
levelsof st if remove_state == 1, local(removed_states) clean
display "`removed_states'"

* Save removed states to a file
file open removed_states using "divorce_output/removed_states.txt", write replace
file write removed_states "States excluded from analysis:" _n _n
file write removed_states "Always treated states (treated before 1956):" _n
levelsof st if always_treated == 1, local(always_states) clean
file write removed_states "`always_states'" _n _n
file write removed_states "Late treated states (insufficient post-treatment periods):" _n
levelsof st if late_treated == 1, local(late_states) clean
file write removed_states "`late_states'" _n _n
file write removed_states "Problematic states:" _n
levelsof st if problematic == 1, local(prob_states) clean
file write removed_states "`prob_states'" _n
file close removed_states

* Remove the states that meet removal criteria
drop if remove_state == 1

* Verify removal
tab st


* Table 2
eststo clear

xi i.years_unilateral i.st i.year
reg div_rate _I* if year>1955 & year<1989 [w=stpop]
testparm _Iyear_*
testparm _Ist_*
eststo t2_col1
estadd local trends "No"

xi i.years_unilateral i.st*time i.year
reg div_rate _I* if year>1955 & year<1989 [w=stpop]
testparm _Iyear_*
testparm _Ist_*
testparm _IstXtime_*
eststo t2_col2
estadd local trends "Linear"

xi i.years_unilateral i.st*time i.st*timesq i.year
reg div_rate _I*  if year>1955 & year<1989 [w=stpop]
testparm _Iyear_*
testparm _Ist_*
testparm _IstXtime_*
testparm _IstXtimea*
eststo t2_col3
estadd local trends "Quadratic"

* Rename coefficients for better LaTeX output
foreach est in t2_col1 t2_col2 t2_col3 {
    estadd local posttrend = e(trends)
    forvalues i = 2/9 {
        if `i' == 2 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "1-2 years"
        }
        if `i' == 3 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "3-4 years"
        }
        if `i' == 4 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "5-6 years"
        }
        if `i' == 5 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "7-8 years"
        }
        if `i' == 6 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "9-10 years"
        }
        if `i' == 7 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "11-12 years"
        }
        if `i' == 8 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "13-14 years"
        }
        if `i' == 9 {
            estimates restore `est'
            nlcom _b[_Iyears_uni_`i']
            local label_`i' "15+ years"
        }
    }
}

* Output Table 2 to LaTeX
esttab t2_col1 t2_col2 t2_col3 using "divorce_output/table2.tex", replace ///
    keep(_Iyears_uni_*) ///
    coeflabels(_Iyears_uni_2 "1-2 years" ///
               _Iyears_uni_3 "3-4 years" ///
               _Iyears_uni_4 "5-6 years" ///
               _Iyears_uni_5 "7-8 years" ///
               _Iyears_uni_6 "9-10 years" ///
               _Iyears_uni_7 "11-12 years" ///
               _Iyears_uni_8 "13-14 years" ///
               _Iyears_uni_9 "15+ years") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    booktabs label ///
    mtitles("No state trends" "State trends" "Quadratic state trends") ///
    title("Response of Divorce Rate to Unilateral Divorce Laws") ///
    addnote("All regressions control for state and year fixed effects. Sample: 1956-1988. Weighted by state population.")

* Display Table 2 in Results window
esttab t2_col1 t2_col2 t2_col3, ///
    keep(_Iyears_uni_*) ///
    coeflabels(_Iyears_uni_2 "1-2 years" ///
               _Iyears_uni_3 "3-4 years" ///
               _Iyears_uni_4 "5-6 years" ///
               _Iyears_uni_5 "7-8 years" ///
               _Iyears_uni_6 "9-10 years" ///
               _Iyears_uni_7 "11-12 years" ///
               _Iyears_uni_8 "13-14 years" ///
               _Iyears_uni_9 "15+ years") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("No state trends" "State trends" "Quadratic state trends") ///
    title("Table 2: Response of Divorce Rate to Unilateral Divorce Laws")


* Table 4, Panel A
gen div_mar=div_rate/married_annual /* Note that the NBER working paper version of the paper uses marrnber, which uses census data through to 1990, and CPS data for the 2000 estimates */
la var div_mar "Divorces per thousand married adults" 
summ div_rate div_mar married_annual [w=stpop] if year>1955 & year<1989
summ div_rate div_mar married_annual [w=stpop] if year>1955 & year<1999

eststo clear

xi: reg div_rate i.years_unilateral i.st i.year if year<1989 [w=stpop]
eststo t4a_col1
estadd local sample "1956-1988"
estadd local outcome "Divorce Rate"

xi: reg div_rate i.years_unilateral_long i.st i.year if year<1999 [w=stpop]
eststo t4a_col2
estadd local sample "1956-1998"
estadd local outcome "Divorce Rate"

xi: reg div_mar i.years_unilateral i.st i.year if year<1989 [w=stpop]
eststo t4a_col3
estadd local sample "1956-1988"
estadd local outcome "Div/Married"

xi: reg div_mar i.years_unilateral_long i.st i.year if year<1999 [w=stpop]
eststo t4a_col4
estadd local sample "1956-1998"
estadd local outcome "Div/Married"

* Output Table 4A to LaTeX
esttab t4a_col1 t4a_col2 t4a_col3 t4a_col4 using "divorce_output/table4a.tex", replace ///
    keep(_Iyears_uni_*) ///
    coeflabels(_Iyears_uni_2 "1-2 years" ///
               _Iyears_uni_3 "3-4 years" ///
               _Iyears_uni_4 "5-6 years" ///
               _Iyears_uni_5 "7-8 years" ///
               _Iyears_uni_6 "9-10 years" ///
               _Iyears_uni_7 "11-12 years" ///
               _Iyears_uni_8 "13-14 years" ///
               _Iyears_uni_9 "15+ years" ///
               _Iyears_uni_10 "15-16 years" ///
               _Iyears_uni_11 "17-18 years" ///
               _Iyears_uni_12 "19+ years") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    booktabs label ///
    mtitles("Div Rate 1956-88" "Div Rate 1956-98" "Div/Married 1956-88" "Div/Married 1956-98") ///
    title("Alternative Specifications") ///
    addnote("All regressions control for state and year fixed effects. Weighted by state population.")

* Display Table 4A in Results window
esttab t4a_col1 t4a_col2 t4a_col3 t4a_col4, ///
    keep(_Iyears_uni_*) ///
    coeflabels(_Iyears_uni_2 "1-2 years" ///
               _Iyears_uni_3 "3-4 years" ///
               _Iyears_uni_4 "5-6 years" ///
               _Iyears_uni_5 "7-8 years" ///
               _Iyears_uni_6 "9-10 years" ///
               _Iyears_uni_7 "11-12 years" ///
               _Iyears_uni_8 "13-14 years" ///
               _Iyears_uni_9 "15+ years" ///
               _Iyears_uni_10 "15-16 years" ///
               _Iyears_uni_11 "17-18 years" ///
               _Iyears_uni_12 "19+ years") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Div Rate 1956-88" "Div Rate 1956-98" "Div/Married 1956-88" "Div/Married 1956-98") ///
    title("Table 4, Panel A: Alternative Specifications")


* Table 5
eststo clear

xi: reg div_rate i.years_unilateral_long i.st i.year if year<1999 [w=stpop]
eststo t5_col1
estadd local spec "Baseline"

xi: reg div_rate neighper i.years_unilateral_long i.st i.year if year<1999 [w=stpop]
eststo t5_col2
estadd local spec "Neighbor control"

gen evdiv50_trend=evdiv50*time
xi: reg div_rate evdiv50_trend i.years_unilateral_long i.st i.year if year<1999 [w=stpop]
eststo t5_col3
estadd local spec "Ever-divorced trend"

xi: reg div_rate i.years_unilateral_long i.year*evdiv50 i.st if year<1999 [w=stpop]
eststo t5_col4
estadd local spec "Year×Ever-div"

xi: reg div_rate i.years_unilateral_long i.st i.year if year<1999 & reform==1 [w=stpop]
eststo t5_col5
estadd local spec "Reform states only"

* Output Table 5 to LaTeX
esttab t5_col1 t5_col2 t5_col3 t5_col4 t5_col5 using "divorce_output/table5.tex", replace ///
    keep(_Iyears_uni_* neighper evdiv50_trend) ///
    coeflabels(_Iyears_uni_2 "1-2 years" ///
               _Iyears_uni_3 "3-4 years" ///
               _Iyears_uni_4 "5-6 years" ///
               _Iyears_uni_5 "7-8 years" ///
               _Iyears_uni_6 "9-10 years" ///
               _Iyears_uni_7 "11-12 years" ///
               _Iyears_uni_8 "13-14 years" ///
               _Iyears_uni_9 "15+ years" ///
               _Iyears_uni_10 "15-16 years" ///
               _Iyears_uni_11 "17-18 years" ///
               _Iyears_uni_12 "19+ years" ///
               neighper "Neighbor adoption rate" ///
               evdiv50_trend "Ever-divorced trend") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    booktabs label ///
    mtitles("Baseline" "Neighbor control" "Ever-divorced trend" "Year×Ever-div" "Reform states only") ///
    title("Robustness to Alternative Confounders") ///
    addnote("All regressions control for state and year fixed effects. Sample: 1956-1998. Weighted by state population.")

* Display Table 5 in Results window
esttab t5_col1 t5_col2 t5_col3 t5_col4 t5_col5, ///
    keep(_Iyears_uni_* neighper evdiv50_trend) ///
    coeflabels(_Iyears_uni_2 "1-2 years" ///
               _Iyears_uni_3 "3-4 years" ///
               _Iyears_uni_4 "5-6 years" ///
               _Iyears_uni_5 "7-8 years" ///
               _Iyears_uni_6 "9-10 years" ///
               _Iyears_uni_7 "11-12 years" ///
               _Iyears_uni_8 "13-14 years" ///
               _Iyears_uni_9 "15+ years" ///
               _Iyears_uni_10 "15-16 years" ///
               _Iyears_uni_11 "17-18 years" ///
               _Iyears_uni_12 "19+ years" ///
               neighper "Neighbor adoption rate" ///
               evdiv50_trend "Ever-divorced trend") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Baseline" "Neighbor control" "Ever-divorced trend" "Year×Ever-div" "Reform states only") ///
    title("Table 5: Robustness to Alternative Confounders")