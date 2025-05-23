program def tomode
*! 1.2.2 NJC & FW 8 December 1998
        version 5.0
        local varlist "max(1)"
        local if "opt"
        local in "opt"
        local options "Generate(str) REPLACE USEmiss Ochmiss BY(string)"
        local options "`options' Minmode UNIque"
        parse "`*'"

        if "`generat'" != "" {
                if "`replace'" == "replace" {
                        di in r "generate( ) may not be combined with replace"
                        exit 198
                }
                confirm new variable `generat'
        }
        else {
                if "`replace'" == "" {
                        di in r "need generate( ) or replace option"
                        exit 198
                }
        }

        if "`minmode'" != "" & "`unique'" != "" {
                di in r "minmode may not be combined with unique"
                exit 198
        }

        if "`by'" != "" {
                unabbrev `by'
                local by "$S_1"
        }

        tempvar touse toch freq fmode uniq
        mark `touse' `if' `in'
        gen byte `toch' = 1
        sort `touse' `by' `varlist'
        qui by `touse' `by' `varlist' : gen `freq' = _N

        local type : type `varlist'

        qui if "`ochmiss'" != "" {
                if substr("`type'",1,3) == "str" {
                        replace `toch' = `varlist' == ""
                }
                else replace `toch' = `varlist' == .
        }

        qui if "`usemiss'" == "" {
                if substr("`type'",1,3) == "str" {
                        replace `freq' = 0 if `varlist' == ""
                }
                else replace `freq' = 0 if `varlist' == .
        }

        if "`minmode'" == "" { sort `touse' `by' `freq' `varlist' }
        else gsort `touse' `by' `freq' - `varlist'

        gen byte `uniq' = 1
        qui if "`unique'" != "" {
                by `touse' `by' `freq' : gen `fmode' = _N
                by `touse' `by' : replace `uniq' = `freq'[_N] == `fmode'[_N]
        }

        if "`replace'" != "" { /* replace old */
                tempvar copy
                qui gen `type' `copy' = `varlist'
                local generat "`varlist'"
        }
        else { /* generate new */
                qui gen `type' `generat' = `varlist'
                local copy "`varlist'"
        }

        qui by `touse' `by' : replace `generat' = `varlist'[_N] /*
         */ if `varlist' != `varlist'[_N] & `toch' & `touse' & `uniq'

        qui count if `copy' != `generat'
        local nch = _result(1)
        if "`replace'" != "" { di in bl "(`nch' real changes made)" }
end
