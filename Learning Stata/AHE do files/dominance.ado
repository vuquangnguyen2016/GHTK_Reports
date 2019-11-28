*! version 1.0 	Owen O'Donnell 		9 June 2006

* Syntax: varlist [if] [in] [fweight aweight] [using], SOrtvar(varname) [Points(integer 19) Level(integer 5) Rule(string) SHares(string) LABels(string)]


* Tests concentration curve dominance
* Can use to test:
*			- dominance of concentration curve against 45 degree line and Lorenz curve	( 1 variable in varlist and using not specified )
*			- dominance between two dependent concentration curves				( 2 variables in varlist and using not specified )
*			- dominance between two independent concentration curves				( 1 variable in varlist and using specified )

* Based on variance-covariance matrix for dependent Lorenz/concentration curves derived by:
*		Bishop, Chow and Formby (1994) International Economic Review
*		Davidson & Duclos (1997) Econometrica

/* Options:
		sortvar() - specifies the living standards vbl. used for Lorenz curve and to produce rank for concentration curve. Required.
		Points(#) - specifies number of quantiles at which curves' ordinates are compared. Maximum=20. Minimum=3.
		Level(#) - level of significance for tests. Only 1(%) or 5(%).
		Rule() - specifies the decision rule to be used. mca = multiple comparison approach, iup = intersection union principle, both = mca & iup
		Shares() - requests cummulative shares at quintiles or deciles with tests of difference from population and income shares. 
					quintiles or deciles allowed. For use only with 1 vbl. in varlist and using not specified.
		Labels() - give labels for concentration curves. For use with using option only.


 		Weights  - 	are applied in obtaining point estimates only.
 				There is no allowance for the impact of weights and other sample design characteristics on the 
 				sampling variability of the estimates of the Lorenz/concentration curve ordinates */


* Program calls the ado files glcurve and lpoly. Download from Stata website if not installed. 

*************************************************************************************************************************

capture program drop dominance
program define dominance

	version 9.2

	syntax varlist [if] [in] [fweight aweight] [using/], SOrtvar(varname) [Points(integer 19) Level(integer 5) Rule(string) SHares(string) LABels(string)]

	tokenize `varlist'
	local first `1'
	local second `2'

	if "`second'"=="" & "`using'"=="" {
		if "`labels'"!="" {
			di as error "Labels option not allowed if using not specified"
			exit
		}
		else {
			domdep1 `varlist' `if' `in' [`weight' `exp'], sortvar(`sortvar') points(`points') level(`level') rule(`rule') shares(`shares')
		}
	}
	else if "`second'"!="" & "`using'"=="" {
		if "`labels'"!="" {
			di as error "Labels option not allowed if using not specified"
			exit
		}
		else if "`shares'"!="" {
			di as error "Shares option not allowed with more than one variable in varlist"
			exit
		}
		else {
			domdep2 `varlist' `if' `in' [`weight' `exp'], sortvar(`sortvar') points(`points') level(`level') rule(`rule') 
		}
	}
	else if "`second'"=="" & "`using'"!="" {
		if "`shares'"!="" {
			di as error "Shares option not allowed if using is specified"
			exit
		}
		else {
			domindep `varlist' `if' `in' [`weight' `exp'] using `using', sortvar(`sortvar') points(`points') level(`level') ///
					rule(`rule') labels(`labels')
		}
	}
	else if "`second'"!="" & "`using'"!="" {
		di as error "Using option not allowed with more than one variable in varlist"
		exit
	}
end

***************************************************************************************************************************************


******************* PROGRAM TO TEST DOMINANCE OF CONCENTRATION CURVE AGAINST 45 DEGREE LINE AND LORENZ CURVE **************************

capture program drop domdep1
program define domdep1

	version 9.2

	syntax varname [if] [in] [fweight aweight], SOrtvar(varname) [Points(integer 19) Level(integer 5) Rule(string) SHares(string)]

	marksample touse
	markout `touse' `sortvar'

	tempvar wi wr GLy CCx cumeany cumeanx y2 x2 yx GLy2 cumeany2 CCx2 cumeanx2 cumvary cumvarx CCyx cumeanyx quantiles cndmnx rule_mca rule_iup
	tempname byname

	quietly {

		if "`weight'" == "" {
			gen byte `wi' = 1
		}
		else {
			gen `wi' `exp'
		}
		
		sum `sortvar' [`weight' `exp'] if `touse'
		local meany=r(mean)
		local vary=r(Var)
		local maxy=r(max)
		if `vary'==0 {
			di as error "no variance in `sortvar'"
			exit
		}
		sum `varlist' [`weight' `exp'] if `touse'
		local meanx=r(mean)
		local varx=r(Var)
		if `varx'==0 {
			di as error "no variance in `varname'"
			exit
		}
		corr `sortvar' `varlist' [`weight' `exp'] if `touse', c
		local covyx=r(cov_12)
		local N=r(N)

		glcurve `sortvar' [`weight' `exp'] if `touse', pvar(`wr') glvar(`GLy') nograph				/* GLorenz and concentration curve ordinates */
		glcurve `varlist' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCx') nograph		

		gen double `cumeany'=`GLy'/`wr' if `touse'						/* Cummulative (weighted) means of income and tax/benefit */
		gen double `cumeanx'=`CCx'/`wr' if `touse'

		gen double `y2'=`sortvar'^2 if `touse'
		gen double `x2'=`varlist'^2 if `touse'
		gen double `yx'=`sortvar'*`varlist' if `touse'
		glcurve `y2' [`weight' `exp'] if `touse', glvar(`GLy2') nograph
		gen double `cumeany2'=`GLy2'/`wr'
		glcurve `x2' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCx2') nograph
		gen double `cumeanx2'=`CCx2'/`wr' if `touse'
		gen double `cumvary'=`cumeany2'-`cumeany'^2 if `touse'				/* cummulative (weighted) variances of income and tax/benefit */
		gen double `cumvarx'=`cumeanx2'-`cumeanx'^2 if `touse'
  		glcurve `yx' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCyx') nograph
		gen double `cumeanyx'=`CCyx'/`wr' if `touse'
		
	

		local q1=`points'+1
		_pctile `sortvar' [pw=`wi'] if `touse', nq(`q1')				/* quantile values of income */
		gen `quantiles'=.
		forvalues i=1/`points' {
			local yq`i'=r(r`i')
			replace `quantiles'=`yq`i'' in `i' if `touse'
		}

												
		lpoly `varlist' `sortvar' if `touse', gen(`cndmnx') at(`quantiles') nograph 		/* kernel regression estimates of conditional means of x at quantiles of y */ 

		forvalues i=1/`points' {						
			local cndmnx`i'=`cndmnx' in `i' 
		}

		local vars "cumeany cumeanx cumvary cumvarx cumeanyx"
		sort `sortvar', stable
		local p=1/`q1'
		forvalues i=1/`points' {
			preserve
			drop if `sortvar'>`yq`i'' 
			foreach var of local vars {
				local `var'`i'=``var'' in l 											/* cummulative means and variances at each quantile point */
			}

			#delimit ;

			local p`i'=`i'*`p';
			local varGL`i'=`p`i''*(`cumvary`i''+(1-`p`i'')*(`yq`i''-`cumeany`i'')^2)	;			/* variance of the GLorenz ordinate */
			local varCC`i'=`p`i''*(`cumvarx`i''+(1-`p`i'')*(`cndmnx`i''-`cumeanx`i'')^2)	;			/* variance of the Concentration Curve ordinate */
			local covGLCC`i'=`p`i''*(`cumeanyx`i''-`cumeany`i''*`cumeanx`i''+(1-`p`i'')*
					(`yq`i''-`cumeany`i'')*(`cndmnx`i''-`cumeanx`i''))	;					/* covariance of GL and CC ordinates */
			local covmyGL`i'=`p`i''*(`cumvary`i''+(`yq`i''-`cumeany`i'')*(`meany'-`cumeany`i''));		/* covariance of GL ordinate with mean y */
			local covmxCC`i'=`p`i''*(`cumvarx`i''+(`cndmnx`i''-`cumeanx`i'')*(`meanx'-`cumeanx`i''));		/* covariance of CC ordinate with mean x */
			local covmyCC`i'=`p`i''*(`cumeanyx`i''-`cumeany`i''*`cumeanx`i''+
						(`cndmnx`i''-`cumeanx`i'')*(`meany'-`cumeany`i'')) ;					/* covariance of CC ordinate with mean y */
			local covmxGL`i'=`p`i''*(`cumeanyx`i''-`cumeany`i''*`cumeanx`i''+
						(`yq`i''-`cumeany`i'')*(`meanx'-`cumeanx`i''))	;					/* covariance of GL ordinate with mean x */
			local k3`i'=2/(`meany'*`meanx') ;
			local k4`i'=2*(`p`i''*`cumeany`i'')/`meany'^3 ;
			local k5`i'=2*(`p`i''*`cumeanx`i'')/`meanx'^3 ;
			local k6`i'=2*(`p`i''*`cumeany`i'')/((`meany'^2)*(`meanx')) ;
			local k7`i'=2*(`p`i''*`cumeanx`i'')/((`meany')*(`meanx'^2)) ;
			local k8`i'=2*((`p`i''*`cumeany`i'')/`meany'^2)*((`p`i''*`cumeanx`i'')/`meanx'^2) ;
			local k9`i'=((`p`i''*`cumeany`i'')/`meany'^2)^2 ;
			local k10`i'=((`p`i''*`cumeanx`i'')/`meanx'^2)^2 ;
		
			local se`i'=(((`varGL`i''/`meany'^2)+(`varCC`i''/`meanx'^2)-(`k3`i''*`covGLCC`i'')				/* standard error of difference b/w GL and CC ordinates */
					-(`k4`i''*`covmyGL`i'')-(`k5`i''*`covmxCC`i'')+(`k6`i''*`covmyCC`i'') 
					+(`k7`i''*`covmxGL`i'')-(`k8`i''*`covyx')+(`k9`i''*`vary')+(`k10`i''*`varx'))/`N')^0.5 ;
		
			local seCC`i'=(((`varCC`i''/`meanx'^2)-(`k5`i''*`covmxCC`i'')+(`k10`i''*`varx'))/`N')^0.5 ;		/* standard error of CC ordinate */
			local seGL`i'=(((`varGL`i''/`meany'^2)-(`k4`i''*`covmyGL`i'')+(`k9`i''*`vary'))/`N')^0.5 ;		/* standard error of GL ordinate */
			#delimit cr
			if (`se`i''==.|`seCC`i''==.|`seGL`i''==.) {
				di as error "standard error evaluated to missing"
				exit
			}

			local s=`level'/200

			local z`i'=`p`i''*((`cumeany`i''/`meany')-(`cumeanx`i''/`meanx'))/`se`i''			/* test statistic for difference b/w CC and GL at each quantile */
			local pos`i'=(`z`i'' > invttail(`N'-1,`s'))								/* index of signif. diff. at each quantile */
			local neg`i'=(-1*`z`i''> invttail(`N'-1,`s'))
				
			local zCC`i'=`p`i''*((`cumeanx`i''/`meanx') - 1)/`seCC`i''						/* test statistic for difference of CC from 45 degree line at each quantile */
			local posCC`i'=(`zCC`i'' > invttail(`N'-1,`s'))								
			local negCC`i'=(-1*`zCC`i''> invttail(`N'-1,`s'))

			local zGL`i'=`p`i''*((`cumeany`i''/`meany') - 1)/`seGL`i''						/* test statistic for difference of CC from 45 degree line at each quantile */
			local posGL`i'=(`zGL`i'' > invttail(`N'-1,`s'))								
			local negGL`i'=(-1*`zGL`i''> invttail(`N'-1,`s'))

			if (`z`i''==.|`zCC`i''==.|`zGL`i''==.) {
				di as error "test statistic evaluated to missing"
				exit
			}
			restore 
		}
	
	
		local index "pos neg posCC negCC posGL negGL"
		local zstat "z zCC zGL"
		tempvar maxz maxzCC maxzGL minz minzCC minzGL
		local i=1
		local j=0
		foreach t of local zstat {
			gen `max`t''=0
			gen `min`t''=0
		}

		while `i'<=`points' {
			if `i'==1 {
				foreach n of local index {
					local sum`n'`i'=``n'`i''
				}
				foreach t of local zstat {
					replace `max`t''=max(0,``t'`i'')
					replace `min`t''=min(0,``t'`i'') 
				}
			}
			else	{
				foreach n of local index {
					local sum`n'`i'=`sum`n'`j''+``n'`i''			/* cummulative sums of indices of sign. diff. across quantiles */
				}
				foreach t of local zstat {
					replace `max`t''=max(`max`t'',``t'`i'')				/* identify maximum positive z statistic */
					replace `min`t''=min(`min`t'',``t'`i'')				/* identify minimum negative z statistic */
				}
			}
			local ++i
			local ++j
		}
		preserve
		keep in 1
		
		local q2=`points'-2
		if `level'==1 {
			local c=1
		}
		else if `level'==5 {
			local c=2
		}

		*************************************************************************************************************************
		matrix smm=J(18,2,0)	/* critical values from Studentized Maximum Modulus Table */
		mat smm[1,1]=2.934	/* rows are specific to the number of means being compared ranging from 3 (row 1) to 20 (row 18) */
		mat smm[2,1]=3.143	/* 1st and 2nd columns are for 1% and 5% significance respectively */
		mat smm[3,1]=3.289	/* degrees of freedom are infinity i.e. for use with large samples */
		mat smm[4,1]=3.402	/* Source: Stoline and Ury (1979) Technometrics 21(1): 87-93 */
		mat smm[5,1]=3.493
		mat smm[6,1]=3.569
		mat smm[7,1]=3.634
		mat smm[8,1]=3.691
		mat smm[9,1]=3.742
		mat smm[10,1]=3.787
		mat smm[11,1]=3.829
		mat smm[12,1]=3.867
		mat smm[13,1]=3.901
		mat smm[14,1]=3.934
		mat smm[15,1]=3.963
		mat smm[16,1]=3.991
		mat smm[17,1]=4.018
		mat smm[18,1]=4.043
		mat smm[1,2]=2.388
		mat smm[2,2]=2.631
		mat smm[3,2]=2.800
		mat smm[4,2]=2.928
		mat smm[5,2]=3.031
		mat smm[6,2]=3.117
		mat smm[7,2]=3.190
		mat smm[8,2]=3.254
		mat smm[9,2]=3.310
		mat smm[10,2]=3.361
		mat smm[11,2]=3.407
		mat smm[12,2]=3.449
		mat smm[13,2]=3.487
		mat smm[14,2]=3.523
		mat smm[15,2]=3.556
		mat smm[16,2]=3.587
		mat smm[17,2]=3.615
		mat smm[18,2]=3.643
		*************************************************************************************************************************

		gen `rule_mca'=4
		if `maxz'!=0 & `minz'==0 {
			replace `rule_mca'=1 if `maxz'>smm[`q2',`c'] 
		}
		else if `maxz'!=0 & `minz'!=0 {
			replace `rule_mca'=1 if `maxz'>smm[`q2',`c'] & (-1*`minz')<=smm[`q2',`c']
			replace `rule_mca'=2 if `maxz'<=smm[`q2',`c'] & (-1*`minz')>smm[`q2',`c']
			replace `rule_mca'=3 if `maxz'>smm[`q2',`c'] & (-1*`minz')>smm[`q2',`c']
		}		
		else if `maxz'==0 & `minz'!=0 {
			replace `rule_mca'=2 if (-1*`minz')>smm[`q2',`c']
		}

		label define codes 1 "Lorenz dominates" 2 "Concentration curve dominates" 3 "curves cross" 4 "non-dominance"
		label values `rule_mca' codes 

		gen `rule_iup'=4
		replace `rule_iup'=1 if `sumpos`points''==`points'
		replace `rule_iup'=2 if `sumneg`points''==`points'
		replace `rule_iup'=3 if `sumpos`points''>0 & `sumneg`points''>0
		label values `rule_iup' codes 
	
		n di in gr "Test of dominance between concentration curve and Lorenz curve"
		n di in gr _n "Variable" _col(12) "Sort vbl." _col(25) "Sign. level" _col(40) "# points" _col(55) "Rule" _n _dup(60) "-"  

		if "`rule'"=="" | "`rule'"=="mca" {
			n di in gr _n "`varlist'" _col(12) "`sortvar'" _col(30) "`level'%" _col(43) "`points'" _col(55) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean
 		}
		else if "`rule'"=="iup" {
			no di in gr _n "`varlist'" _col(12) "`sortvar'" _col(30) "`level'%" _col(43) "`points'" _col(55) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean	
		}
		else if "`rule'"=="both" {
			no di in gr _n "`varlist'" _col(12) "`sortvar'" _col(30) "`level'%" _col(43) "`points'" _col(55) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean	
			no di in gr _n _n "`varlist'" _col(12) "`sortvar'" _col(30) "`level'%" _col(43) "`points'" _col(55) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean	
		}

		 

		replace `rule_mca'=4
		if `maxzCC'!=0 & `minzCC'==0 {
			replace `rule_mca'=1 if `maxzCC'>smm[`q2',`c'] 
		}
		else if `maxzCC'!=0 & `minzCC'!=0 {
			replace `rule_mca'=1 if `maxzCC'>smm[`q2',`c'] & (-1*`minzCC')<=smm[`q2',`c']
			replace `rule_mca'=2 if `maxzCC'<=smm[`q2',`c'] & (-1*`minzCC')>smm[`q2',`c']
			replace `rule_mca'=3 if `maxzCC'>smm[`q2',`c'] & (-1*`minzCC')>smm[`q2',`c']
		}		
		else if `maxzCC'==0 & `minzCC'!=0 {
			replace `rule_mca'=2 if (-1*`minzCC')>smm[`q2',`c']
		}
		label define codes 1 "Concentration curve dominates" 2 "45 degree dominates" 3 "curve crosses 45 degree" 4 "non-dominance", modify
		label values `rule_mca' codes 

		replace `rule_iup'=4
		replace `rule_iup'=1 if `sumposCC`points''==`points'
		replace `rule_iup'=2 if `sumnegCC`points''==`points'
		replace `rule_iup'=3 if `sumposCC`points''>0 & `sumnegCC`points''>0
		label values `rule_iup' codes 
	
		n di _n _n "Test of dominance between concentration curve and 45 degree line"
		n di in gr _n "Variable" _col(12) "Sign. level" _col(27) "# points" _col(39) "Rule" _n _dup(45) "-"  
	
		if "`rule'"=="" | "`rule'"=="mca" {
			no di in gr _n "`varlist'" _col(17) "`level'%" _col(30) "`points'" _col(40) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean
		 }
		else if "`rule'"=="iup" {
			no di in gr _n "`varlist'" _col(17) "`level'%" _col(30) "`points'" _col(40) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean
		}
		else if "`rule'"=="both" {
			no di in gr _n "`varlist'" _col(17) "`level'%" _col(30) "`points'" _col(40) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean
			no di in gr _n _n "`varlist'" _col(17) "`level'%" _col(30) "`points'" _col(40) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean
		}

		
		if "`shares'"=="" {
			local groups ""
		}
		else if "`shares'"=="quintiles" {
			forvalues i=20(20)80 {
				local q`i'=`i'/(`p'*100)
			}
			local groups "q20 q40 q60 q80"
		}
		else if "`shares'"=="deciles" {
			forvalues i=10(10)90 {
				local q`i'=`i'/(`p'*100)
			}
			local groups "q10 q20 q30 q40 q50 q60 q70 q80 q90"
		}
		
		foreach v of local groups {
			local CC`v'=(`p``v'''*(`cumeanx``v'''/`meanx'))*100
			local seCC`v'=`seCC``v'''*100	
			local GL`v'=(`p``v'''*(`cumeany``v'''/`meany'))*100
			local seGL`v'=`seGL``v'''*100
			local CC`v'p=2*ttail(`N'-1,abs(`zCC``v'''))
			local GL`v'p=2*ttail(`N'-1,abs(`zGL``v'''))
			local CCGL`v'p=2*ttail(`N'-1,abs(`z``v'''))
		}
		
		if "`shares'"=="" {
			di ""
		}
		else if "`shares'"=="quintiles" | "`shares'"=="deciles" {
			n di in gr _n _n "Cummulative shares of `sortvar'"
			n di in gr _n "Quantile" _col(12) "Cumm. share" _col(27) "std. error" _col(43) "p-value" _n  _dup(55) "-" _n
			foreach v of local groups { 
				n di as result "`v'" _col(12) %9.4f `GL`v'' "%" _col(27) %9.4f `seGL`v'' _col(41) %9.4f `GL`v'p'  
			}
			n di in gr _dup(55) "-" _n _n "Cummulative shares of `varlist'"
			n di in gr _n "Quantile" _col(12) "Cumm. share" _col(27) "std. error" _col(41) "Diff. from" _col(55) "Diff. from"  
			n di in gr _col(41) "pop. share" _col(55) "income share"
			n di in gr _n _col(41) "p-value" _col(55) "p-value"  _n _dup(65) "-" _n
			foreach v of local groups { 
				n di as result "`v'" _col(12) %9.4f `CC`v'' "%" _col(27) %9.4f `seCC`v''  _col(41) %9.4f `CC`v'p' _col(55) %9.4f `CCGL`v'p' 
			}
			n di in gr _dup(65) "-"
		}
		restore
	}
end

*********************************************************************************************************************************************


******************************  PROGRAM TO TEST DOMINANCE BETWEEN TWO DEPENDENT CONCENTRATION CURVES ****************************************

capture program drop domdep2
program define domdep2

	version 9.2

	syntax varlist [if] [in] [fweight aweight], SOrtvar(varname) [Points(integer 19) Level(integer 5) Rule(string)]

	marksample touse
	markout `touse' `sortvar'

	tokenize `varlist'
	local x `1'
	local v `2'

	tempvar wi wr CCx CCv cumeanv cumeanx v2 x2 xv CCv2 cumeanv2 CCx2 cumeanx2 cumvarv cumvarx CCxv cumeanxv quantiles cndmnx cndmnv rule_mca rule_iup

	quietly {

		if "`weight'" == "" {
			gen byte `wi' = 1
		}
		else {
			gen `wi' `exp'
		}
		
		sum `x' [`weight' `exp'] if `touse'
		local meanx=r(mean)
		local varx=r(Var)
		if `varx'==0 {
			di as error "no variance in `x'"
			exit
		}
		sum `v' [`weight' `exp'] if `touse'
		local meanv=r(mean)
		local varv=r(Var)
		if `varv'==0 {
			di as error "no variance in `v'"
			exit
		}
		corr `x' `v' [`weight' `exp'] if `touse', c
		local covxv=r(cov_12)
		local N=r(N)
		

		glcurve `x' [`weight' `exp'] if `touse', sortvar(`sortvar') pvar(`wr') glvar(`CCx') nograph		/* Concentration curve ordinates */
		glcurve `v' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCv') nograph		

		gen double `cumeanx'=`CCx'/`wr'								/* Cummulative (weighted) means of tax/benefit variables */
		gen double `cumeanv'=`CCv'/`wr'

		gen double `v2'=`v'^2 if `touse'
		gen double `x2'=`x'^2 if `touse'
		gen double `xv'=`v'*`x' if `touse'
		glcurve `v2' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCv2') nograph
		gen double `cumeanv2'=`CCv2'/`wr'
		glcurve `x2' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCx2') nograph
		gen double `cumeanx2'=`CCx2'/`wr' if `touse'
		gen double `cumvarv'=`cumeanv2'-`cumeanv'^2 if `touse'				/* cummulative (weighted) variances of income and tax/benefit */
		gen double `cumvarx'=`cumeanx2'-`cumeanx'^2 if `touse'
  		glcurve `xv' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCxv') nograph
		gen double `cumeanxv'=`CCxv'/`wr' if `touse'
		


		local q1=`points'+1
		_pctile `sortvar' [pw=`wi'] if `touse', nq(`q1')				/* quantile values of income */
		gen `quantiles'=.
		forvalues i=1/`points' {
			local yq`i'=r(r`i')
			replace `quantiles'=`yq`i'' in `i' if `touse'
		}

												
		lpoly `x' `sortvar' if `touse', gen(`cndmnx') at(`quantiles') nograph 		/* kernel regression estimates of conditional means of x at quantiles of y */ 
		lpoly `v' `sortvar' if `touse', gen(`cndmnv') at(`quantiles') nograph 		 
							
		forvalues i=1/`points' {	
			local cndmnx`i'=`cndmnx' in `i'
			local cndmnv`i'=`cndmnv' in `i'
		}



		local vars "cumeanv cumeanx cumvarv cumvarx cumeanxv"
		sort `sortvar', stable
		local p=1/`q1'
		forvalues i=1/`points' {
			preserve
			drop if `sortvar'>`yq`i'' 
			foreach var of local vars {
				local `var'`i'=``var'' in l 											/* cummulative means and variances at each quantile point */
			}

	
			#delimit ;

			local p`i'=`i'*`p';
			local varCCv`i'=`p`i''*(`cumvarv`i''+(1-`p`i'')*(`cndmnv`i''-`cumeanv`i'')^2)	;			/* variance of the Concentration Curve ordinate */
			local varCCx`i'=`p`i''*(`cumvarx`i''+(1-`p`i'')*(`cndmnx`i''-`cumeanx`i'')^2)	;
			local covCCxv`i'=`p`i''*(`cumeanxv`i''-`cumeanv`i''*`cumeanx`i''+(1-`p`i'')*
						(`cndmnv`i''-`cumeanv`i'')*(`cndmnx`i''-`cumeanx`i''))	;			/* covariance of CC ordinates of x and v */
			local covmxCCx`i'=`p`i''*(`cumvarx`i''+(`cndmnx`i''-`cumeanx`i'')*(`meanx'-`cumeanx`i''));	/* covariance of CC ordinate with mean x */
			local covmvCCv`i'=`p`i''*(`cumvarv`i''+(`cndmnv`i''-`cumeanv`i'')*(`meanv'-`cumeanv`i''));		
			local covmvCCx`i'=`p`i''*(`cumeanxv`i''-`cumeanx`i''*`cumeanv`i''+
						(`cndmnx`i''-`cumeanx`i'')*(`meanv'-`cumeanv`i'')) ;					/* covariance of x CC ordinate with mean v */
			local covmxCCv`i'=`p`i''*(`cumeanxv`i''-`cumeanx`i''*`cumeanv`i''+
						(`cndmnv`i''-`cumeanv`i'')*(`meanx'-`cumeanx`i''))	;		
			local k3`i'=2/(`meanv'*`meanx') ;
			local k4`i'=2*(`p`i''*`cumeanv`i'')/`meanv'^3 ;
			local k5`i'=2*(`p`i''*`cumeanx`i'')/`meanx'^3 ;
			local k6`i'=2*(`p`i''*`cumeanv`i'')/((`meanv'^2)*(`meanx')) ;
			local k7`i'=2*(`p`i''*`cumeanx`i'')/((`meanv')*(`meanx'^2)) ;
			local k8`i'=2*((`p`i''*`cumeanv`i'')/`meanv'^2)*((`p`i''*`cumeanx`i'')/`meanx'^2) ;
			local k9`i'=((`p`i''*`cumeanv`i'')/`meanv'^2)^2 ;
			local k10`i'=((`p`i''*`cumeanx`i'')/`meanx'^2)^2 ;
		
			local se`i'=(((`varCCv`i''/`meanv'^2)+(`varCCx`i''/`meanx'^2)-(`k3`i''*`covCCxv`i'')			/* standard error of difference b/w GL and CC ordinates */
					-(`k4`i''*`covmvCCv`i'')-(`k5`i''*`covmxCCx`i'')+(`k6`i''*`covmvCCx`i'') 
					+(`k7`i''*`covmxCCv`i'')-(`k8`i''*`covxv')+(`k9`i''*`varv')+(`k10`i''*`varx'))/`N')^0.5 ;
			#delimit cr
			if `se`i''==. {
				di as error "standard error evaluated to missing"
				exit
			}

			
			local s=`level'/200

			local z`i'=`p`i''*((`cumeanv`i''/`meanv')-(`cumeanx`i''/`meanx'))/`se`i''		/* test statistic for difference b/w CC curves at each quantile */
			local pos`i'=(`z`i'' > invttail(`N'-1,`s'))							/* index of signif. diff. at each quantile */
			local neg`i'=(-1*`z`i''> invttail(`N'-1,`s'))
			if `z`i''==. {
				di as error "test statistic evaluated to missing"
				exit
			}

			restore 
		}
	
		local index "pos neg"
		local i=1
		local j=0
		tempvar max min
		gen `max'=0
		gen `min'=0


		while `i'<=`points' {
			if `i'==1 {
				foreach n of local index {
					local sum`n'`i'=``n'`i''
				}
				replace `max'=max(0,`z`i'')
				replace `min'=min(0,`z`i'') 
			}
			else	{
				foreach n of local index {
					local sum`n'`i'=`sum`n'`j''+``n'`i''			/* cummulative sums of indices of sign. diff. across quantiles */
				}
				replace `max'=max(`max',`z`i'')					/* identify maximum positive z statistic */
				replace `min'=min(`min',`z`i'')					/* identify minimum negative z statistic */
			}
			local ++i
			local ++j
		}

		preserve
		keep in 1
		
		local q2=`points'-2
		if `level'==1 {
			local c=1
		}
		else if `level'==5 {
			local c=2
		}

		*************************************************************************************************************************
		matrix smm=J(18,2,0)	/* critical values from Studentized Maximum Modulus Table */
		mat smm[1,1]=2.934	/* rows are specific to the number of means being compared ranging from 3 (row 1) to 20 (row 18) */
		mat smm[2,1]=3.143	/* 1st and 2nd columns are for 1% and 5% significance respectively */
		mat smm[3,1]=3.289	/* degrees of freedom are infinity i.e. for use with large samples */
		mat smm[4,1]=3.402	/* Source: Stoline and Ury (1979) Technometrics 21(1): 87-93 */
		mat smm[5,1]=3.493
		mat smm[6,1]=3.569
		mat smm[7,1]=3.634
		mat smm[8,1]=3.691
		mat smm[9,1]=3.742
		mat smm[10,1]=3.787
		mat smm[11,1]=3.829
		mat smm[12,1]=3.867
		mat smm[13,1]=3.901
		mat smm[14,1]=3.934
		mat smm[15,1]=3.963
		mat smm[16,1]=3.991
		mat smm[17,1]=4.018
		mat smm[18,1]=4.043
		mat smm[1,2]=2.388
		mat smm[2,2]=2.631
		mat smm[3,2]=2.800
		mat smm[4,2]=2.928
		mat smm[5,2]=3.031
		mat smm[6,2]=3.117
		mat smm[7,2]=3.190
		mat smm[8,2]=3.254
		mat smm[9,2]=3.310
		mat smm[10,2]=3.361
		mat smm[11,2]=3.407
		mat smm[12,2]=3.449
		mat smm[13,2]=3.487
		mat smm[14,2]=3.523
		mat smm[15,2]=3.556
		mat smm[16,2]=3.587
		mat smm[17,2]=3.615
		mat smm[18,2]=3.643
		*************************************************************************************************************************

		gen `rule_mca'=4
		if `max'!=0 & `min'==0 {
			replace `rule_mca'=1 if `max'>smm[`q2',`c'] 
		}
		else if `max'!=0 & `min'!=0 {
			replace `rule_mca'=1 if `max'>smm[`q2',`c'] & (-1*`min')<=smm[`q2',`c']
			replace `rule_mca'=2 if `max'<=smm[`q2',`c'] & (-1*`min')>smm[`q2',`c']
			replace `rule_mca'=3 if `max'>smm[`q2',`c'] & (-1*`min')>smm[`q2',`c']
		}		
		else if `max'==0 & `min'!=0 {
			replace `rule_mca'=2 if (-1*`min')>smm[`q2',`c']
		}

		label define codes 1 "`v' dominates `x'" 2 "`x' dominates `v'" 3 "curves cross" 4 "non-dominance"
		label values `rule_mca' codes 

		gen `rule_iup'=4
		replace `rule_iup'=1 if `sumpos`points''==`points'
		replace `rule_iup'=2 if `sumneg`points''==`points'
		replace `rule_iup'=3 if `sumpos`points''>0 & `sumneg`points''>0
		label values `rule_iup' codes 
	
		n di in gr "Test of dominance between concentration curves of `x' and `v'"
		n di in gr _n "Vbl. 1" _col(12) "Vbl. 2" _col(25) "Sign. level" _col(40) "# points" _col(55) "Rule" _n _dup(60) "-"  

		if "`rule'"=="" | "`rule'"=="mca" {
			n di in gr _n _col(3) "`x'" _col(15) "`v'" _col(30) "`level'%" _col(43) "`points'" _col(55) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean
 		}
		else if "`rule'"=="iup" {
			no di in gr _n _col(3) "`x'" _col(15) "`v'" _col(30) "`level'%" _col(43) "`points'" _col(55) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean	
		}
		else if "`rule'"=="both" {
			no di in gr _n _col(3) "`x'" _col(15) "`v'" _col(30) "`level'%" _col(43) "`points'" _col(55) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean	
			no di in gr _n _n _col(3) "`x'" _col(15) "`v'" _col(30) "`level'%" _col(43) "`points'" _col(55) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean	
		}

		restore
	} 
end


****************************************************************************************************************************************


*********************************  PROGRAM TO TEST DOMINANCE BETWEEN TWO INDEPENDENT CONCENTRATION CURVES ******************************

capture program drop domindep
program define domindep

	version 9.2

	syntax varlist [if] [in] [fweight aweight] using/, SOrtvar(varname) [Points(integer 19) Level(integer 5) Rule(string) LABels(string)]

	quietly {

		tempfile master
		save `master'

		local datasets "`master' `using'"

		local d 1

		foreach data of local datasets {

			drop _all
			use `data'

			marksample touse
			markout `touse' `sortvar'

			tempvar wi wr CCx cumeanx x2 CCx2 cumeanx2 cumvarx quantiles cndmnx rule_mca rule_iup

			if "`weight'" == "" {
				gen byte `wi' = 1
			}
			else {
				gen `wi' `exp'
			}
		

			sum `varlist' [`weight' `exp'] if `touse'
			local meanx=r(mean)
			local varx=r(Var)
			local N`d'=r(N)
		
			glcurve `varlist' [`weight' `exp'] if `touse', sortvar(`sortvar') pvar(`wr') glvar(`CCx') nograph		/* Concentration curve ordinates */

			gen double `cumeanx'=`CCx'/`wr' if `touse'									/* Cummulative (weighted) means of tax/benefit variables */

			gen double `x2'=`varlist'^2 if `touse'
			glcurve `x2' [`weight' `exp'] if `touse', sortvar(`sortvar') glvar(`CCx2') nograph
			gen double `cumeanx2'=`CCx2'/`wr' if `touse'
			gen double `cumvarx'=`cumeanx2'-`cumeanx'^2  if `touse'
			
			local q1=`points'+1
			_pctile `sortvar' [pw=`wi'] if `touse', nq(`q1')				/* quantile values of income */
			gen `quantiles'=. 
			forvalues i=1/`points' {
				local yq`i'=r(r`i')
				replace `quantiles'=`yq`i'' in `i'
			}
												
			n lpoly `varlist' `sortvar' if `touse', gen(`cndmnx') at(`quantiles') nograph 		/* kernel regression estimates of conditional means of x at quantiles of y */ 

			forvalues i=1/`points' {						
				local cndmnx`i'=`cndmnx' in `i'
			}
			local vars "cumeanx cumvarx"
			sort `sortvar', stable
			local p=1/`q1'
			forvalues i=1/`points' {
				preserve
				drop if `sortvar'>`yq`i'' 
				foreach var of local vars {
					local `var'`i'=``var'' in l				/* cummulative means and variances at each quantile point */
				}

				local p`i'=`i'*`p'
				local varCCx`i'=`p`i''*(`cumvarx`i''+(1-`p`i'')*(`cndmnx`i''-`cumeanx`i'')^2)				
				local covmxCCx`i'=`p`i''*(`cumvarx`i''+(`cndmnx`i''-`cumeanx`i'')*(`meanx'-`cumeanx`i''))	/* covariance of CC ordinate with mean x */
				local k1`i'=2*(`p`i''*`cumeanx`i'')/`meanx'^3 
				local k2`i'=((`p`i''*`cumeanx`i'')/`meanx'^2)^2 
				local ordinate`d'_`i'=`cumeanx`i''/`meanx'
				local var`d'_`i'=((`varCCx`i''/`meanx'^2)-(`k1`i''*`covmxCCx`i'')+(`k2`i''*`varx'))/`N`d''
				if `var`d'_`i''==. {
					di as error "variance of ordinate evaluated to missing"
					exit
				} 
				restore 
			}
			local ++d
		}

		local s=`level'/200
		local N=min(`N1',`N2')
		forvalues i=1/`points' {
			local z`i'=`p`i''*(`ordinate1_`i''-`ordinate2_`i'')/((`var1_`i''+`var2_`i'')^0.5)		/* test statistic for difference b/w CC curves at each quantile */
			local pos`i'=(`z`i'' > invttail(`N'-1,`s'))								/* index of signif. diff. at each quantile */
			local neg`i'=(-1*`z`i''> invttail(`N'-1,`s'))
			if `z`i''==. {
				di as error "test statistic evaluated to missing"
				exit
			}
		}

		local index "pos neg"
		tempvar max min
		local i=1
		local j=0
		gen `max'=0
		gen `min'=0
		
		while `i'<=`points' {
			if `i'==1 {
				foreach n of local index {
					local sum`n'`i'=``n'`i''
				}
				replace `max'=max(0,`z`i'')
				replace `min'=min(0,`z`i'') 
			}
			else	{
				foreach n of local index {
					local sum`n'`i'=`sum`n'`j''+``n'`i''			/* cummulative sums of indices of sign. diff. across quantiles */
				}
				replace `max'=max(`max',`z`i'')					/* identify maximum positive z statistic */
				replace `min'=min(`min',`z`i'')					/* identify minimum negative z statistic */
			}
			local ++i
			local ++j
		}

		keep in 1
	
		local q2=`points'-2
		if `level'==1 {
			local c=1
		}
		else if `level'==5 {
			local c=2
		}


		*************************************************************************************************************************
		matrix smm=J(18,2,0)	/* critical values from Studentized Maximum Modulus Table */
		mat smm[1,1]=2.934	/* rows are specific to the number of means being compared ranging from 3 (row 1) to 20 (row 18) */
		mat smm[2,1]=3.143	/* 1st and 2nd columns are for 1% and 5% significance respectively */
		mat smm[3,1]=3.289	/* degrees of freedom are infinity i.e. for use with large samples */
		mat smm[4,1]=3.402	/* Source: Stoline and Ury (1979) Technometrics 21(1): 87-93 */
		mat smm[5,1]=3.493
		mat smm[6,1]=3.569
		mat smm[7,1]=3.634
		mat smm[8,1]=3.691
		mat smm[9,1]=3.742
		mat smm[10,1]=3.787
		mat smm[11,1]=3.829
		mat smm[12,1]=3.867
		mat smm[13,1]=3.901
		mat smm[14,1]=3.934
		mat smm[15,1]=3.963
		mat smm[16,1]=3.991
		mat smm[17,1]=4.018
		mat smm[18,1]=4.043
		mat smm[1,2]=2.388
		mat smm[2,2]=2.631
		mat smm[3,2]=2.800
		mat smm[4,2]=2.928
		mat smm[5,2]=3.031
		mat smm[6,2]=3.117
		mat smm[7,2]=3.190
		mat smm[8,2]=3.254
		mat smm[9,2]=3.310
		mat smm[10,2]=3.361
		mat smm[11,2]=3.407
		mat smm[12,2]=3.449
		mat smm[13,2]=3.487
		mat smm[14,2]=3.523
		mat smm[15,2]=3.556
		mat smm[16,2]=3.587
		mat smm[17,2]=3.615
		mat smm[18,2]=3.643
		*************************************************************************************************************************

		gen `rule_mca'=4
		if `max'!=0 & `min'==0 {
			replace `rule_mca'=1 if `max'>smm[`q2',`c'] 
		}
		else if `max'!=0 & `min'!=0 {
			replace `rule_mca'=1 if `max'>smm[`q2',`c'] & (-1*`min')<=smm[`q2',`c']
			replace `rule_mca'=2 if `max'<=smm[`q2',`c'] & (-1*`min')>smm[`q2',`c']
			replace `rule_mca'=3 if `max'>smm[`q2',`c'] & (-1*`min')>smm[`q2',`c']
		}		
		else if `max'==0 & `min'!=0 {
			replace `rule_mca'=2 if (-1*`min')>smm[`q2',`c']
		}

		if "`labels'"=="" {
			local label1="data1"
			local label2="data2"
		}
		else {
			tokenize `labels'
			local label1 `1'
			local label2 `2'
		}
		label define codes 1 "`label1' dominates `label2'" 2 "`label2' dominates `label1'" 3 "curves cross" 4 "non-dominance"
		label values `rule_mca' codes 

		gen `rule_iup'=4
		replace `rule_iup'=1 if `sumpos`points''==`points'
		replace `rule_iup'=2 if `sumneg`points''==`points'
		replace `rule_iup'=3 if `sumpos`points''>0 & `sumneg`points''>0
		label values `rule_iup' codes 
	
		n di in gr "Test of dominance between concentration curves of `varlist' for `label1' and `label2'"
		n di in gr _n "Data 1" _col(12) "Data 2" _col(25) "Sign. level" _col(40) "# points" _col(55) "Rule" _n _dup(60) "-"  

		if "`rule'"=="" | "`rule'"=="mca" {
			n di in gr _n "`label1'" _col(12) "`label2'" _col(30) "`level'%" _col(43) "`points'" _col(55) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean
 		}
		else if "`rule'"=="iup" {
			no di in gr _n "`label1'" _col(12) "`label2'" _col(30) "`level'%" _col(43) "`points'" _col(55) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean	
		}
		else if "`rule'"=="both" {
			no di in gr _n "`label1'" _col(12) "`label2'" _col(30) "`level'%" _col(43) "`points'" _col(55) "mca" _n 
			n list `rule_mca' in 1, noheader noobs clean	
			no di in gr _n _n "`label1'" _col(12) "`label2'" _col(30) "`level'%" _col(43) "`points'" _col(55) "iup" _n 
			n list `rule_iup' in 1, noheader noobs clean	
		}

		drop _all
		use `master'
	}
end





