// File to construct liquid wealth from 2004 SCF dataset 

clear 
cd C:/GitHub/SCF-IncWealthDist/data/2004
//use rscfp2004, replace 

// Check the liq variable:
//use yy1 y1 checking saving mma call liq using rscfp2004, replace 
//gen liqTest = checking + saving + mma + call  // = liq 
//keep yy1 y1 liq liqTest 

// Check the nmmf variable: 
//use yy1 y1 stmutf tfbmutf gbmutf obmutf comutf omutf nmmf using rscfp2004, replace 
//gen mutfTest = stmutf + tfbmutf + gbmutf + obmutf + comutf + omutf // = nmmf 
//keep yy1 y1 nmmf mutfTest

// Save file with answer to how often total balance is paid: 
//use Y1 X432 using p04i6, replace 
//rename Y1 y1
//rename X432 x432
//save ccbal_answer, replace

// Load data: 
use yy1 y1 wgt age educ edcl norminc liq cds nmmf stocks bond ccbal install veh_inst using rscfp2004, replace  

merge 1:1 y1 using ccbal_answer
replace ccbal = 0 if x432 == 1
drop x432 _merge 

// Sample selection: 
bysort yy1: egen meanAge = mean(age)
drop age 
rename meanAge age 
drop if age < 25 | 62 < age 
drop if norminc < 0 

// Generate liquid wealth: 
gen tempLiqWealthInst = liq*1.05 + cds + nmmf + stocks + bond - ccbal - (install-veh_inst)
gen tempLiqWealthKaplan = liq*1.05 + cds + nmmf + stocks + bond - ccbal
// Cash adjustment: see Kaplan et al. (2014, Eca), appendix B1
drop liq cds nmmf stocks bond ccbal install veh_inst

// Generate education classifications:
gen myEd = 1*(edcl==1) + 2*(edcl==2 | edcl ==3) + 3*(edcl==4)
// 1=no high school, 2=high school/some college, 3=college 
gen myEdText = "No high school" if myEd == 1
replace myEdText = "High school/some college" if myEd == 2
replace myEdText = "College" if myEd == 3
drop educ edcl 

bysort yy1: egen liqWealthInst = mean(tempLiqWealthInst)
bysort yy1: egen liqWealthKaplan = mean(tempLiqWealthKaplan)
bysort yy1: egen permInc = mean(norminc)
bysort yy1: egen weight = mean(wgt*5)
drop tempLiqWealthInst tempLiqWealthKaplan norminc wgt
keep if mod(y1,5)==1 
drop y1

// More sample selection - lowest percentile of permInc 
egen totalWeight = sum(weight)
gen normweight = weight/totalWeight
sort permInc 
gen sumW = sum(normweight)
drop if sumW < 0.05
drop totalWeight normweight sumW 

//////////////////////////////////// 
// Before proceeding, decide which liquid wealth measure to use: 
scalar includeInstallmentDebt = 0
if includeInstallmentDebt == 1 {
	gen liqWealth = liqWealthInst	
} 
else {	
	gen liqWealth = liqWealthKaplan
}
drop if liqWealth < 0

egen totalWeight = total(weight)  // need to redo this after dropping stuff
gen normweight = weight/totalWeight
format totalWeight normweight %12.0g
bysort myEd: egen edfrac = total(normweight)
gen edWeight = normweight/edfrac

/*
// Deal with outliers:
bysort myEd (liqWealth): gen sumW = sum(edWeight)*100
bysort myEd (liqWealth): drop if sumW > 99.5
drop totalWeight normweight edfrac edWeight sumW

egen totalWeight = sum(weight)  // need to redo this after dropping stuff
gen normweight = weight/totalWeight
format totalWeight normweight %12.0g
bysort myEd: egen edfrac = sum(normweight)
gen edWeight = normweight/edfrac
*/
sort liqWealth yy1
gen sumNormW = sum(normweight)*100
egen totLiqWealth = total(normweight*liqWealth)  // egen -> sum gives total sum, use total instead	
bysort myEd: egen totLiqWealth_ed = total(normweight*liqWealth)
bysort myEd: gen fracLiqWealth_ed = totLiqWealth_ed/totLiqWealth 


// ******************************************************
// Only report any stats after dealing with outliers:
bysort myEd: su edfrac

// Weighted average of liquid wealth/PI by education:
gen indLWoPI = liqWealth/permInc 
bysort myEd: egen avIndLWoPI = total(edWeight*indLWoPI)
bysort myEd: su avIndLWoPI

// Total liquid wealth / total PI by education:
bysort myEd: egen totPI_ed = total(normweight*permInc)
bysort myEd: gen LWoPI_ed = totLiqWealth_ed/totPI_ed 
bysort myEd: su LWoPI_ed 

// Median liquid wealth/PI by education: 
bysort myEd: egen medIndLWoPI = median(indLWoPI)
bysort myEd: su medIndLWoPI

// Weighted median liquid wealth/PI by education: 
gen wtMedLWoPI = . 
quietly forval ii=1/3 {
    su indLWoPI [w=edWeight] if myEd==`ii', detail 
	replace wtMedLWoPI = r(p50) if myEd == `ii'
}
bysort myEd: su wtMedLWoPI
gen wtMedLWoPIann = wtMedLWoPI * 4 
bysort myEd: su wtMedLWoPIann

// Weighted median take 2: 
bysort myEd (indLWoPI yy1): gen temp_sumEdW = sum(edWeight)*100
drop temp_sumEdW

// Weighted median liquid wealth/PI by education and age=62:
gen wtMedLWoPIa62 = . 
quietly forval ii=1/3 {
    su indLWoPI [w=edWeight] if myEd==`ii' & age==62, detail 
	replace wtMedLWoPIa62 = r(p50) if myEd == `ii' & age==62
}
bysort myEd: su wtMedLWoPIa62
gen wtMedLWoPIa62ann = wtMedLWoPIa62 * 4
bysort myEd: su wtMedLWoPIa62ann


// Initial permanent incomes? 
gen permIncQ = permInc/4
gen logPermIncQ = log(permIncQ)
bysort myEd: su permIncQ [w=edWeight] if age==25
bysort myEd: su logPermIncQ [w=edWeight] if age==25
su logPermIncQ [w=normweight] if age==25
/*
grstyle init 
grstyle set grid
//grstyle set mesh
if includeInstallmentDebt == 1 {
}
else {
	// NOTE: histogram is unweighted... 
	twoway histogram logPermIncQ, sort by(myEdText, title("Log-initial PI") note("")) name(logInitPI, replace) ///
	xtitle("Log(initial PI)") ytitle("Frequency") /// 
	ylabel(#5) xlabel(#5)
}
*/

/*
// Total liquid wealth / Total permanent income by education: 
bysort myEd: egen totLW = sum(weight*liqWealth)
bysort myEd: egen totPI = sum(weight*permInc)
gen LWoPI = totLW/totPI 
format totLW totPI LWoPI %15.0g
*/
sort liqWealth yy1 

// Lorenz curve for all households
gen weightedLW_all = normweight*liqWealth
egen totWeightedLW_all = total(weightedLW_all)
gen sumLWall = sum(weightedLW_all/totWeightedLW_all)*100

grstyle init
grstyle set grid
if includeInstallmentDebt == 1 {
	twoway line sumLWall sumNormW, title("Lorenz curves (inst.)") note("") name(LorenzAll, replace) ///
	xtitle("Cumulative fraction of population") ytitle("Cumulative fraction of liquid wealth") /// 
	ylabel(#5) xlabel(#5)
}
else {
	twoway line sumLWall sumNormW, title("Lorenz curves") note("") name(LorenzAll, replace) ///
	xtitle("Cumulative fraction of population") ytitle("Cumulative fraction of liquid wealth") /// 
	ylabel(#5) xlabel(#5)
}
preserve 
keep if sumNormW < 80
su sumLW
keep if sumNormW < 60
su sumLW 
keep if sumNormW < 40 
su sumLW 
keep if sumNormW < 20
su sumLW 
restore 

// Save to .csv for easier plotting in Excel 
outsheet yy1 myEd sumNormW sumLWall using LorenzAll.csv, replace  


// Lorenz curves for each education group
bysort myEd: gen weightedLW = edWeight*liqWealth
bysort myEd: egen totWeightedLW = total(weightedLW)
bysort myEd (liqWealth yy1): gen sumLW = sum(weightedLW/totWeightedLW)*100
bysort myEd (liqWealth yy1): gen sumEdW = sum(edWeight)*100  

grstyle init 
grstyle set grid
//grstyle set mesh
if includeInstallmentDebt == 1 {
	twoway line sumLW sumEdW, sort by(myEdText, title("Lorenz curves (inst.)") note("")) name(LorCombined, replace) ///
	xtitle("Cumulative fraction of population") ytitle("Cumulative fraction of liquid wealth") /// 
	ylabel(#5) xlabel(#5)
}
else {
	twoway line sumLW sumEdW, sort by(myEdText, title("Lorenz curves") note("")) name(LorCombined, replace) ///
	xtitle("Cumulative fraction of population") ytitle("Cumulative fraction of liquid wealth") /// 
	ylabel(#5) xlabel(#5)
}
forvalues xx = 1/3 {
	preserve 
	keep if myEd == `xx'
	display myEdText
	keep if sumEdW < 80
	su sumLW
	keep if sumEdW < 60
	su sumLW 
	keep if sumEdW < 40 
	su sumLW 
	keep if sumEdW < 20
	su sumLW 
	restore 
}

// Save to .csv for easier plotting in Excel 
outsheet yy1 myEd sumEdW sumLW using LorenzEd.csv, replace 


////////////////////////
/*
preserve 
keep if myEd==1
twoway line sumLW sumW, name(Lor1,replace) title("Lorenz curve, no high school") /// 
xtitle("Cumulative fraction of population") ytitle("Cumulative fraction of liquid wealth")
//graph display Lor1, ysize(6) xsize(10) scale(1.4)
restore

preserve
keep if myEd==2
twoway line sumLW sumW, name(Lor2,replace) title("Lorenz curve, high school/some college") ///
xtitle("Cumulative fraction of population") ytitle("Cumulative fraction of liquid wealth")
//graph display Lor2, ysize(6) xsize(10) scale(1.4)
restore

preserve
keep if myEd==3
twoway line sumLW sumW, name(Lor3,replace) title("Lorenz curve, college") ///
xtitle("Cumulative fraction of population") ytitle("Cumulative fraction of liquid wealth")
graph display Lor3, ysize(6) xsize(10) scale(1.4)
restore


bysort yy1: egen meanWgt = mean(5*wgt/sumwgt)
drop sumwgt


gen weightedLW = meanLW * meanWgt
gen weightedPI = meanPI * meanWgt
gen LWoPI = weightedLW/weightedPI

gen altLWoPI = meanLW/meanPI

bysort myEd: egen meanLWoPI = mean(LWoPI)
bysort myEd: egen meanAltLWoPI = mean(meanWgt*altLWoPI)


sort meanLW



lorenz estimate liqWealth, over(myEd)
lorenz graph, aspectratio(1) xlabel(,grid)

//lorenz graph, aspectratio(1) xlabel(,grid) overlay
*/