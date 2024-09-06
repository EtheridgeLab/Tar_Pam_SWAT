*Stata code for calculating the percentage of farmers willing to accept contract at give EQIP rates

*The following STATA code was used to conduct the econometric modeling analysis highlighted above. The initial part of the code cleans the data and generates variables needed for the analysis. Then the data is reshaped to analyze the discrete choice experiment results. 
*The mixlogit command is used to provide the coefficients in the analysis discussed above. Furthermore, the “mixlbetas.dta” file is employed to gain individual-specific insights on farmer preferences and WTA values. The WTA values for two contracts are calculated: one with strict nitrogen application limits and one with cover crop requirements. 
*Finally, the code reveals the calculations for predicting the total acreage enrolled in the program. For this calculation, each farmer whose WTA value was less than the EQUIP payment rate was predicted to enroll in the program. These calculations are explained in greater detail in the previous section. 

*** COMPILATION OF ALL STATA FSD DO-FILES (Mahesh Tapas)
*** FSD DO FILE 1 ------------------------------------------------------------
*** FSD DO FILE 1 ------------------------------------------------------------

* Open FSD file
* use "C:\Users\mairm23\OneDrive - East Carolina University\STATA\Farmer Survey Data\Survey of Eastern North Carolina Farmers_online and mail combined raw ready to edit.dta"

* use "C:\Users\mairm23\OneDrive - East Carolina University\STATA\Farmer Survey Data\Matthew's Copy (Farmer Survey Data).dta"
*use "C:\Users\mairm23\OneDrive - East Carolina University\STATA\Farmer Survey Data (Take 1)\Matthew's Copy (Farmer Survey Data).dta" 

*import delimited "D:\All\research\CNH2 project\Farmer Survey\Data\Survey of Eastern North Carolina Farmers_raw data 9_11_2023.csv", varnames(1) encoding(UTF-8) clear

ssc install mixlogit

import excel "U:\Econometrics\1.xlsx", sheet("Survey of Eastern North Carolin") firstrow case(lower) clear

import excel "E:\All\research\CNH2 project\Farmer Survey\Data\Survey of Eastern North Carolina Farmers_raw data 3_29_2024.xlsx", sheet("Survey of Eastern North Carolin") firstrow case(lower) clear

 export delimited using "E:\All\Advising\Tapas\farmer survey raw data.csv", replace
 
import delimited "E:\All\Advising\Tapas\farmer survey raw data.csv", encoding(UTF-8) clear 
* Data Cleaning
*rename q9 q8
*rename ax q9
* Their response for q11_4_text was "veg", which wasn't very specific (Row 188)
*replace q11 = "1" if q11=="1,4"
* Changed "2 cows" to "2" (Row 91)
replace q37b_5_text = "2" if q37b_5_text=="2 cows"
* Converted pounds to tons (Row 31), because they answered in pounds when the question was asked in tons/acre
replace q14_2 = "0.6" if q14_2=="1200 lbs"
* One respondent put "all my life; however, they did not respond to the age question (Row 91)
replace q38 = "" if q38=="All my life"
* We are looking for the highest level of education, so we selected the highest value
*replace q35 = "4" if q35=="2,3,4"
*replace q35 = "3" if q35=="2,3"
* someone responded "2,3,4" (Row 189), so 3 was selected as as the average date range
*replace q27 = "3" if q27=="2,3,4"
* someone responded "2,3,4" (Row 181), so 3 was selected as as the average soil type
*replace q21 = "3" if q21=="2,3,4"
* Changed decimal answers to reflect integer percentages
replace q23_2_text = "5" if q23_2_text==".05"
replace q23_2_text = "10" if q23_2_text==".1"
replace q23_2_text = "32" if q23_2_text==".32"
replace q23_2_text = "15" if q23_2_text==".15"
replace q23_2_text = "40" if q23_2_text==".4"
replace q23_2_text = "20" if q23_2_text==".2"

replace q34="30" if q34==",30"
* got rid of question marks and changed from string to long format
destring q11, replace
destring q14_2, replace
destring q15_2_1, replace
destring q13, replace
destring q21, replace
destring q27, replace
destring q34, replace
destring q35, replace
destring q38, replace
destring q39_3_1, replace
destring q39_3_2, replace
destring q40_2, replace
destring q40_3, replace
destring q40_5, replace
destring q37b_5_text, replace

*** Potential Outliers
* Rows 185 and 192 in q13

*** Labels
label define CertaintyOfPolicyImpact 4 "75%" 3 "50%" 2 "25%" 1 "0%"
label values q32 CertaintyOfPolicyImpact



****Fixing the CE variable names
rename ce_b2s1 ce_b1s2 
rename ce_b2s1cert_nps_group ce_b1s2cert_nps_group 
rename ce_b2s1cert ce_b1s2cert 

rename cn ce_b2s1  
rename co ce_b2s1cert_nps_group  
rename cp ce_b2s1cert  

rename db ce_b4s1cert


gen ce1=.
gen ce2=.
gen cecert1=.
gen cecert2=.

replace ce1=ce_b1s1 if ce1==.
replace ce1=ce_b2s1 if ce1==.
replace ce1=ce_b3s1 if ce1==.
replace ce1=ce_b4s1 if ce1==.
replace ce1=ce_b5s1 if ce1==.

replace ce2=ce_b1s2 if ce2==.
replace ce2=ce_b2s2 if ce2==.
replace ce2=ce_b3s2 if ce2==.
replace ce2=ce_b4s2 if ce2==.
replace ce2=ce_b5s2 if ce2==.

replace cecert1=ce_b1s1cert if cecert1==.
replace cecert1=ce_b2s1cert if cecert1==.
replace cecert1=ce_b3s1cert if cecert1==.
replace cecert1=ce_b4s1cert if cecert1==.
replace cecert1=ce_b5s1cert if cecert1==.

replace cecert2=ce_b1s2cert if cecert2==.
replace cecert2=ce_b2s2cert if cecert2==.
replace cecert2=ce_b3s2cert if cecert2==.
replace cecert2=ce_b4s2cert if cecert2==.
replace cecert2=ce_b5s2cert if cecert2==.

gen id=_n

save "E:\All\research\CNH2 project\Farmer Survey\Data\Farmer Survey Data Github.dta", replace 


*** Reshaping the Data
reshape long ce cecert, i(id) j(set)

tab ce, missing
drop if ce==.

gen choice1=.
gen choice2=.
gen choice3=.

gen choicecert1=.
gen choicecert2=.
gen choicecert3=.

replace choice1=1 if ce==1
replace choice2=1 if ce==2
replace choice3=1 if ce==3

reshape long choice, i(id set) j(alternativeoptions)
replace choice=0 if choice==.

*** Creating Variables for Attribute Levels (Info from Optimal Design)

destring block, replace
* N Application (0 is Not Rquired, 1 is Lenient, 2 is Strict)
generate napplication = ., before(alternative)
replace napplication = 0 if alternativeoptions==3
replace napplication = 2 if block==1 & set==1 & alternativeoptions==1
replace napplication = 1 if block==1 & set==1 & alternativeoptions==2
replace napplication = 0 if block==1 & set==2 & alternativeoptions==1
replace napplication = 2 if block==1 & set==2 & alternativeoptions==2
replace napplication = 1 if block==2 & set==1 & alternativeoptions==1
replace napplication = 2 if block==2 & set==1 & alternativeoptions==2
replace napplication = 0 if block==2 & set==2 & alternativeoptions==1
replace napplication = 1 if block==2 & set==2 & alternativeoptions==2
replace napplication = 0 if block==3 & set==1 & alternativeoptions==1
replace napplication = 2 if block==3 & set==1 & alternativeoptions==2
replace napplication = 1 if block==3 & set==2 & alternativeoptions==1
replace napplication = 0 if block==3 & set==2 & alternativeoptions==2
replace napplication = 1 if block==4 & set==1 & alternativeoptions==1
replace napplication = 0 if block==4 & set==1 & alternativeoptions==2
replace napplication = 2 if block==4 & set==2 & alternativeoptions==1
replace napplication = 1 if block==4 & set==2 & alternativeoptions==2
replace napplication = 0 if block==5 & set==1 & alternativeoptions==1
replace napplication = 2 if block==5 & set==1 & alternativeoptions==2
replace napplication = 2 if block==5 & set==2 & alternativeoptions==1
replace napplication = 1 if block==5 & set==2 & alternativeoptions==2

* Cover Crops (0 is Not Required, 1 is Required)
generate covercrops = ., before(alternative)
replace covercrops = 0 if alternativeoptions==3
replace covercrops = 1 if block==1 & set==1 & alternativeoptions==1
replace covercrops = 0 if block==1 & set==1 & alternativeoptions==2
replace covercrops = 1 if block==1 & set==2 & alternativeoptions==1
replace covercrops = 0 if block==1 & set==2 & alternativeoptions==2
replace covercrops = 1 if block==2 & set==1 & alternativeoptions==1
replace covercrops = 0 if block==2 & set==1 & alternativeoptions==2
replace covercrops = 1 if block==2 & set==2 & alternativeoptions==1
replace covercrops = 1 if block==2 & set==2 & alternativeoptions==2
replace covercrops = 1 if block==3 & set==1 & alternativeoptions==1
replace covercrops = 0 if block==3 & set==1 & alternativeoptions==2
replace covercrops = 1 if block==3 & set==2 & alternativeoptions==1
replace covercrops = 1 if block==3 & set==2 & alternativeoptions==2
replace covercrops = 0 if block==4 & set==1 & alternativeoptions==1
replace covercrops = 1 if block==4 & set==1 & alternativeoptions==2
replace covercrops = 1 if block==4 & set==2 & alternativeoptions==1
replace covercrops = 0 if block==4 & set==2 & alternativeoptions==2
replace covercrops = 1 if block==5 & set==1 & alternativeoptions==1
replace covercrops = 1 if block==5 & set==1 & alternativeoptions==2
replace covercrops = 1 if block==5 & set==2 & alternativeoptions==1
replace covercrops = 0 if block==5 & set==2 & alternativeoptions==2

* Funding Source (0 is No Source, 1 is State/Federal, 2 is Private)
generate fundingsource = ., before(alternative)
replace fundingsource = 0 if alternativeoptions==3
replace fundingsource = 2 if block==1 & set==1 & alternativeoptions==1
replace fundingsource = 1 if block==1 & set==1 & alternativeoptions==2
replace fundingsource = 1 if block==1 & set==2 & alternativeoptions==1
replace fundingsource = 2 if block==1 & set==2 & alternativeoptions==2
replace fundingsource = 2 if block==2 & set==1 & alternativeoptions==1
replace fundingsource = 1 if block==2 & set==1 & alternativeoptions==2
replace fundingsource = 2 if block==2 & set==2 & alternativeoptions==1
replace fundingsource = 1 if block==2 & set==2 & alternativeoptions==2
replace fundingsource = 2 if block==3 & set==1 & alternativeoptions==1
replace fundingsource = 1 if block==3 & set==1 & alternativeoptions==2
replace fundingsource = 1 if block==3 & set==2 & alternativeoptions==1
replace fundingsource = 2 if block==3 & set==2 & alternativeoptions==2
replace fundingsource = 2 if block==4 & set==1 & alternativeoptions==1
replace fundingsource = 1 if block==4 & set==1 & alternativeoptions==2
replace fundingsource = 1 if block==4 & set==2 & alternativeoptions==1
replace fundingsource = 2 if block==4 & set==2 & alternativeoptions==2
replace fundingsource = 1 if block==5 & set==1 & alternativeoptions==1
replace fundingsource = 2 if block==5 & set==1 & alternativeoptions==2
replace fundingsource = 1 if block==5 & set==2 & alternativeoptions==1
replace fundingsource = 1 if block==5 & set==2 & alternativeoptions==2

* Payment (0 is $0, 1 is $130, 2 is $100, 3 is $70, 4 is $40, 5 is $10)
generate payment = ., before(alternative)
replace payment = 0 if alternativeoptions==3
replace payment = 1 if block==1 & set==1 & alternativeoptions==1
replace payment = 5 if block==1 & set==1 & alternativeoptions==2
replace payment = 2 if block==1 & set==2 & alternativeoptions==1
replace payment = 3 if block==1 & set==2 & alternativeoptions==2
replace payment = 4 if block==2 & set==1 & alternativeoptions==1
replace payment = 5 if block==2 & set==1 & alternativeoptions==2
replace payment = 4 if block==2 & set==2 & alternativeoptions==1
replace payment = 1 if block==2 & set==2 & alternativeoptions==2
replace payment = 5 if block==3 & set==1 & alternativeoptions==1
replace payment = 4 if block==3 & set==1 & alternativeoptions==2
replace payment = 3 if block==3 & set==2 & alternativeoptions==1
replace payment = 1 if block==3 & set==2 & alternativeoptions==2
replace payment = 2 if block==4 & set==1 & alternativeoptions==1
replace payment = 3 if block==4 & set==1 & alternativeoptions==2
replace payment = 4 if block==4 & set==2 & alternativeoptions==1
replace payment = 2 if block==4 & set==2 & alternativeoptions==2
replace payment = 3 if block==5 & set==1 & alternativeoptions==1
replace payment = 5 if block==5 & set==1 & alternativeoptions==2
replace payment = 2 if block==5 & set==2 & alternativeoptions==1
replace payment = 1 if block==5 & set==2 & alternativeoptions==2

* Adjust Payment Variable to Dollars
replace payment = 130 if payment==1
replace payment = 100 if payment==2
replace payment = 70 if payment==3
replace payment = 40 if payment==4
replace payment = 10 if payment==5


*** Creating Dummy Variables

* N Application Dummy Variables
generate dummylenientn = ., before(covercrops)
replace dummylenientn = 1 if napplication==1
replace dummylenientn = 0 if napplication==0 | napplication==2

generate dummystrictn = ., before(covercrops)
replace dummystrictn = 1 if napplication==2
replace dummystrictn = 0 if napplication==0 | napplication==1


* Funding Source Dummy Variables
generate dummystatefunding = ., before(payment)
replace dummystatefunding = 1 if fundingsource==1
replace dummystatefunding = 0 if fundingsource==0 | fundingsource==2

generate dummyprivatefunding = ., before(payment)
replace dummyprivatefunding = 1 if fundingsource==2
replace dummyprivatefunding = 0 if fundingsource==0 | fundingsource==1


*** Creating the Alternative Specific Constant (ASC)

* ASC (0 if anything other than status quo, 1 if status quo)
generate asc = ., before(alternative)
replace asc = 1 if alternativeoptions==3
replace asc = 0 if alternativeoptions !=3

*gen Case Variable
gen case=10*id+set

sort case
by case: egen totalchosen=total(choice)
tab totalchosen

*** Running the Conditional Logit

** Conditional Logit for all Variabes
* Dummy Variables for N application, Cover Crops, and Funding Source
* vce command clusters standard errors around farmers
clogit choice  dummylenientn dummystrictn covercrops dummystatefunding  payment asc, group(id) vce(cluster id)


gen choice_cert=choice
replace choice_cert=1 if ce!=3&alternativeoptions==3&cecert<7
replace choice_cert=0 if ce!=3&alternativeoptions!=3&cecert<7



*mixlogit choice_cert  payment, rand(dummylenientn dummystrictn covercrops dummystatefunding asc) group(case) id(id) nrep(500)

*mixlogit choice_cert payment dummylenientn dummystrictn dummystatefunding , rand(asc covercrops) group(case ) id(id) nrep(500)

*******************************************************************************************************************************************************************
***This is the model. Make usre to add the choice certainty code here. Use this model, but use ce_cert as the dependent variable


tab choice_cert if alternativeoptions==3
tab choice if alternativeoptions==3


set seed 48292
set more off 
mixlogit choice_cert dummystatefunding payment dummystrictn dummylenientn, rand(asc covercrops ) group(case) id(id) nrep(500)


**Code that generates the coefficients
mixlbeta dummylenientn dummystrictn dummystatefunding asc covercrops payment if e(sample)==1, sav("D:\All\Advising\Tapas\stata data\mixlbetas") replace 

save "D:\All\Advising\Tapas\stata data\post mixlogit data.dta", replace

use "D:\All\Advising\Tapas\stata data\mixlbetas.dta" , clear

rename dummylenientn bdummylenientn
rename dummystrictn bdummystrictn
rename dummystatefunding bdummystatefunding
rename asc basc
rename covercrops bcovercrops
rename payment bpayment

save "D:\All\Advising\Tapas\stata data\mixlbetas.dta" , replace


use "D:\All\Advising\Tapas\stata data\post mixlogit data.dta", clear

duplicates drop id, force

merge 1:1 id using "D:\All\Advising\Tapas\stata data\mixlbetas.dta"
gen ones=1
drop if bpayment==.
replace q39_3_1="." if q39_3_1=="?"
replace q39_3_2="." if q39_3_2=="?"
destring q39_3_1 q39_3_2, replace 
*replace q7=q39_1_1/q39_3_1 if q7==.

***Contract with strict N limit, state funding 
gen WTA_StrictN =(basc-(bdummystrictn+bdummystatefunding))/bpayment

***Contract with cover crops, state funding 
gen WTA_CC =(basc-bcovercrops-bdummystatefunding)/bpayment

****Change this for the new data set
gen stotalsamp=76

destring q7, replace
tab q7, missing
sort q7
tab q39_3_1
replace q39_3_1="" if q39_3_1=="."
replace q39_3_1="100" if q39_3_1=="100+"
destring q39_1_1 q39_3_1, replace
replace q7=q39_1_1/q39_3_1 if q7==.
egen total_acres=total(q7)
/*
tab q8, missing
tab q22, missing
sort q8

***Imputing missing values for subbasin using contact information
***Make variable distinguising imputed values
gen impute_subbasin=q8==.

***Placing Craven Co. person in subbasin 2
replace q8=2 if externalreference==2878
***Placing Wilson Co. person in subbasin 4
replace q8=4 if externalreference==39234
***Placing Halifax Co. person in subbasin 7
replace q8=7 if externalreference==2434
***Placing Wilson Co. person in subbasin 4
replace q8=4 if externalreference==74751
***Placing Coastal person (zip code 27919) in subbasin 1
replace q8=1 if externalreference==73155
***Placing Pitt County person (zip code 27834) in subbasin 3
replace q8=3 if externalreference==3554
***Placing Nash County person (zip code 27807) in subbasin 6
replace q8=6 if externalreference==88449

tab q8, missing

gen downstream=q8<4

by downstream, sort : summarize WTA_StrictN WTA_CC
by downstream, sort : summarize basc bcovercrops bpayment

*/
****Total in sample: 4,607 acres
****Total in Tar-Pamlico: 28% of land x 6,400 sq. miles in Tar-Pam * 640 acres in sq. mile = 1,146,880 acres in Tar-Pamlico

****One acre in our sample represents 250 acres in Tar Pam

***Simulation for Strict N restriction
egen totalSN=count(ones) if WTA_StrictN<=50
sum totalSN
scalar stotalSN=r(mean) 
scalar penrolledSN=stotalSN/stotalsamp
di penrolledSN

***Total acreage enrolled
egen total_acreageSN=total(q7) if WTA_StrictN<=50

/*
***Total acres enrolled by subbasin groups
gen basin_group1=q8==1|q8==2
gen basin_group2=q8==3|q8==4
gen basin_group3=q8>4
**Group 1 (subbasins 1&2)
egen total_acreageSN_g1=total(q7) if WTA_StrictN<=50&basin_group1==1
**Group 2 (subbasins 3&4)
egen total_acreageSN_g2=total(q7) if WTA_StrictN<=50&basin_group2==1
**Group 3 (subbasins 5-10)
egen total_acreageSN_g3=total(q7) if WTA_StrictN<=50&basin_group3==1
sum total_acreageSN_g1 total_acreageSN_g2 total_acreageSN_g3

egen total_acreage_g1 = total(q7) if basin_group1==1
egen total_acreage_g2 = total(q7) if basin_group2==1
egen total_acreage_g3 = total(q7) if basin_group3==1
sum total_acreage_g1 total_acreage_g2 total_acreage_g3
*/
egen total_acreage = total(q7) 
sum total_acreageSN
scalar pacreage_enrolled_SN=total_acreageSN/total_acreage
gen pacreage_enrolled_SN=total_acreageSN/total_acreage
sum pacreage_enrolled_SN


***Total in sample: ???
*total in Tar-Pam = 1,382.277*250 = 345,569 acres at cost of 345,569*50 = $17,278,462 


***Simulation for Cover Crops
egen totalCC=count(ones) if WTA_CC<=75
sum totalCC
scalar stotalCC=r(mean) 
scalar penrolledCC=stotalCC/stotalsamp
di penrolledCC

***Total acreage enrolled
egen total_acreageCC=total(q7) if WTA_CC<=75
sum total_acreageCC

egen total_acreageCC_add=total(q7) if WTA_CC<=75&q24==1
sum total_acreageCC_add

/*
egen total_acreageCCdown=total(q7) if WTA_CC<=50&downstream==1
sum total_acreageCCdown
egen total_acreageCCup=total(q7) if WTA_CC<=50&downstream==0
sum total_acreageCCup
*/
gen pacreage_enrolled_CC=total_acreageCC/total_acreage
sum pacreage_enrolled_CC

gen pacreage_enrolled_CC_add=total_acreageCC_add/total_acreage
sum pacreage_enrolled_CC_add


***Total in sample: 1,539.277
*total in Tar-Pam = 1,539.277*250 = 384,750 acres at cost of 384,750*50 = $26,932,500



