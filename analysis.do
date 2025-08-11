clear 

cd "C:\Users\zat4002\Box\Meta analysis paper\Paper\First review round\Codes for Github"
import excel "dataimputed.xlsx", sheet("Sheet1") firstrow

*Recoding of variables

egen WHORegion_num = group(WHORegion), label
list WHORegion WHORegion_num
label list

egen Under3_num = group(Under3), label
list Under3 Under3_num
label list

*Categorize variables 

generate improvedwater = 0 if HouseholdswithImprovedDrinkin>=0 & HouseholdswithImprovedDrinkin<0.5
replace improvedwater  = 1 if HouseholdswithImprovedDrinkin>=0.5 & HouseholdswithImprovedDrinkin<0.75
replace improvedwater = 2 if HouseholdswithImprovedDrinkin>=0.75 & HouseholdswithImprovedDrinkin<=1.00
label define improvedwaterlbl 0"Low" 1"Intermediate" 2"High" 
label values improvedwater improvedwaterlbl

generate improvedsanit = 0 if HouseholdswithImprovedSanitat>=0 & HouseholdswithImprovedSanitat<0.5
replace improvedsanit  = 1 if HouseholdswithImprovedSanitat>=0.5 & HouseholdswithImprovedSanitat<0.75
replace improvedsanit = 2 if HouseholdswithImprovedSanitat>=0.75 & HouseholdswithImprovedSanitat<=1.00
label define improvedsanitlbl 0"Low" 1"Intermediate" 2"High"  
label values improvedsanit improvedsanitlbl

generate wateraccess = 0 if WaterAccessibilityIndex>=0 & WaterAccessibilityIndex<0.28
replace wateraccess  = 1 if WaterAccessibilityIndex>=0.28 & WaterAccessibilityIndex<0.455
replace wateraccess = 2 if WaterAccessibilityIndex>=0.455 & WaterAccessibilityIndex<=1.00
label define wateraccesslbl 0"Low" 1"Intermediate" 2"High" 
label values wateraccess wateraccesslbl

generate avghousesize = 0 if AverageHouseholdSize>=0 & AverageHouseholdSize<4
replace avghousesize  = 1 if AverageHouseholdSize>=4 & AverageHouseholdSize<5
replace avghousesize = 2 if AverageHouseholdSize>=5 & AverageHouseholdSize<6
replace avghousesize  = 3 if AverageHouseholdSize>=6
label define avghousesizelbl 0"Small" 1"Medium" 2"Upper-Intermediate" 3"High" 
label values avghousesize avghousesizelbl

generate motheredu = 0 if MaternalEducationLevelIndex>=0 & MaternalEducationLevelIndex<0.5
replace motheredu = 1 if MaternalEducationLevelIndex>=0.5 & MaternalEducationLevelIndex<0.75
replace motheredu = 2 if MaternalEducationLevelIndex>=0.75 & MaternalEducationLevelIndex<=1.00
label define motheredulbl 0"Low" 1"Intermediate" 2"High" 
label values motheredu motheredulbl

generate hdi = 0 if HumanDevelopmentIndexHDI>=0 & HumanDevelopmentIndexHDI<0.550
replace hdi = 1 if HumanDevelopmentIndexHDI>=0.550 & HumanDevelopmentIndexHDI<0.70
replace hdi = 2 if HumanDevelopmentIndexHDI>=0.70 & HumanDevelopmentIndexHDI<=1
label define hdilbl 0"Low" 1"Medium" 2"High-Very High" 
label values hdi hdilbl

generate density = 0 if PopulationDensitypeoplepers>=0 & PopulationDensitypeoplepers<10
replace density = 1 if PopulationDensitypeoplepers>=10 & PopulationDensitypeoplepers<50
replace density = 2 if PopulationDensitypeoplepers>=50 & PopulationDensitypeoplepers<100
replace density = 3 if PopulationDensitypeoplepers>=100 & PopulationDensitypeoplepers<500
replace density = 4 if PopulationDensitypeoplepers>=500
label define densitylbl 0"0-9.9" 1"10-49.9" 2"50-99.9" 3"100-499.9" 4">=500" 
label values density densitylbl

generate urbanhouseholds = 0 if ProportionofUrbanHouseholds>=0 & ProportionofUrbanHouseholds<0.5
replace urbanhouseholds  = 1 if ProportionofUrbanHouseholds>=0.5 & ProportionofUrbanHouseholds<=1.00
replace urbanhouseholds = 2 if ProportionofUrbanHouseholds>=0.75 & ProportionofUrbanHouseholds
label define urbanhouseholdslbl  0"Low" 1"Intermediate" 2"High"  
label values urbanhouseholds urbanhouseholdslbl

generate underweight = 0 if UnderweightPrevalenceinChildr>=0 & UnderweightPrevalenceinChildr<0.1
replace underweight  = 1 if UnderweightPrevalenceinChildr>=0.1 & UnderweightPrevalenceinChildr<0.2
replace underweight = 2 if UnderweightPrevalenceinChildr>=0.2 & UnderweightPrevalenceinChildr<0.3
replace underweight  = 3 if UnderweightPrevalenceinChildr>=0.3 & UnderweightPrevalenceinChildr<=1
label define underweightlbl 0"Low" 1"Medium" 2"Upper-Intermediate" 3"High" 
label values underweight underweightlbl

generate stunting = 0 if StuntingPrevalenceinChildren>=0 & StuntingPrevalenceinChildren<0.2
replace stunting   = 1 if StuntingPrevalenceinChildren>=0.2 & StuntingPrevalenceinChildren<0.3
replace stunting  = 2 if StuntingPrevalenceinChildren>=0.3 & StuntingPrevalenceinChildren<0.4
replace stunting   = 3 if StuntingPrevalenceinChildren>=0.4 & StuntingPrevalenceinChildren<=1
label define stuntinglbl 0"Low" 1"Medium" 2"Upper-Intermediate" 3"High" 
label values stunting  stuntinglbl

generate wasting = 0 if WastingPrevalenceinChildrenU>=0 & WastingPrevalenceinChildrenU<0.05
replace wasting  = 1 if WastingPrevalenceinChildrenU>=0.05 & WastingPrevalenceinChildrenU<0.1
replace wasting = 2 if WastingPrevalenceinChildrenU>=0.1 & WastingPrevalenceinChildrenU<0.15
replace wasting  = 3 if WastingPrevalenceinChildrenU>=0.15 & WastingPrevalenceinChildrenU<=1
label define wastinglbl 0"Low" 1"Medium" 2"Upper-Intermediate" 3"High"
label values wasting wastinglbl

generate gdppercap = 0 if GrossDomesticProductGDPper>=0 & GrossDomesticProductGDPper<=1145
replace gdppercap  = 1 if GrossDomesticProductGDPper>1145 & GrossDomesticProductGDPper<=4515
replace gdppercap = 2 if GrossDomesticProductGDPper>4515 & GrossDomesticProductGDPper<=14005
replace gdppercap  = 3 if GrossDomesticProductGDPper>14005
label define gdppercaplbl 0"Low income" 1"Lower-middle-income" 2"Upper-middle-income" 3"High income"
label values gdppercap gdppercaplbl

*Tabulation variables

tab Under3_num
tab WHORegion_num
tab urbanhouseholds
tab motheredu
tab avghousesize
tab density
tab hdi
tab gdppercap
tab improvedwater
tab improvedsanit
tab wateraccess
tab underweight
tab stunting
tab wasting

bysort Under3_num: tabstat TotalSampleSizen, statistics(sum)
bysort WHORegion_num: tabstat TotalSampleSizen, statistics(sum)
bysort urbanhouseholds: tabstat TotalSampleSizen, statistics(sum)
bysort motheredu: tabstat TotalSampleSizen, statistics(sum)
bysort avghousesize: tabstat TotalSampleSizen, statistics(sum)
bysort density: tabstat TotalSampleSizen, statistics(sum)
bysort hdi: tabstat TotalSampleSizen, statistics(sum)
bysort gdppercap: tabstat TotalSampleSizen, statistics(sum)
bysort improvedwater: tabstat TotalSampleSizen, statistics(sum)
bysort improvedsanit: tabstat TotalSampleSizen, statistics(sum)
bysort wateraccess: tabstat TotalSampleSizen, statistics(sum)
bysort underweight: tabstat TotalSampleSizen, statistics(sum)
bysort stunting: tabstat TotalSampleSizen, statistics(sum)
bysort wasting: tabstat TotalSampleSizen, statistics(sum)

*Generating standard errors

gen trans=log((TotalNumberofDiarrheaCases)/(TotalSampleSizen))
gen SE=sqrt((1/(TotalNumberofDiarrheaCases))-(1/(TotalSampleSizen)))


***Univariable metaregression

xi :metareg trans i.Under3_num, wsse(SE) eform z
xi :metareg trans i.WHORegion_num, wsse(SE) eform z
xi :metareg trans i.urbanhouseholds, wsse(SE) eform z
xi :metareg trans i.motheredu, wsse(SE) eform z
xi :metareg trans i.avghousesize, wsse(SE) eform z
xi :metareg trans i.density, wsse(SE) eform z
xi :metareg trans i.hdi, wsse(SE) eform z
xi :metareg trans i.gdppercap, wsse(SE) eform z
xi :metareg trans i.improvedwater, wsse(SE) eform z
xi :metareg trans i.improvedsanit, wsse(SE) eform z
xi :metareg trans i.wateraccess, wsse(SE) eform z
xi :metareg trans i.underweight, wsse(SE) eform z
xi :metareg trans i.stunting, wsse(SE) eform z
xi :metareg trans i.wasting, wsse(SE) eform z
xi :metareg trans Year_calculated, wsse(SE) eform z


*stratification by WHO region

preserve
keep if WHORegion_num == 1
collect clear
local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 1
collect clear

local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), statistic(sum TotalSampleSizen) append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 1
xi :metareg trans i.Under3_num, wsse(SE) eform z
xi :metareg trans i.urbanhouseholds, wsse(SE) eform z
xi :metareg trans i.motheredu, wsse(SE) eform z
xi :metareg trans i.avghousesize, wsse(SE) eform z
xi :metareg trans i.density, wsse(SE) eform z
xi :metareg trans i.hdi, wsse(SE) eform z
xi :metareg trans i.gdppercap, wsse(SE) eform z
xi :metareg trans i.improvedwater, wsse(SE) eform z
xi :metareg trans i.improvedsanit, wsse(SE) eform z
xi :metareg trans i.wateraccess, wsse(SE) eform z
xi :metareg trans i.underweight, wsse(SE) eform z
xi :metareg trans i.stunting, wsse(SE) eform z
xi :metareg trans i.wasting, wsse(SE) eform z
xi :metareg trans Year_calculated, wsse(SE) eform z
restore

preserve
keep if WHORegion_num == 2
collect clear
local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 2
collect clear

local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), statistic(sum TotalSampleSizen) append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 2
xi :metareg trans i.Under3_num, wsse(SE) eform z
xi :metareg trans i.urbanhouseholds, wsse(SE) eform z
xi :metareg trans i.motheredu, wsse(SE) eform z
xi :metareg trans i.avghousesize, wsse(SE) eform z
xi :metareg trans i.density, wsse(SE) eform z
xi :metareg trans i.hdi, wsse(SE) eform z
xi :metareg trans i.gdppercap, wsse(SE) eform z
xi :metareg trans i.improvedwater, wsse(SE) eform z
xi :metareg trans i.improvedsanit, wsse(SE) eform z
xi :metareg trans i.wateraccess, wsse(SE) eform z
xi :metareg trans i.underweight, wsse(SE) eform z
xi :metareg trans i.stunting, wsse(SE) eform z
xi :metareg trans i.wasting, wsse(SE) eform z
xi :metareg trans Year_calculated, wsse(SE) eform z
restore

preserve
keep if WHORegion_num == 3
collect clear
local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 3
collect clear

local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), statistic(sum TotalSampleSizen) append totals(`var')
     }

collect layout (`varlist') (result)

restore

preserve
keep if WHORegion_num == 3
xi :metareg trans i.Under3_num, wsse(SE) eform z
xi :metareg trans i.urbanhouseholds, wsse(SE) eform z
xi :metareg trans i.motheredu, wsse(SE) eform z
xi :metareg trans i.avghousesize, wsse(SE) eform z
xi :metareg trans i.density, wsse(SE) eform z
xi :metareg trans i.hdi, wsse(SE) eform z
xi :metareg trans i.gdppercap, wsse(SE) eform z
xi :metareg trans i.improvedwater, wsse(SE) eform z
xi :metareg trans i.improvedsanit, wsse(SE) eform z
xi :metareg trans i.wateraccess, wsse(SE) eform z
xi :metareg trans i.underweight, wsse(SE) eform z
xi :metareg trans i.stunting, wsse(SE) eform z
xi :metareg trans i.wasting, wsse(SE) eform z
xi :metareg trans Year_calculated, wsse(SE) eform z
restore

preserve
keep if WHORegion_num == 4
collect clear
local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 4
collect clear

local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), statistic(sum TotalSampleSizen) append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 4
xi :metareg trans i.Under3_num, wsse(SE) eform z
xi :metareg trans i.urbanhouseholds, wsse(SE) eform z
xi :metareg trans i.motheredu, wsse(SE) eform z
xi :metareg trans i.avghousesize, wsse(SE) eform z
xi :metareg trans i.density, wsse(SE) eform z
xi :metareg trans i.hdi, wsse(SE) eform z
xi :metareg trans i.gdppercap, wsse(SE) eform z
xi :metareg trans i.improvedwater, wsse(SE) eform z
xi :metareg trans i.improvedsanit, wsse(SE) eform z
xi :metareg trans i.wateraccess, wsse(SE) eform z
xi :metareg trans i.underweight, wsse(SE) eform z
xi :metareg trans i.stunting, wsse(SE) eform z
xi :metareg trans i.wasting, wsse(SE) eform z
xi :metareg trans Year_calculated, wsse(SE) eform z
restore

preserve
keep if WHORegion_num == 5
collect clear
local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 5
collect clear

local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), statistic(sum TotalSampleSizen) append totals(`var')
     }

collect layout (`varlist') (result)

restore

preserve
keep if WHORegion_num == 5
xi :metareg trans i.Under3_num, wsse(SE) eform z
xi :metareg trans i.urbanhouseholds, wsse(SE) eform z
xi :metareg trans i.motheredu, wsse(SE) eform z
xi :metareg trans i.avghousesize, wsse(SE) eform z
xi :metareg trans i.density, wsse(SE) eform z
xi :metareg trans i.hdi, wsse(SE) eform z
xi :metareg trans i.gdppercap, wsse(SE) eform z
xi :metareg trans i.improvedwater, wsse(SE) eform z
xi :metareg trans i.improvedsanit, wsse(SE) eform z
xi :metareg trans i.wateraccess, wsse(SE) eform z
xi :metareg trans i.underweight, wsse(SE) eform z
xi :metareg trans i.stunting, wsse(SE) eform z
xi :metareg trans i.wasting, wsse(SE) eform z
xi :metareg trans Year_calculated, wsse(SE) eform z
restore

preserve
keep if WHORegion_num == 6
collect clear
local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), append totals(`var')
     }

collect layout (`varlist') (result)
restore

preserve
keep if WHORegion_num == 6
collect clear

local varlist Under3_num urbanhouseholds motheredu avghousesize density hdi gdppercap improvedwater improvedsanit wateraccess underweight stunting wasting

foreach var of local varlist {
     table (`var'), statistic(sum TotalSampleSizen) append totals(`var')
     }

collect layout (`varlist') (result)

restore

preserve
keep if WHORegion_num == 6
xi :metareg trans i.Under3_num, wsse(SE) eform z
xi :metareg trans i.urbanhouseholds, wsse(SE) eform z
xi :metareg trans i.motheredu, wsse(SE) eform z
xi :metareg trans i.avghousesize, wsse(SE) eform z
xi :metareg trans i.density, wsse(SE) eform z
xi :metareg trans i.hdi, wsse(SE) eform z
xi :metareg trans i.gdppercap, wsse(SE) eform z
xi :metareg trans i.improvedwater, wsse(SE) eform z
xi :metareg trans i.improvedsanit, wsse(SE) eform z
xi :metareg trans i.wateraccess, wsse(SE) eform z
xi :metareg trans i.underweight, wsse(SE) eform z
xi :metareg trans i.stunting, wsse(SE) eform z
xi :metareg trans i.wasting, wsse(SE) eform z
xi :metareg trans Year_calculated, wsse(SE) eform z
restore