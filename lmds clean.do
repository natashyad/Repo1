clear all

global IN "C:\Users\A0055987\My Drive\Research\PCC\my code2\SA loop try 3_1\Data"

use "$IN\lmdsa-2019-v1.1.dta", clear

global OUT "C:\Users\A0055987\My Drive\Research\PCC\my code2\SA loop try 3_1\Data"

*Note
*there are 11, 935 individual who report being employed and a sector but has no wages
*    ---code impute wage for them based on heckman model
*br Status sector1 Q54a_monthly Q57a_monthly if Status==1 & sector1!=. & Q54a_monthly==. & Q57a_monthly==.


****Employment Status
*gen worker_sim0=1 if (sector1==1|sector1==2|sector1==4) //non-agric
*replace worker_sim0=2 if (sector1==3) & worker_sim0==.  //agric
*replace worker_sim0=3 if (Status==2| Status==3) & worker_sim0==.
*replace worker_sim0=3 if (Q14AGE>=15 & Q14AGE<=65) & Status==4 & worker_sim0==.
*replace worker_sim0=3 if (Status==2| Status==3|Status==4) & Q14AGE>=15 & worker_sim0==. //unemployed or discouraged job seeker
*replace worker_sim0=. if (Status==4) & worker_sim0==.

recode indus (1=1 "Agric") (2=2 "Mining") (3=3 "Manufacturing") (4/11=4 "Services") (5=5 "unemployed"), gen (worker_sim0)

replace worker_sim0=5 if Unempl_Status!=.  //unemployed in data

tab worker_sim0, m
sum Q14AGE if worker_sim0==.    //note the rest are minors (0 to 14)

*label var worker_sim0 "employment status (1=non-agric; 2=agric; 3=unemployed)"



*****Skill level
recode Q17EDUCATION (13/18 19/28=1) (1/12 29/98=0), gen(skilled_sim0)

****identifiers
gen hhid=UQNO
gen pid=PERSONNO

******other dempgraphics
gen age_sim0=Q14AGE
gen age2_sim0=Q14AGE*Q14AGE
gen male=(Q13GENDER==1)

tab Q16MARITALSTATUS, gen(marr)
ren marr1 married
ren marr2 living_together
ren marr3 widowed
ren marr4 divorced
ren marr5 never_married

**educ  (I am sure this is wrong)
gen yrschool = 0
replace yrschool =. if Q17EDUCATION==31 | Q17EDUCATION==30 
replace yrschool = 1 if Q17EDUCATION==1
replace yrschool = 2 if Q17EDUCATION==2
replace yrschool = 3 if Q17EDUCATION==3
replace yrschool = 4 if Q17EDUCATION==4
replace yrschool = 5 if Q17EDUCATION==5
replace yrschool = 6 if Q17EDUCATION==6
replace yrschool = 7 if Q17EDUCATION==7
replace yrschool = 8 if Q17EDUCATION==8
replace yrschool = 9 if Q17EDUCATION==9
replace yrschool = 10 if Q17EDUCATION==10 | Q17EDUCATION==13|Q17EDUCATION==16
replace yrschool = 11 if Q17EDUCATION==11 | Q17EDUCATION==14 | Q17EDUCATION==17
replace yrschool = 12 if Q17EDUCATION==12 | Q17EDUCATION==15|Q17EDUCATION ==18
replace yrschool = 13 if Q17EDUCATION==19 | Q17EDUCATION==20| Q17EDUCATION==21 |Q17EDUCATION==22 | Q17EDUCATION==23
replace yrschool = 15 if Q17EDUCATION==24 
replace yrschool = 16 if Q17EDUCATION==25 
replace yrschool = 17 if Q17EDUCATION==26 |Q17EDUCATION==27 |Q17EDUCATION==28 
label variable yrschool "Years of schooling completed- derived"


gen yrschool1 = .
replace yrschool1 = 0 if Q17EDUCATION == 98|Q17EDUCATION ==0
replace yrschool1 = 1 if Q17EDUCATION == 1
replace yrschool1 = 2 if Q17EDUCATION == 2
replace yrschool1 = 3 if Q17EDUCATION == 3
replace yrschool1 = 4 if Q17EDUCATION == 4
replace yrschool1 = 5 if Q17EDUCATION == 5
replace yrschool1 = 6 if Q17EDUCATION == 6
replace yrschool1 = 7 if Q17EDUCATION == 7
replace yrschool1 = 8 if Q17EDUCATION == 8
replace yrschool1 = 9 if Q17EDUCATION == 9
replace yrschool1 = 10 if Q17EDUCATION == 10
replace yrschool1 = 11 if Q17EDUCATION == 11
replace yrschool1 = 12 if Q17EDUCATION == 12
replace yrschool1 = 13 if Q17EDUCATION > 12 & Q17EDUCATION < 19
replace yrschool1 = 12 if Q17EDUCATION > 18 & Q17EDUCATION < 23
replace yrschool1 = 13 if Q17EDUCATION ==23
replace yrschool1 = 16 if Q17EDUCATION ==24|Q17EDUCATION ==25|Q17EDUCATION ==26|Q17EDUCATION ==27
replace yrschool1 = 19 if Q17EDUCATION ==28



gen children=(Q14AGE<=15)
egen n_children=sum(children), by(UQNO Qtr)
egen hhsize=count(pid), by(UQNO Qtr)

***Province
tab Province, gen(prov)
ren prov1 wCape
ren prov2 eCape
ren prov3 nCape
ren prov4 Fstate
ren prov5 Kwznatal
ren prov6 nWest
ren prov7 Gauteng
ren prov8 Mpml
ren prov9 Lim

***rural/urnan
tab Geo_type_code, gen(geo)
ren geo1 urban
ren geo2 trad
ren geo3 farms

***income
gen wages=Q54a_monthly //employees
replace wages=Q57a_monthly if wages==. //employers and self employees

count if wages >1000000 & wages!=.  //outliers
count if wages==0    //5 zeros

gen ln_wage_1=ln(wage+1) if skilled_sim0==1 & wages!=.
gen ln_wage_0=ln(wage+1) if skilled_sim0==0 &  wages!=.

***race 
gen race=Q15POPULATION

/*
******Weights calibration
gen wgt19_sim0=Weight
tabulate Province [iw=wgt19_sim0],gen(province)   //current population by region

*To compute population in year 1 of simulation we calculate weights that account for 3% population growth by region

*matrix of population by region  
matrix M1 = J(1,9,.)  
matrix colnames M1 = "wCape" "eCape" "nCape" "Fstate" "Kwznatal" "nWest" "Gauteng" "Mpml" "Lim"
matrix rownames M1 =  "population"

*population growth in excel
scalar pop_growth=0.00748896

local i=1
foreach name in "wCape" "eCape" "nCape" "Fstate" "Kwznatal" "nWest" "Gauteng" "Mpml" "Lim" {
	
	tabulate Province [iw=wgt19_sim0] if `name'==1
	matrix M1[1,`i']=`r(N)'*(1+pop_growth)
	
	local i=`i'+1
}      




calibrate , marginals(wCape eCape nCape Fstate Kwznatal nWest Gauteng Mpml Lim) poptot(M1) entrywt(wgt19_sim0) exitwt(wgt19_sim1)

tabulate Province [iw=wgt19_sim1]
*/

keep worker_sim0 skilled_sim0 hhid pid age_sim0 age2_sim0 male married living_together widowed divorced never_married Q16MARITALSTATUS race yrschool yrschool1 children n_children wCape eCape nCape Fstate Kwznatal nWest Gauteng Mpml Lim Province urban farms Geo_type_code wages Weight /*wgt19_sim1 wgt19_sim0*/ hhsize ln_wage_0 ln_wage_1 Qtr Status


save "$OUT\lmdsa_2019_clean.dta", replace

/*
*checks against esternal stats
*1 population size

tab pid [iw= Weight]   //57.34m versus stats SA 58.78  https://www.statssa.gov.za/publications/P0302/P03022019.pdf


*employment figures 
*StatSA doc "https://www.statssa.gov.za/publications/P0211/Presentation%20QLFS%20Q4_2019.pdf" 

tab worker_sim0 [iw= Weight ] if age_sim0>=15 , m

*unemployment rate 25+2.09=27.09
*statSA figure 29.1 (based on 4th quater pg 2)

*breakdown
tab Status [iw= Weight ] if (age_sim0>=15 & age_sim0<=64) , m
*                 Estimate                StatSA
*Employed          16.35M                 16.4M
*Unemployed        6.58M                  6.7M
*Discouraged       2.85M                  2.9M
*NEA               12.73M                 12.7M
*see pg 5 of the document


tab worker_sim0 [iw= Weight ] if (age_sim0>=15 & age_sim0<=64) , m
* how it pans out in our sample
* 1 Employed (non-agric)
* 2 Employed  (agric) 
* 3 Unemployed and discouraged
* 4 NEA 

sum age_sim0 if Status==1 //age range of people in employment is 15 to 96

