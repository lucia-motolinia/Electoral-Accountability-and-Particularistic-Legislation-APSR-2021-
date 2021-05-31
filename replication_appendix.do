/* Electoral Accountability and Particularistic Legislation: Evidence from an Electoral Reform in Mexico
Author: Lucia Motolinia*/
*******Replication Code*******

clear all
set more off
cd "D:\Users\Lucia\Dropbox\NYU\Papers\Reelection and promises update\Diputados Locales\00Results\_Replication_\"

***Tables and Figures in Appendix***
use Partic_Mex_2020.dta, clear

*Table A4
local varlist "particularistic"
foreach x in `varlist'{
forval i=1/20{
display "`x' `i'"
sum `x' if edo==`i'
return list
sca m1=r(mean)
matrix col1 =nullmat(col1) \ (m1)  
}
}
local varlist "general"
foreach x in `varlist'{
forval i=1/20{
display "`x' `i'"
sum `x' if edo==`i'
return list
sca m2=r(mean)
matrix col2 =nullmat(col2) \ (m2)  
}
}
local varlist "procedural"
foreach x in `varlist'{
forval i=1/20{
display "`x' `i'"
sum `x' if edo==`i'
return list
sca m3=r(mean)
matrix col3 =nullmat(col3) \ (m3)  
}
}
mat appolicy= col1,col2,col3
matlist appolicy
outtable using "A4.tex", mat(appolicy) replace center f(%9.3f %9.3f %9.3f)
mat drop col1 col2 col3 appolicy

*Table A5
local varlist "particularistic general procedural"
foreach x in `varlist'{
display "`x'"
sum `x' 
return list
sca m1=r(mean)
sca m2=r(sd)
sca m3=r(min)
sca m4=r(max)
sca m5=r(N)
matrix col1 =nullmat(col1) \ (m1)  
matrix col2 =nullmat(col2) \ (m2) 
matrix col3 =nullmat(col3) \ (m3) 
matrix col4 =nullmat(col4) \ (m4) 
matrix col5 =nullmat(col5) \ (m5) 
}
mat appA= col1,col2,col3,col4,col5
matlist appA
outtable using "A5.tex", mat(appA) replace center f(%9.3f %9.3f %9.3f %9.3f)
mat drop col1 col2 col3 col4 col5 appA

*Table A7
gen calendar=.
replace calendar=1 if treat4==0
replace calendar=0 if treat4==1
replace calendar=1 if state=="Chiapas" | state=="Tabasco" | state=="Mexico" | state=="Guanajuato" | state=="Jalisco" 

local vars "calendar"
foreach x in `vars'{
reg particularistic c.treat4#c.reform c.`x'#c.reform i.edo i.date2,robust cluster(edo) 
outreg2 using "A7.tex", keep(c.treat4#c.reform c.`x'#c.reform) dec(5) onecol se br symbol(***, **, *) append
}

*Table A9
gen pri=.
replace pri=1 if treat42==0
replace pri=0 if treat42==1 
replace pri=1 if state=="Veracruz" | state=="Chiapas" | state=="Campeche" | state=="Durango" | state=="Jalisco" | state=="Mexico" | state=="Queretaro" | state=="Tlaxcala"

local vars "particularistic"
foreach x in `vars'{
reg `x' c.treat42#c.reform c.pri#c.reform i.edo i.date2,robust cluster(edo) 
outreg2 using "A9.tex", keep(c.treat42#c.reform c.pri#c.reform) dec(5) onecol se br symbol(***, **, *) replace
}

*Table A8
merge m:1 state using "inegi_baseline.dta"
drop if _merge==2
drop _merge
gen pctincometax= incometax/income*100

local vars "poppobing poppobsoc exports popinmi popemi homicides pop pophealth income pctincometax participaciones" 
foreach x in `vars'{
reg particularistic c.treat4#c.reform c.`x'#c.reform i.edo i.date2,robust cluster(edo) 
outreg2 using "A8.tex", keep(c.treat4#c.reform c.`x'#c.reform) dec(5) onecol se br symbol(***, **, *) append
}

*Table A11
local vars "pop poppobing poppobsoc popinmi popemi homicides exports pophealth income pctincometax participaciones" 
foreach x in `vars'{
reg particularistic c.treat42#c.reform c.`x'#c.reform i.edo i.date2,robust cluster(edo) 
outreg2 using "A11.tex", keep(c.treat42#c.reform c.`x'#c.reform) dec(5) onecol se br symbol(***, **, *) append
}

*Table A10
merge m:1 state using "leg_cov_baseline.dta"
drop if _merge==2
drop _merge
local vars "education_years polexperience_any_years polexperience_appointed_years polexperience_elected_years" 
foreach x in `vars'{
reg particularistic c.treat42#c.reform c.`x'#c.reform i.edo i.date2,robust cluster(edo) 
outreg2 using "A10.tex", keep(c.treat42#c.reform c.`x'#c.reform) dec(5) onecol se br symbol(***, **, *) append
}

*Table A13
gen timinglic=.
replace timinglic=1 if date2>20150003 & date2<=20150006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timinglic=1 if date2>20180003 & date2<=20180006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timinglic=1 if date2>20160003 & date2<=20160006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timinglic=1 if date2>20180003 & date2<=20180006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timinglic=1 if date2>20130003 & date2<=20130006 & (state=="Puebla")
replace timinglic=1 if date2>20180003 & date2<=20180006 & (state=="Puebla")
gen lic=.
replace lic=1 if state=="Puebla" | state=="CdMex" | state=="Campeche" | state=="Durango"| state=="Guanajuato" | state=="Guerrero" | state=="Jalisco"  | state=="Mexico" | state=="Sinaloa" | state=="Tlaxcala"  | state=="Veracruz"

reg particularistic timb3 i.edo i.date2 if timinglic==1 & lic==1,  robust cluster(edo) 
outreg2 using "A13.tex", keep(c.timb3) dec(5) onecol se br symbol(***, **, *) replace

*Figure A3
reg particularistic c.timb3#c.smdpct c.timb3 c.reform#c.smdpct i.edo i.date2,robust cluster(edo) 
margins, dydx(c.timb3) at(c.smdpct=(60 (2) 70)) 
marginsplot, x(smdpct) recast(line) recastci(rarea) title("Average Marginal Effects of DiD") xtitle("Percentage of the Legislature elected in SSDs") graphregion(col(white)) bgcol(white) 

*Table A14
local vars "unions transfers pg awards infra"
foreach x in `vars'{
reg part_`x' c.treat4#c.reform i.edo i.date2,robust cluster(edo) 
outreg2 using "A14.tex", keep(c.treat4#c.reform) dec(5) onecol se br symbol(***, **, *) append
}

*Figure A4
gen timinga1=.
replace timinga1=1 if date2>20120006 & date2<=20130006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timinga1=1 if date2>20150006 & date2<=20160006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timinga1=1 if date2>20060006 & date2<=20110006 & (state=="Puebla")
replace timinga1=1 if date2>20150006 & date2<=20160006 & (state=="Puebla")

gen timinga2=.
replace timinga2=1 if date2>20130006 & date2<=20140006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timinga2=1 if date2>20160006 & date2<=20170006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timinga2=1 if date2>20060006 & date2<=20110006 & (state=="Puebla")
replace timinga2=1 if date2>20150006 & date2<=20160006 & (state=="Puebla")
replace timinga2=1 if date2>20120006 & date2<=20130006 & (state=="CdMex") 
replace timinga2=1 if date2>20150006 & date2<=20160006 & (state=="CdMex") 

gen timingb1=.
replace timingb1=1 if date2>20130006 & date2<=20140006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingb1=1 if date2>20160006 & date2<=20170006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingb1=1 if date2>20110006 & date2<=20120006 & (state=="Puebla")
replace timingb1=1 if date2>20160006 & date2<=20170006 & (state=="Puebla")

gen timingc1=.
replace timingc1=1 if date2>20140006 & date2<=20150006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingc1=1 if date2>20170006 & date2<=20180006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingc1=1 if date2>20120006 & date2<=20130006 & (state=="Puebla")
replace timingc1=1 if date2>20170006 & date2<=20180006 & (state=="Puebla")

gen timingc2=.
replace timingc2=1 if date2>20150006 & date2<=20160006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timingc2=1 if date2>20170006 & date2<=20180006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timingc2=1 if date2>20120006 & date2<=20130006 & (state=="Puebla")
replace timingc2=1 if date2>20170006 & date2<=20180006 & (state=="Puebla")
replace timingc2=1 if date2>20140006 & date2<=20150006 & (state=="CdMex")
replace timingc2=1 if date2>20170006 & date2<=20180006 & (state=="CdMex")

local vars "particularistic"
foreach x in `vars'{
reg `x' c.timb3 i.edo i.date2 if timinga1==1,  robust cluster(edo) 
estimate store tim_`x'
reg `x' c.timb3 i.edo i.date2 if timingb1==1,  robust cluster(edo) 
estimate store tim2_`x'
reg `x' c.timb3 i.edo i.date2 if timingc1==1,  robust cluster(edo) 
estimate store tim3_`x'
}
coefplot (tim_particularistic)(tim2_particularistic)(tim3_particularistic), vertical keep(timb3) levels(95 90) yline(0) legend(off) coeflabels(timb3="Election (t)	 		t+1 			t+2") graphregion(col(white)) bgcol(white) name(x)
graph drop _all

local vars "particularistic"
foreach x in `vars'{
reg `x' c.timb3 i.edo i.date2 if timinga2==1, robust cluster(edo) 
estimate store timb_`x'
reg `x' c.timb3 i.edo i.date2 if timingc2==1, robust cluster(edo) 
estimate store timb3_`x'
}
coefplot (timb_particularistic) (timb3_particularistic), vertical keep(timb3) levels(95 90) yline(0) legend(off) coeflabels(timb3="Election (t)	 			t+1") graphregion(col(white)) bgcol(white) name(y)
graph drop _all

*Figure A5
gen r2=.
replace r2=0 if date2<20140010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r2=1 if date2>=20140010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r2=0 if date2<20150010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r2=1 if date2>=20150010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r2=0 if date2<20120010 & (state=="Puebla")
replace r2=1 if date2>=20120010 & (state=="Puebla")

gen r3=0 if date2<20130010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r3=1 if date2>=20130010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r3=0 if date2<20140010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r3=1 if date2>=20140010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r3=0 if date2<20110010 & (state=="Puebla")
replace r3=1 if date2>=20110010 & (state=="Puebla")

gen r4=0 if date2<20120010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r4=1 if date2>=20120010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r4=0 if date2<20130010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r4=1 if date2>=20130010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r4=0 if date2<20100010 & (state=="Puebla")
replace r4=1 if date2>=20100010 & (state=="Puebla")

gen r12=.
replace r12=0 if date2<20160010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r12=1 if date2>=20160010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r12=0 if date2<20170010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r12=1 if date2>=20170010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r12=0 if date2<20150010 & (state=="Puebla")
replace r12=1 if date2>=20150010 & (state=="Puebla")

gen r13=.
replace r13=0 if date2<20170010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r13=1 if date2>=20170010 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace r13=0 if date2<20180010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r13=1 if date2>=20180010 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace r13=0 if date2<20160010 & (state=="Puebla")
replace r13=1 if date2>=20160010 & (state=="Puebla")

local vars "particularistic"
local treat "r4 r3 r2 reform r12 r13"
foreach x in `vars'{
foreach y in `treat'{
quietly reg `x' c.treat4#c.`y' i.date2 i.edo,robust cluster(edo) 
estimate store `x'_`y'
}
}
coefplot (particularistic_r4)  (particularistic_r3) (particularistic_r2) (particularistic_reform) (particularistic_r12)  (particularistic_r13), vertical keep(c.treat4#c.r12 c.treat4#c.r13 c.treat4#c.r2 c.treat4#c.reform c.treat4#c.r3 c.treat4#c.r4) yline(0) levels(95 90) legend(off) graphregion(col(white)) bgcol(white) name(p)
graph drop _all

*Hausman Test
fvset base 2 edo
fvset base 20120009 date2
local vars "particularistic"
foreach x in `vars'{
reg `x' c.treat4#c.reform i.edo i.date2
estimates store tj1
reg `x' c.treat42#c.reform i.edo i.date2
estimates store tj2
}
suest tj1 tj2, vce(cl edo)
test [tj1_mean]c.treat4#c.reform=[tj2_mean]c.treat42#c.reform


*Table A2
use legisladores_smd.dta, clear
ttest share_win if reform==1 & (treat4==1 | treat2==1), by(busco_reeleccion)

*Table A6
use legisladores_cv.dta, clear
ttest education_years if treat4!=., by(reform) 
ttest polexperience_any_years if treat4!=., by(reform) 
ttest polexperience_appointed_years if treat4!=., by(reform) 
ttest polexperience_elected_years if treat4!=., by(reform) 

*Table A12
ttest education_years if reform==0, by(treat4) 
ttest polexperience_any_years if reform==0, by(treat4)
ttest polexperience_appointed_years if reform==0, by(treat4)
ttest polexperience_elected_years if reform==0, by(treat4) 

*Table A16
ttest education_years if reform==0, by(treat42) 
ttest polexperience_any_years if reform==0, by(treat42)
ttest polexperience_appointed_years if reform==0, by(treat42)
ttest polexperience_elected_years if reform==0, by(treat42)
