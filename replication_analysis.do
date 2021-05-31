/* Electoral Accountability and Particularistic Legislation: Evidence from an Electoral Reform in Mexico
Author: Lucia Motolinia*/
*******Replication Code*******

clear all
set more off
cd "D:\Users\Lucia\Dropbox\NYU\Papers\Reelection and promises update\Diputados Locales\00Results\_Replication_\"

insheet using "allres.csv", clear
destring _all, replace ignore(NA)
encode state, gen(edo)
gen mc="."
replace mc="01" if month=="January"
replace mc="02" if month=="February"
replace mc="03" if month=="March"
replace mc="04" if month=="April"
replace mc="05" if month=="May"
replace mc="06" if month=="June"
replace mc="07" if month=="July"
replace mc="08" if month=="August"
replace mc="09" if month=="September"
replace mc="10" if month=="October"
replace mc="11" if month=="November"
replace mc="12" if month=="December"
rename public general
gen realyear2=realyear*100
egen date2=concat(realyear2 mc)
replace date2="." if mc=="."
destring date2, replace
gen treat42=.
replace treat42=1 if treat4==1
replace treat42=0 if treat2==1
forval i=1/9{
gen rm10`i'=0 if date2<2015000`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm10`i'=1 if date2>=2015000`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm10`i'=0 if date2<2016000`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm10`i'=1 if date2>=2016000`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm10`i'=0 if date2<2013000`i' & (state=="Puebla")
replace rm10`i'=1 if date2>=2013000`i' & (state=="Puebla")
replace rm10`i'=. if reform==1
}

forval i=1/9{
gen rm20`i'=0 if date2<2014000`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm20`i'=1 if date2>=2014000`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm20`i'=0 if date2<2015000`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm20`i'=1 if date2>=2015000`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm20`i'=0 if date2<2012000`i' & (state=="Puebla")
replace rm20`i'=1 if date2>=2012000`i' & (state=="Puebla")
replace rm20`i'=. if reform==1
}

forval i=10/12{
gen rm2`i'=0 if date2<201400`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm2`i'=1 if date2>=201400`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm2`i'=0 if date2<201500`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm2`i'=1 if date2>=201500`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm2`i'=0 if date2<201200`i' & (state=="Puebla")
replace rm2`i'=1 if date2>=201200`i' & (state=="Puebla")
replace rm2`i'=. if reform==1
}

forval i=1/9{
gen rm30`i'=0 if date2<2013000`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm30`i'=1 if date2>=2013000`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm30`i'=0 if date2<2014000`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm30`i'=1 if date2>=2014000`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm30`i'=0 if date2<2011000`i' & (state=="Puebla")
replace rm30`i'=1 if date2>=2011000`i' & (state=="Puebla")
replace rm30`i'=. if reform==1
}

forval i=10/12{
gen rm3`i'=0 if date2<201300`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm3`i'=1 if date2>=201300`i' & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace rm3`i'=0 if date2<201400`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm3`i'=1 if date2>=201400`i' & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace rm3`i'=0 if date2<201100`i' & (state=="Puebla")
replace rm3`i'=1 if date2>=201100`i' & (state=="Puebla")
replace rm3`i'=. if reform==1
}
gen timinga=.
replace timinga=1 if date2>20120006 & date2<=20130006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timinga=1 if date2>20150006 & date2<=20160006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timinga=1 if date2>20130006 & date2<=20140006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timinga=1 if date2>20160006 & date2<=20170006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timinga=1 if date2>20060006 & date2<=20110006 & (state=="Puebla")
replace timinga=1 if date2>20150006 & date2<=20160006 & (state=="Puebla")

gen timingb=.
replace timingb=1 if date2>20130006 & date2<=20140006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingb=1 if date2>20160006 & date2<=20170006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingb=1 if date2>20140006 & date2<=20150006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timingb=1 if date2>20160006 & date2<=20170006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timingb=1 if date2>20110006 & date2<=20120006 & (state=="Puebla")
replace timingb=1 if date2>20160006 & date2<=20170006 & (state=="Puebla")

gen timingc=.
replace timingc=1 if date2>20140006 & date2<=20150006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingc=1 if date2>20170006 & date2<=20180006 & (state=="Jalisco" | state=="Queretaro" | state=="Campeche" | state=="CdMex" | state=="Chiapas" | state=="Colima" | state=="Guanajuato" | state=="Guerrero" | state=="Mexico" | state=="Tabasco") 
replace timingc=1 if date2>20150006 & date2<=20160006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timingc=1 if date2>20170006 & date2<=20180006 & (state=="Oaxaca" | state=="Chihuahua" | state=="Durango" | state=="Tlaxcala" | state=="Veracruz" | state=="Sinaloa" | state=="Aguascalientes" | state=="Oaxaca" | state=="Hidalgo" | state=="Zacatecas") 
replace timingc=1 if date2>20120006 & date2<=20130006 & (state=="Puebla")
replace timingc=1 if date2>20170006 & date2<=20180006 & (state=="Puebla")
gen timb3=treat4*reform

save "Partic_Mex_2020.dta", replace


***Tables and Figures in Results***
use Partic_Mex_2020.dta, clear
*Table 2: H1
reg particularistic timb3 i.edo i.date2,robust cluster(edo) 
outreg2 using "h1.tex", keep(timb3) dec(5) onecol se br symbol(***, **, *) replace

*Table 3: H3
reg particularistic c.treat42#c.reform i.edo i.date2,robust cluster(edo) 
outreg2 using "h3.tex", keep(c.treat42#c.reform) dec(5) onecol se br symbol(***, **, *) replace

*Figure 5: parallel trends for H1 
local vars "particularistic"
local treat "rm301 rm302 rm303 rm304 rm305 rm306 rm307 rm308 rm309 rm310 rm311 rm312 rm201 rm202 rm203 rm204 rm205 rm206 rm207 rm208 rm209 rm210 rm211 rm212 rm101 rm102 rm103 rm104 rm105 rm106 rm107 rm108 rm109"
foreach x in `vars'{
foreach y in `treat'{
reg `x' c.treat4#c.`y' i.date2 i.edo,robust cluster(edo) 
estimate store `x'_`y'
}
}
reg particularistic c.treat4#c.reform i.edo i.date2,robust cluster(edo) 
estimate store didh1
coefplot (particularistic_rm301, color(*.8)) (particularistic_rm302, color(*.8)) (particularistic_rm303, color(*.8)) (particularistic_rm304, color(*.8)) (particularistic_rm305, color(*.8)) (particularistic_rm306, color(*.8)) (particularistic_rm307, color(*.8)) (particularistic_rm308, color(*.8)) (particularistic_rm309, color(*.8)) (particularistic_rm310, color(*.8)) (particularistic_rm311, color(*.8)) (particularistic_rm312, color(*.8)) (particularistic_rm201, color(*.8)) (particularistic_rm202, color(*.8)) (particularistic_rm203, color(*.8)) (particularistic_rm204, color(*.8)) (particularistic_rm205, color(*.8)) (particularistic_rm206, color(*.8)) (particularistic_rm207, color(*.8)) (particularistic_rm208, color(*.8)) (particularistic_rm209, color(*.8)) (particularistic_rm210, color(*.8)) (particularistic_rm211, color(*.8)) (particularistic_rm212, color(*.8)) (particularistic_rm101, color(*.8)) (particularistic_rm102, color(*.8)) (particularistic_rm103, color(*.8)) (particularistic_rm104, color(*.8)) (particularistic_rm105, color(*.8)) (particularistic_rm106, color(*.8)) (particularistic_rm107, color(*.8)) (particularistic_rm108, color(*.8)) (particularistic_rm109, color(*.8)) (didh1, color(*.8)), vertical keep(c.treat4#c.rm301 c.treat4#c.rm302 c.treat4#c.rm303 c.treat4#c.rm304 c.treat4#c.rm305 c.treat4#c.rm306 c.treat4#c.rm307 c.treat4#c.rm308 c.treat4#c.rm309 c.treat4#c.rm310 c.treat4#c.rm311 c.treat4#c.rm312 c.treat4#c.rm201 c.treat4#c.rm202 c.treat4#c.rm203 c.treat4#c.rm204 c.treat4#c.rm205 c.treat4#c.rm206 c.treat4#c.rm207 c.treat4#c.rm208 c.treat4#c.rm209 c.treat4#c.rm210 c.treat4#c.rm211 c.treat4#c.rm212 c.treat4#c.rm101 c.treat4#c.rm102 c.treat4#c.rm103 c.treat4#c.rm104 c.treat4#c.rm105 c.treat4#c.rm106 c.treat4#c.rm107 c.treat4#c.rm108 c.treat4#c.rm109 c.treat4#c.reform) yline(0) xline(34) legend(off) graphregion(col(white)) bgcol(white) name(p)
graph drop _all

*Figure 7: parallel trends for H3
local vars "particularistic"
local treat "rm301 rm302 rm303 rm304 rm305 rm306 rm307 rm308 rm309 rm310 rm311 rm312 rm201 rm202 rm203 rm204 rm205 rm206 rm207 rm208 rm209 rm210 rm211 rm212 rm101 rm102 rm103 rm104 rm105 rm106 rm107 rm108 rm109"
foreach x in `vars'{
foreach y in `treat'{
reg `x' c.treat42#c.`y' i.date2 i.edo,robust cluster(edo) 
estimate store `x'_`y'
}
}
reg particularistic c.treat42#c.reform i.edo i.date2,robust cluster(edo) 
estimate store didh3
coefplot (particularistic_rm301, color(*.8)) (particularistic_rm302, color(*.8)) (particularistic_rm303, color(*.8)) (particularistic_rm304, color(*.8)) (particularistic_rm305, color(*.8)) (particularistic_rm306, color(*.8)) (particularistic_rm307, color(*.8)) (particularistic_rm308, color(*.8)) (particularistic_rm309, color(*.8)) (particularistic_rm310, color(*.8)) (particularistic_rm311, color(*.8)) (particularistic_rm312, color(*.8)) (particularistic_rm201, color(*.8)) (particularistic_rm202, color(*.8)) (particularistic_rm203, color(*.8)) (particularistic_rm204, color(*.8)) (particularistic_rm205, color(*.8)) (particularistic_rm206, color(*.8)) (particularistic_rm207, color(*.8)) (particularistic_rm208, color(*.8)) (particularistic_rm209, color(*.8)) (particularistic_rm210, color(*.8)) (particularistic_rm211, color(*.8)) (particularistic_rm212, color(*.8)) (particularistic_rm101, color(*.8)) (particularistic_rm102, color(*.8)) (particularistic_rm103, color(*.8)) (particularistic_rm104, color(*.8)) (particularistic_rm105, color(*.8)) (particularistic_rm106, color(*.8)) (particularistic_rm107, color(*.8)) (particularistic_rm108, color(*.8)) (particularistic_rm109, color(*.8)) (didh3, color(*.8)), vertical keep(c.treat42#c.rm301 c.treat42#c.rm302 c.treat42#c.rm303 c.treat42#c.rm304 c.treat42#c.rm305 c.treat42#c.rm306 c.treat42#c.rm307 c.treat42#c.rm308 c.treat42#c.rm309 c.treat42#c.rm310 c.treat42#c.rm311 c.treat42#c.rm312 c.treat42#c.rm201 c.treat42#c.rm202 c.treat42#c.rm203 c.treat42#c.rm204 c.treat42#c.rm205 c.treat42#c.rm206 c.treat42#c.rm207 c.treat42#c.rm208 c.treat42#c.rm209 c.treat42#c.rm210 c.treat42#c.rm211 c.treat42#c.rm212 c.treat42#c.rm101 c.treat42#c.rm102 c.treat42#c.rm103 c.treat42#c.rm104 c.treat42#c.rm105 c.treat42#c.rm106 c.treat42#c.rm107 c.treat42#c.rm108 c.treat42#c.rm109 c.treat42#c.reform) yline(0) xline(34) legend(off) graphregion(col(white)) bgcol(white) name(p)
graph drop _all

*Figure 6: H2
local vars "particularistic"
foreach x in `vars'{
reg `x' timb3 i.edo i.date2 if timinga==1,  robust cluster(edo) 
estimate store tim_`x'
reg `x' timb3 i.edo i.date2 if timingb==1,  robust cluster(edo) 
estimate store tim2_`x'
reg `x' timb3 i.edo i.date2 if timingc==1,  robust cluster(edo) 
estimate store tim3_`x'
}
coefplot (tim_particularistic)  (tim2_particularistic) (tim3_particularistic), vertical keep(timb3 timb3 timb3) levels(95 90) yline(0) coeflabels(timb3="Election (t)	 		t+1 			t+2") legend(off) graphregion(col(white)) bgcol(white) name(d)
graph drop _all

***Tables and Figures in CTM***
*Figure 2 in R code replication_cleaning_CTM line 375

*Figure 3 
collapse (mean) salud education farming v192, by(state realyear)
rename state estado
rename realyear year
merge 1:1 estado year using efipem_edo.dta
drop _merge
merge 1:1 estado year using conapo_poblacion.dta
drop _merge
sort estado year
replace farming=. if farming>.04 
replace pop_i=pop_i[_n-1] if year==2016|year==2017

gen gedu=lpedu/pop_i
gen gsalud=lpsalud/pop_i
gen gfar=lpagrop/pop_i
gen gtur=lptur/pop_i


twoway (scatter farming gfar if farming<.04 & gfar<550)(lfit farming gfar if farming<.04 & gfar<550), ytitle("Mean Proportion of Discussions") title("Mean Proportion of Discussions") xtitle("Spending on Agriculture, Farming and Fishing (per capita)") title("Agriculture, Farming and Fishing") legend(off) graphregion(col(white)) bgcol(white) name(g0)
twoway (scatter education gedu)(lfit education gedu), ytitle("Mean Proportion of Discussions") title("Education") xtitle("Spending on Education (per capita)")  legend(off) graphregion(col(white)) bgcol(white) name(g1)
twoway (scatter v192 gtur if gtur<200)(lfit v192 gtur if gtur<200),  ytitle("Mean Proportion of Discussions") xtitle("Spending on Tourism (per capita)") title("Tourism")  legend(off) graphregion(col(white)) bgcol(white) name(g2)
twoway (scatter salud gsalud if gsalud>500)(lfit salud gsalud if gsalud>500),  ytitle("Mean Proportion of Discussions") xtitle("Spending on Health (per capita)")  title("Health")  legend(off) graphregion(col(white)) bgcol(white) name(g3)
graph combine g0 g1 g2 g3
graph drop _all

