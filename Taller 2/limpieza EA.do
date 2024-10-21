********************************************************************************
***	LIMPIEZA DE DATOS DE TALLER 2 - ESTEBAN ALVAREZ                          ***
********************************************************************************

clear all
macro drop _all
set more off

cd "C:\Users\Esteban\Dropbox\Estudios\Big Data y Machine Learning\Talleres\Taller 2"

import delimited "test_personas.csv", clear
global vars
foreach x of varlist *{
	global vars ${vars} `x'
}
rename p6020 hombre
replace hombre=0 if hombre==2
rename p6040 edad
rename p6050 parentesco
gen jefe=(parentesco==1)
gen conyuge=(parentesco==2)
rename p6090 seg_social
rename p6100 regimen
gen regimen_cont=(regimen=="1")
gen regimen_sub=(regimen=="3")
gen regimen_esp=(regimen=="2")
drop regimen
rename p6210 nivel_educativo
rename p6240 actividad 
* oficio
rename p6426 meses_empresa 
rename p6430 ocupacion 
rename p6510 ing_horasextra
rename p6545 primas
rename p6580 bonificaciones
rename p6585s1 sub_alimentacion
rename p6585s2 sub_transporte
rename p6585s3 sub_familiar
rename p6585s4 sub_educativo
rename p6590 salario_alimentos
rename p6600 salario_vivienda
rename p6610 transporte_empresa
rename p6620 salario_especie
rename p6630s1 prima12
rename p6630s2 prima_navidad12
rename p6630s3 prima_vacaciones12
rename p6630s4 viaticos12
rename p6630s6 bonificaciones12
rename p6800 horas_trabajadas 
rename p6870 tamano_empresa
rename p6920 cot_pension
gen pensionado=(cot_pension=="3")
replace cot_pension="0" if cot_pension=="3" | cot_pension=="NA" | cot_pension=="2"
destring cot_pension, replace
rename p7040 otro_trabajo_negocio
rename p7045 horas_trabajo_2
rename p7050 actividad2 
rename p7090 mas_horas
rename p7110 intenta_mas_horas
rename p7120 disp_mas_horas
rename p7150 int_cambio_trabajo
rename p7160 cambio_mes
rename p7310 primer_trabajo
rename p7350 ultimo_trabajo_des // revisar
rename p7422 ingresos_des // revisar p7472
rename p7495 ing_arr_pen // pension = menos las otras 2
rename p7500s2 ing_pension
rename p7500s3 ing_separacion
rename p7505 ing_otros
rename p7510s1 ing_per_pais
rename p7510s2 ing_per_ext
rename p7510s3 ing_inst
rename p7510s5 ing_fin
rename p7510s6 ing_cesantias
rename p7510s7 ing_dif

foreach x in seg_social ing_horasextra primas bonificaciones sub_alimentacion sub_transporte sub_familiar sub_educativo salario_alimentos salario_vivienda transporte_empresa salario_especie prima12 prima_navidad12 prima_vacaciones12 viaticos12 bonificaciones12 otro_trabajo_negocio mas_horas intenta_mas_horas disp_mas_horas int_cambio_trabajo cambio_mes ingresos_des ing_arr_pen ing_pension ing_separacion ing_otros ing_per_pais ing_per_ext ing_inst ing_fin ing_cesantias ing_dif  primer_trabajo pet oc des ina meses_empresa horas_trabajadas horas_trabajo_2 p7472{
	replace `x'="0" if `x'=="2" | `x'=="9" | `x'=="NA"
	destring `x', replace
}

drop dominio clase orden actividad actividad2 ultimo_trabajo_des
* Arreglos
replace oficio="" if jefe!=1 | oficio=="NA"
destring oficio, replace
gen jefe_mujer=(jefe==1 & hombre==0)
sum edad, d
gen tercera_edad=(edad>=60)
gen jefe_tedad=(jefe==1 & tercera_edad==1)
foreach x in regimen_cont regimen_esp regimen_sub seg_social{
	replace `x'=. if jefe==0
}
sum meses_empresa,d
replace meses_empresa=r(p99) if meses_empresa>r(p99) & meses_empresa!=.

gen adulto=(edad>=18)
gen menor=(edad<18)
gen menor14=(edad<=14)
gen adul_trabaja=(oc==1 & edad>=18)
gen menor_trabajando=(oc==1 & edad<18)

gen ne_ninguno=(nivel_educativo=="1")
gen ne_preescolar=(nivel_educativo=="2")
gen ne_basica=(nivel_educativo=="3")
gen ne_secundaria=(nivel_educativo=="4")
gen ne_media=(nivel_educativo=="5")
gen ne_superior=(nivel_educativo=="6")
drop nivel_educativo p6210s1

gen empleado=(ocupacion=="1" | ocupacion=="2")
gen domestico=(ocupacion=="3")
gen independiente=(ocupacion=="4")
gen patron=(ocupacion=="5")
gen sin_remuneracion=(ocupacion=="6" | ocupacion=="7")
gen jornalero=(ocupacion=="8")
drop ocupacion

gen tamano1=(tamano_empresa=="1" | tamano_empresa=="2" | tamano_empresa=="3" | tamano_empresa=="4")
gen tamano2=(tamano_empresa=="5" | tamano_empresa=="6" | tamano_empresa=="7")
gen tamano3=(tamano_empresa=="8" | tamano_empresa=="9")
drop tamano_empresa

* Esto solo para el jefe y la conjugue
gen salario_extra=(bonificaciones==1 | sub_alimentacion==1 | sub_transporte==1 | sub_familiar==1 | sub_educativo==1 | salario_alimentos==1 | salario_vivienda==1 | transporte_empresa==1 | salario_especie==1)
gen ingreso_extra=(ing_otros==1 | ing_per_pais==1 | ing_per_ext==1 | ing_inst==1 | ing_fin==1 | ing_cesantias==1 | ing_dif==1 | ing_arr_pen==1 | ing_pension==1 | ing_separacion==1)

foreach x in seg_social ing_horasextra primas bonificaciones sub_alimentacion sub_transporte sub_familiar sub_educativo salario_alimentos salario_vivienda transporte_empresa salario_especie prima12 prima_navidad12 prima_vacaciones12 viaticos12 bonificaciones12 otro_trabajo_negocio mas_horas intenta_mas_horas disp_mas_horas int_cambio_trabajo cambio_mes ingresos_des ing_arr_pen ing_pension ing_separacion ing_otros ing_per_pais ing_per_ext ing_inst ing_fin ing_cesantias ing_dif primer_trabajo ne_ninguno ne_preescolar ne_basica ne_secundaria ne_media ne_superior empleado domestico independiente sin_remuneracion jornalero tamano1 tamano2 tamano3 edad cot_pension meses_empresa salario_extra horas_trabajadas ingreso_extra{
	replace `x'=. if parentesco!=1 & parentesco!=2
	gen `x'_j=`x' if jefe==1
}

collapse (sum) adulto menor menor14 adul_trabaja (mean) edad* meses_empresa* horas_trabajadas* (max) seg_social* oficio ing_horasextra* primas* bonificaciones* sub_alimentacion* sub_transporte* sub_familiar* sub_educativo* salario_alimentos* salario_vivienda* transporte_empresa* salario_especie* prima12* prima_navidad12* prima_vacaciones12* viaticos12* salario_extra* cot_pension* otro_trabajo_negocio* mas_horas* intenta_mas_horas* disp_mas_horas* int_cambio_trabajo* primer_trabajo* ing_arr_pen* ing_pension* ing_separacion* ing_otros* ing_per_pais* ing_per_ext* ing_inst* ing_fin* ing_cesantias* ing_dif* ne_ninguno* ne_preescolar* ne_basica* ne_secundaria* ne_media* ne_superior* empleado* domestico* independiente* sin_remuneracion* jornalero* tamano1* tamano2* tamano3* menor_trabajando jefe_tedad tercera_edad ingreso_extra (max) jefe_mujer, by(id)

foreach x of varlist _all{
	label var `x' ""
}

save "data_test", replace

import delimited "train_personas.csv", clear
drop if dominio=="BOGOTA" // se elimina porque no hay en el test
keep $vars
rename p6020 hombre
replace hombre=0 if hombre==2
rename p6040 edad
rename p6050 parentesco
gen jefe=(parentesco==1)
gen conyuge=(parentesco==2)
rename p6090 seg_social
rename p6100 regimen
gen regimen_cont=(regimen=="1")
gen regimen_sub=(regimen=="3")
gen regimen_esp=(regimen=="2")
drop regimen
rename p6210 nivel_educativo
rename p6240 actividad 
* oficio
rename p6426 meses_empresa 
rename p6430 ocupacion 
rename p6510 ing_horasextra
rename p6545 primas
rename p6580 bonificaciones
rename p6585s1 sub_alimentacion
rename p6585s2 sub_transporte
rename p6585s3 sub_familiar
rename p6585s4 sub_educativo
rename p6590 salario_alimentos
rename p6600 salario_vivienda
rename p6610 transporte_empresa
rename p6620 salario_especie
rename p6630s1 prima12
rename p6630s2 prima_navidad12
rename p6630s3 prima_vacaciones12
rename p6630s4 viaticos12
rename p6630s6 bonificaciones12
rename p6800 horas_trabajadas 
rename p6870 tamano_empresa
rename p6920 cot_pension
gen pensionado=(cot_pension=="3")
replace cot_pension="0" if cot_pension=="3" | cot_pension=="NA" | cot_pension=="2"
destring cot_pension, replace
rename p7040 otro_trabajo_negocio
rename p7045 horas_trabajo_2
rename p7050 actividad2 
rename p7090 mas_horas
rename p7110 intenta_mas_horas
rename p7120 disp_mas_horas
rename p7150 int_cambio_trabajo
rename p7160 cambio_mes
rename p7310 primer_trabajo
rename p7350 ultimo_trabajo_des // revisar
rename p7422 ingresos_des // revisar p7472
rename p7495 ing_arr_pen // pension = menos las otras 2
rename p7500s2 ing_pension
rename p7500s3 ing_separacion
rename p7505 ing_otros
rename p7510s1 ing_per_pais
rename p7510s2 ing_per_ext
rename p7510s3 ing_inst
rename p7510s5 ing_fin
rename p7510s6 ing_cesantias
rename p7510s7 ing_dif

foreach x in seg_social ing_horasextra primas bonificaciones sub_alimentacion sub_transporte sub_familiar sub_educativo salario_alimentos salario_vivienda transporte_empresa salario_especie prima12 prima_navidad12 prima_vacaciones12 viaticos12 bonificaciones12 otro_trabajo_negocio mas_horas intenta_mas_horas disp_mas_horas int_cambio_trabajo cambio_mes ingresos_des ing_arr_pen ing_pension ing_separacion ing_otros ing_per_pais ing_per_ext ing_inst ing_fin ing_cesantias ing_dif  primer_trabajo pet oc des ina meses_empresa horas_trabajadas horas_trabajo_2 p7472{
	replace `x'="0" if `x'=="2" | `x'=="9" | `x'=="NA"
	destring `x', replace
}

drop dominio clase orden actividad actividad2 ultimo_trabajo_des
* Arreglos
replace oficio="" if jefe!=1 | oficio=="NA"
destring oficio, replace
gen jefe_mujer=(jefe==1 & hombre==0)
sum edad, d
gen tercera_edad=(edad>=60)
gen jefe_tedad=(jefe==1 & tercera_edad==1)
foreach x in regimen_cont regimen_esp regimen_sub seg_social{
	replace `x'=. if jefe==0
}
sum meses_empresa,d
replace meses_empresa=r(p99) if meses_empresa>r(p99) & meses_empresa!=.

gen adulto=(edad>=18)
gen menor=(edad<18)
gen menor14=(edad<=14)
gen adul_trabaja=(oc==1 & edad>=18)
gen menor_trabajando=(oc==1 & edad<18)

gen ne_ninguno=(nivel_educativo=="1")
gen ne_preescolar=(nivel_educativo=="2")
gen ne_basica=(nivel_educativo=="3")
gen ne_secundaria=(nivel_educativo=="4")
gen ne_media=(nivel_educativo=="5")
gen ne_superior=(nivel_educativo=="6")
drop nivel_educativo p6210s1

gen empleado=(ocupacion=="1" | ocupacion=="2")
gen domestico=(ocupacion=="3")
gen independiente=(ocupacion=="4")
gen patron=(ocupacion=="5")
gen sin_remuneracion=(ocupacion=="6" | ocupacion=="7")
gen jornalero=(ocupacion=="8")
drop ocupacion

gen tamano1=(tamano_empresa=="1" | tamano_empresa=="2" | tamano_empresa=="3" | tamano_empresa=="4")
gen tamano2=(tamano_empresa=="5" | tamano_empresa=="6" | tamano_empresa=="7")
gen tamano3=(tamano_empresa=="8" | tamano_empresa=="9")
drop tamano_empresa

* Esto solo para el jefe y la conjugue
gen salario_extra=(bonificaciones==1 | sub_alimentacion==1 | sub_transporte==1 | sub_familiar==1 | sub_educativo==1 | salario_alimentos==1 | salario_vivienda==1 | transporte_empresa==1 | salario_especie==1)
gen ingreso_extra=(ing_otros==1 | ing_per_pais==1 | ing_per_ext==1 | ing_inst==1 | ing_fin==1 | ing_cesantias==1 | ing_dif==1 | ing_arr_pen==1 | ing_pension==1 | ing_separacion==1)

foreach x in seg_social ing_horasextra primas bonificaciones sub_alimentacion sub_transporte sub_familiar sub_educativo salario_alimentos salario_vivienda transporte_empresa salario_especie prima12 prima_navidad12 prima_vacaciones12 viaticos12 bonificaciones12 otro_trabajo_negocio mas_horas intenta_mas_horas disp_mas_horas int_cambio_trabajo cambio_mes ingresos_des ing_arr_pen ing_pension ing_separacion ing_otros ing_per_pais ing_per_ext ing_inst ing_fin ing_cesantias ing_dif primer_trabajo ne_ninguno ne_preescolar ne_basica ne_secundaria ne_media ne_superior empleado domestico independiente sin_remuneracion jornalero tamano1 tamano2 tamano3 edad cot_pension meses_empresa salario_extra horas_trabajadas ingreso_extra{
	replace `x'=. if parentesco!=1 & parentesco!=2
	gen `x'_j=`x' if jefe==1
}


collapse (sum) adulto menor menor14 adul_trabaja (mean) edad* meses_empresa* horas_trabajadas* (max) seg_social* oficio ing_horasextra* primas* bonificaciones* sub_alimentacion* sub_transporte* sub_familiar* sub_educativo* salario_alimentos* salario_vivienda* transporte_empresa* salario_especie* prima12* prima_navidad12* prima_vacaciones12* viaticos12* salario_extra* cot_pension* otro_trabajo_negocio* mas_horas* intenta_mas_horas* disp_mas_horas* int_cambio_trabajo* primer_trabajo* ing_arr_pen* ing_pension* ing_separacion* ing_otros* ing_per_pais* ing_per_ext* ing_inst* ing_fin* ing_cesantias* ing_dif* ne_ninguno* ne_preescolar* ne_basica* ne_secundaria* ne_media* ne_superior* empleado* domestico* independiente* sin_remuneracion* jornalero* tamano1* tamano2* tamano3* menor_trabajando jefe_tedad tercera_edad ingreso_extra (max) jefe_mujer, by(id)

foreach x of varlist _all{
	label var `x' ""
}

save "data_train", replace
*******
import delimited "test_hogares.csv", clear
global vars
foreach x of varlist *{
	global vars ${vars} `x'
}
* clase = rural-urbano
rename p5000 cuartos
rename p5010 dormitorios
rename p5090 tipo_vivienda 
rename p5100 cuota_amort
rename p5130 arriendo_est
rename p5140 arriendo

foreach x in cuota_amort arriendo_est arriendo{
	replace `x'="" if `x'=="NA"
	destring `x', replace
}

gen vivienda_pagada=(tipo_vivienda==1)
gen vivienda_pagando=(tipo_vivienda==2)
gen vivienda_arriendo=(tipo_vivienda==3)
gen vivienda_usufructo=(tipo_vivienda==4)
gen vivienda_otro=(tipo_vivienda==5 | tipo_vivienda==6)
drop tipo_vivienda
replace arriendo=arriendo_est if arriendo==.
gen urbano=(clase==1)
drop clase
* el encode puede crear numeros distintos en cada base
* li y lp varían por municipio
encode dominio, gen(ciudad)
preserve
	keep dominio ciudad
	gduplicates drop
	tempfile ciudad
	save `ciudad'
restore
drop dominio
sum nper npersug
drop npersug
gen per_hab=nper/dormitorios
merge 1:1 id using "data_test", nogen
save "data_test", replace

*
import delimited "train_hogares.csv", clear
keep $vars pobre
drop if dominio=="BOGOTA"
rename p5000 cuartos
rename p5010 dormitorios
rename p5090 tipo_vivienda 
rename p5100 cuota_amort
rename p5130 arriendo_est
rename p5140 arriendo

foreach x in cuota_amort arriendo_est arriendo{
	replace `x'="" if `x'=="NA"
	destring `x', replace
}

gen vivienda_pagada=(tipo_vivienda==1)
gen vivienda_pagando=(tipo_vivienda==2)
gen vivienda_arriendo=(tipo_vivienda==3)
gen vivienda_usufructo=(tipo_vivienda==4)
gen vivienda_otro=(tipo_vivienda==5 | tipo_vivienda==6)
drop tipo_vivienda
replace arriendo=arriendo_est if arriendo==.
gen urbano=(clase==1)
drop clase
* li y lp varían por municipio
* el encode puede crear numeros distintos en cada base
*encode dominio, gen(ciudad)
merge m:1 dominio using `ciudad', nogen
drop dominio
sum nper npersug
drop npersug
gen per_hab=nper/dormitorios
merge 1:1 id using "data_train", nogen keep(3) 
save "data_train", replace

