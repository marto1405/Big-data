## require/install packages on this session
install.packages("rvest")
#####Analisis descriptivo


if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       rvest,
       caret,
       boot,
       mice)

rm(list = ls())

# Crear un vector con los nombres de las variables
Nombre <- c("id",
  "Directorio", "Secuencia_p", "Orden", "Clase", "Dominio", "Mes", "Estrato1",
  "P6020", "P6040", "P6050", "P6090", "P6100", "P6210", "P6210s1", "P6240", "Oficio", 
  "P6426", "P6430", "P6500", "P6510", "P6510s1", "P6510s2", "P6545", "P6545s1", 
  "P6545s2", "P6580", "P6580s1", "P6580s2", "P6585s1", "P6585s1a1", "P6585s1a2", 
  "P6585s2", "P6585s2a1", "P6585s2a2", "P6585s3", "P6585s3a1", "P6585s3a2", 
  "P6585s4", "P6585s4a1", "P6585s4a2", "P6590", "P6590s1", "P6600", "P6600s1", 
  "P6610", "P6610s1", "P6620", "P6620s1", "P6630s1", "P6630s1a1", "P6630s2", 
  "P6630s2a1", "P6630s3", "P6630s3a1", "P6630s4", "P6630s4a1", "P6630s6", 
  "P6630s6a1", "P6750", "P6760", "P550", "P6800", "P6870", "P6920", "P7040", 
  "P7045", "P7050", "P7070", "P7090", "P7110", "P7120", "P7140s1", "P7140s2", 
  "P7150", "P7160", "P7310", "P7350", "P7422", "P7422s1", "P7472", "P7472s1", 
  "P7495", "P7500s1", "P7500s1a1", "P7500s2", "P7500s2a1", "P7500s3", "P7500s3a1", 
  "P7505", "P7510s1", "P7510s1a1", "P7510s2", "P7510s2a1", "P7510s3", "P7510s3a1", 
  "P7510s5", "P7510s5a1", "P7510s6", "P7510s6a1", "P7510s7", "P7510s7a1", "Pet", 
  "Oc", "Des", "Ina", "Impa", "Isa", "Ie", "Imdi", "Iof1", "Iof2", "Iof3h", "Iof3i", 
  "Iof6", "Cclasnr2", "Cclasnr3", "Cclasnr4", "Cclasnr5", "Cclasnr6", "Cclasnr7", 
  "Cclasnr8", "Cclasnr11", "Impaes", "Isaes", "Iees", "Imdies", "Iof1es", "Iof2es", 
  "Iof3hes", "Iof3ies", "Iof6es", "Ingtotob", "Ingtotes", "Ingtot", "Fex_c", "Depto", 
  "Fex_dpto"
)


# Crear un vector con las preguntas
Descripcion <- c("id del hogar",
  "Llave de vivienda",
  "Llave de hogar",
  "Llave de persona",
  "1. Cabecera, 2. Resto (centros poblados y área rural dispersa)",
  "Cada una de las 24 a.M., otras cabeceras y resto",
  "Mes",
  "Estrato de energía para las 13 a.M., y sextil de icv para otras cabeceras y resto",
  "Sexo",
  "¿cuántos años cumplidos tiene?",
  "¿cuál es el parentesco de ...Con el jefe o jefa del hogar?",
  "¿ ... Está afiliado, es cotizante o es beneficiario de alguna entidad de seguridad social en salud?",
  "¿A cual de los siguientes regímenes de seguridad social en salud está afiliado:",
  "¿Cuál es el nivel educativo más alto alcanzado por .... y el último año o grado aprobado en este nivel?",
  "Grado escolar aprobado",
  "¿En que actividad ocupó...... la mayor parte del tiempo la semana pasada?",
  "¿qué hace……en este trabajo?",
  "¿cuanto tiempo lleva ... Trabajando en esta empresa, negocio, industria, oficina, firma o finca de manera continua?",
  "En este trabajo es …. (posición ocupacional primera actividad)",
  "Antes de descuentos ¿cuánto ganó .... el mes pasado en este empleo?",
  "¿el mes pasado recibió ingresos por concepto de horas extras?",
  "¿cuánto recibió por horas extras?",
  "¿incluyó este valor en los ingresos del mes pasado?",
  "El mes pasado recibió a. Primas (técnica, de antigüedad, clima,orden publico, otras, etc)",
  "¿cuánto recibió por primas?",
  "¿incluyó este valor en los ingresos del mes pasado ($ ____) que me declaró anteriormente?",
  "¿el mes pasado recibió b. Bonificaciones?",
  "¿cuánto recibió por bonificaciones?",
  "¿incluyó este valor en los ingresos del mes pasado ($ ____) que me declaró anteriormente?",
  "¿el mes pasado recibió a. Auxilio o subsidio de alimentación?",
  "¿cuánto recibió por subsidio de alimentación?",
  "¿incluyó este valor en los ingresos del mes pasado ($ ____) que me declaró anteriormente?",
  "¿el mes pasado recibió b. Auxilio subsidio de transporte?",
  "¿cuánto recibió por subsidio de transporte?",
  "¿incluyó este valor en los ingresos del mes pasado ($ ____) que me declaró anteriormente?",
  "¿el mes pasado recibió c. Subsidio familiar?",
  "¿cuánto recibió por subsidio familiar?",
  "¿incluyó este valor en los ingresos del mes pasado ($ ____) que me declaró anteriormente?",
  "¿el mes pasado recibió d. Subsidio educativo?",
  "¿cuánto recibió por subsidio educativo?",
  "¿incluyó este valor en los ingresos del mes pasado ($ ____) que me declaró anteriormente?",
  "¿además del salario en dinero,¿el mes pasado recibió alimentos como parte de pago por su trabajo?",
  "¿en cuánto estima lo que recibió? $____________",
  "¿además del salario en dinero,¿el mes pasado recibió vivienda como parte de pago por su trabajo?",
  "¿en cuánto estima lo que recibió? $_______________",
  "¿normalmente... Utiliza transporte de la empresa para desplazarse a su trabajo (bus o automóvil)?",
  "¿en cuánto estima lo que recibió?$_______________",
  "Además del salario en dinero, ¿el mes pasado... Recibió otros ingresos en especie por su trabajo (electrodomésticos,ropa, productos diferentes a alimentos o bonos tipo sodexho)?",
  "¿en cuánto estima lo que recibió? $_______________",
  "En los últimos 12 meses recibió … a. Prima de servicios",
  "¿cuánto recibió? $____________",
  "En los últimos 12 meses recibió ... B. Prima de navidad",
  "¿cuánto recibió? $____________",
  "En los últimos 12 meses recibió … c. Prima de vacaciones",
  "¿cuánto recibió? $____________",
  "En los últimos 12 meses recibió ... D. Viáticos permanentes",
  "¿cuánto recibió? $____________",
  "En los últimos 12 meses recibió … e. Bonificaciones anuales",
  "¿cuánto recibió? $____________",
  "¿cuál fue la ganancia neta o los honorarios netos de ... En esa actividad, negocio, profesión o finca, el mes pasado?",
  "¿ a cuántos meses corresponde lo que recibió?",
  "¿cuál fue la ganancia neta del negocio o de la cosecha durante los últimos doce meses? (sólo para centros poblados y área rural dispersa)",
  "¿cuántas horas a la semana trabaja normalmente.... en ese trabajo?",
  "¿cuántas personas en total tiene la empresa, negocio, industria, oficina, firma, finca o sitio donde ..... Trabaja?",
  "¿está... Cotizando actualmente a un fondo de pensiones?",
  "Además de la ocupación principal, ¿.... tenía la semana pasada otro trabajo o negocio?",
  "¿cuántas horas trabajó ... La semana pasada en ese segundo trabajo?",
  "En ese segundo trabajo…..es: (ocupación segunda actividad)",
  "¿cuánto recibió o ganó ….. el mes pasado en ese segundo trabajo o negocio?",
  "Además de las horas que trabaja actualmente ¿...... quiere trabajar más horas?",
  "Durante las últimas 4 semanas, ¿ ...... hizo diligencias para trabajar más horas?",
  "Si la semana pasada le hubiera resultado la posibilidad de trabajar más horas ¿ estaba...... disponible para hacerlo?",
  "¿Por que motivos ....... desea cambiar de trabajo o empleo: a. Para mejorar la utilización de sus capacidades o formación?",
  "¿Por que motivos ....... desea cambiar de trabajo o empleo: b. Desea mejorar sus ingresos?",
  "Durante las ÚLTIMAS 4 SEMANAS, ¿...... hizo diligencias para cambiar de trabajo?",
  "Si le resultara un nuevo trabajo o empleo a...¿podría empezar a desempeñarlo antes de un mes?",
  "¿...... ha buscado trabajo por primera vez o había trabajado antes por lo menos durante dos semanas consecutivas?",
  "En este último trabajo era: … (Desocupados)",
  "¿Recibió o ganó el mes pasado ingresos por concepto de trabajo?. (Desocupados)",
  "¿Cuánto? $_________________",
  "¿recibió o ganó el mes pasado ingresos por concepto de trabajo?. (desocupados)",
  "¿cuánto? $_________________",
  "El mes pasado, ¿recibió pagos por concepto de arriendos y/o pensiones?",
  "¿El mes pasado, recibió pagos por: a. arriendos de casas, apartamentos, fincas, lotes, vehículos, equipos etc?",
  "Valor mes pasado $ __________________",
  "¿El mes pasado recibió pagos por b. pensiones o jubilaciones por vejez,invalidez o sustitución pensional ?",
  "Valor mes pasado $ __________________",
  "¿El mes pasado recibió pagos por c. pensión alimenticia por paternidad, divorcio o separación?",
  "Valor mes pasado $ __________________",
  "Durante los últimos doce meses, ¿recibió dinero de otros hogares, personas o instituciones no gubernamentales; dinero por intereses, dividendos, utilidades o por cesantias?",
  "Durante los últimos 12 meses, ¿recibió a. dinero de otros hogares o personas residentes en el país?",
  "Valor $ ___________",
  "Durante los últimos 12 meses, ¿recibió b. dinero de otros hogares o personas residentes fuera del país?",
  "Valor $ ___________",
  "Durante los últimos 12 meses, ¿recibió c. ayudas en dinero de instituciones del país?",
  "Valor $ ___________",
  "Durante los últimos 12 meses, ¿recibió d. dinero por intereses de prestamos o CDT´s, depositos de ahorros, utilidades, ganancias o dividendos por inversiones?",
  "Valor $ ___________",
  "Durante los últimos 12 meses, ¿recibió e. dinero por concepto de cesantías y/o intereses a las cesantías?",
  "Valor $ ___________",
  "Durante los últimos 12 meses, ¿recibió f. dinero de otras fuentes diferentes a las anteriores?",
  "Valor $ ___________",
  "Población en edad de trabajar 1: sí 0: no",
  "Ocupado 1: sí",
  "Desocupado 1: sí",
  "Inactivo 1: sí",
  "Ingreso monetario de la primera actividad antes de imputación",
  "Ingreso monetario de la segunda actividad antes de imputación",
  "Ingreso en especie antes de imputación",
  "Ingreso por trabajo de desocupados e inactivos antes de imputación",
  "Ingreso por intereses y dividendos antes de imputación",
  "Ingreso por jubilaciones y pensiones antes de imputación",
  "Ingreso por ayudas de hogares, antes de imputación",
  "Ingreso por ayudas de instituciones, antes de imputación",
  "Ingreso por arriendos antes de imputación",
  "Estado de impa 1:faltante 0: observado",
  "Estado de isa 1:faltante 0: observado",
  "Estado de ie 1:faltante 0: observado",
  "Estado de imdi 1:faltante 0: observado",
  "Estado de iof1 1:faltante 0: observado",
  "Estado de iof2 1:faltante 0: observado",
  "Estado de iof3 1:faltante 0: observado",
  "Estado de iof6 1:faltante 0: observado",
  "Ingreso monetario de la primera actividad imputado (sólo para faltantes, extremos o ceros inconsistentes)",
  "Ingreso monetario de la segunda actividad imputado (sólo para faltantes o extremos)",
  "Ingreso en especie imputado (sólo para faltantes o extremos)",
  "Ingreso por trabajo de desocupados e inactivos imputado (sólo para faltantes o extremos)",
  "Ingreso por intereses y dividendos imputado (sólo para faltantes o extremos)",
  "Ingreso por jubilaciones y pensiones imputado (sólo para faltantes o extremos)",
  "Ingreso por ayudas de hogares, imputado (sólo para faltantes o extremos)",
  "Ingreso por ayudas de instituciones, imputado (sólo para faltantes o extremos)",
  "Ingreso por arriendos imputado (sólo para faltantes o extremos)",
  "Ingreso total observado",
  "Ingreso total imputado",
  "Ingreso total",
  "Factor de expansión anualizado",
  "Departamento",
  "Factor Expansión departamental"
)

nombre_nuevo <- c("id",
  "Llave_Vivienda",
  "Llave_Hogar",
  "Llave_Persona",
  "Cabecera_Resto",
  "24a_Mas_Otras",
  "Mes",
  "Estrato_Energia",
  "Sexo",
  "Edad",
  "Parentesco_Jefe",
  "Afiliacion_Seguridad_Social",
  "Regimen_Seguridad_Social",
  "Nivel_Educativo",
  "Grado_Educativo",
  "Actividad_Semana_Pasada",
  "Funcion_Trabajo",
  "Tiempo_Trabajo_Continuo",
  "Posicion_Ocupacional",
  "Ingreso_Mes_Pasado",
  "Ingreso_Horas_Extras",
  "Valor_Horas_Extras",
  "Incluido_Ingresos_Mes_Pasado",
  "Ingreso_Primas",
  "Valor_Primas",
  "Incluido_Ingresos_Mes_Pasado_Primas",
  "Ingreso_Bonificaciones",
  "Valor_Bonificaciones",
  "Incluido_Ingresos_Mes_Pasado_Bonificaciones",
  "Ingreso_Auxilio_Alimentacion",
  "Valor_Auxilio_Alimentacion",
  "Incluido_Ingresos_Mes_Pasado_Auxilio_Alimentacion",
  "Ingreso_Auxilio_Transporte",
  "Valor_Auxilio_Transporte",
  "Incluido_Ingresos_Mes_Pasado_Auxilio_Transporte",
  "Ingreso_Subsidio_Familiar",
  "Valor_Subsidio_Familiar",
  "Incluido_Ingresos_Mes_Pasado_Subsidio_Familiar",
  "Ingreso_Subsidio_Educativo",
  "Valor_Subsidio_Educativo",
  "Incluido_Ingresos_Mes_Pasado_Subsidio_Educativo",
  "Ingreso_Alimentos_En_Pesos",
  "Valor_Alimentos_En_Pesos",
  "Ingreso_Vivienda_En_Pesos",
  "Valor_Vivienda_En_Pesos",
  "Transporte_Empresa",
  "Valor_Transporte_Empresa",
  "Ingreso_Otros_Ingresos",
  "Valor_Otros_Ingresos",
  "Prima_Servicios",
  "Valor_Prima_Servicios",
  "Prima_Navidad",
  "Valor_Prima_Navidad",
  "Prima_Vacaciones",
  "Valor_Prima_Vacaciones",
  "Viaticos_Permanentes",
  "Valor_Viaticos",
  "Bonificaciones_Anuales",
  "Valor_Bonificaciones_Anuales",
  "Ganancia_Neta_Mes_Pasado",
  "Meses_Recibidos",
  "Ganancia_Neta_Ultimos_12_Meses",
  "Horas_Trabajo_Normales",
  "Total_Empleados",
  "Cotizando_Pensiones",
  "Otro_Trabajo_Semana_Pasada",
  "Horas_Segundo_Trabajo",
  "Ocupacion_Segundo_Trabajo",
  "Ingreso_Segundo_Trabajo",
  "Desea_Mas_Horas",
  "Diligencias_Mas_Horas",
  "Disponible_Mas_Horas",
  "Motivo_Cambiar_Trabajo_Capacidades",
  "Motivo_Cambiar_Trabajo_Ingresos",
  "Diligencias_Cambiar_Trabajo",
  "Inicio_Nuevo_Trabajo",
  "Buscar_Trabajo_Primer_Vez",
  "Ultimo_Trabajo_Desempleado",
  "Ingreso_Desempleado",
  "Valor_Ingreso_Desempleado",
  "Ingreso_Desempleado_Otro",
  "Valor_Ingreso_Desempleado_Otro",
  "Pagos_Arriendos",
  "Pagos_Arriendos_Casas",
  "Valor_Arriendos",
  "Pagos_Pensiones",
  "Valor_Pensiones",
  "Pagos_Pension_Alimenticia",
  "Valor_Pension_Alimenticia",
  "Dinero_Otros_Hogares",
  "Dinero_Otros_Hogares_Nacionales",
  "Valor_Otros_Hogares_Nacionales",
  "Dinero_Otros_Hogares_Externos",
  "Valor_Otros_Hogares_Externos",
  "Ayudas_Dinero_Instituciones_Nacionales",
  "Valor_Ayudas_Nacionales",
  "Dinero_Intereses_Dividendos",
  "Valor_Intereses_Dividendos",
  "Dinero_Cesantias",
  "Valor_Cesantias",
  "Dinero_Otras_Fuentes",
  "Valor_Otras_Fuentes",
  "Poblacion_Edad_Trabajo",
  "Ocupado",
  "Desocupado",
  "Inactivo",
  "Ingreso_Primer_Actividad",
  "Ingreso_Segunda_Actividad",
  "Ingreso_Espacie",
  "Ingreso_Desocupados_Inactivos",
  "Ingreso_Intereses_Dividendos",
  "Ingreso_Jubilaciones_Pensiones",
  "Ingreso_Ayudas_Hogares",
  "Ingreso_Ayudas_Instituciones",
  "Ingreso_Arriendos",
  "Estado_Impa",
  "Estado_Isa",
  "Estado_Ie",
  "Estado_Imdi",
  "Estado_Iof1",
  "Estado_Iof2",
  "Estado_Iof3",
  "Estado_Iof6",
  "Ingreso_Primer_Actividad_Imputado",
  "Ingreso_Segunda_Actividad_Imputado",
  "Ingreso_Espacie_Imputado",
  "Ingreso_Desocupados_Inactivos_Imputado",
  "Ingreso_Intereses_Dividendos_Imputado",
  "Ingreso_Jubilaciones_Pensiones_Imputado",
  "Ingreso_Ayudas_Hogares_Imputado",
  "Ingreso_Ayudas_Instituciones_Imputado",
  "Ingreso_Arriendos_Imputado",
  "Ingreso_Total_Observado",
  "Ingreso_Total_Imputado",
  "Ingreso_Total",
  "Factor_Expansion_Anualizado",
  "Departamento",
  "Factor_Expansion_Departamental"
)


tabla_nombres <- data.frame(Nombre,nombre_nuevo, Descripcion)

####Cargar las bases de datos
setwd("D:/clases/Machine learning para economia aplicada/taller2/data")

train_personas <- read.csv("train_personas.csv")

skim(train_personas)

test_personas <- read.csv("test_personas.csv")

########
#limpieza de las bases de datos
########

## DEjaremas incialmente solo las variables que estan en la base de datos de test 
names(test_personas)
train_personas2 <- train_personas%>% select(names(test_personas))


names(train_personas2)

# Calcular la tasa de completitud
completitud <- sapply(train_personas2, function(x) sum(!is.na(x)) / length(x))

# Crear un dataframe con las tasas de completitud
tasa_completitud <- data.frame(
  Variable = names(completitud),
  Tasa_Completitud = completitud
)

# Unir tasa_completitud con tabla_nombres para agregar nombre_nuevo y Descripcion
tasa_completitud1 <- tasa_completitud %>%
  left_join(tabla_nombres, by = c("Variable" = "Nombre"))
# Mostrar la tabla
view(tasa_completitud)

# Filtrar variables con tasa de completitud mayor o igual al 40%
tasa_completitud_filtrada <- tasa_completitud %>%
  filter(Tasa_Completitud >= 0.3)

# Mostrar la tabla filtrada
print(tasa_completitud_filtrada)


# Crear un nuevo dataframe con solo las variables filtradas
train_personas2<- train_personas2 %>%
  select(all_of(tasa_completitud_filtrada$Variable))

colnames(train_personas2) <- tabla_nombres$nombre_nuevo[match(colnames(train_personas2), tabla_nombres$Nombre)]

names(train_personas2)


skim(train_personas2)

table(train_personas2$Parentesco_Jefe)

# Tomar una muestra aleatoria del 10% de train_personas2
muestra <- train_personas2 %>% slice_sample(prop = 0.01)
# Visualizar la tasa de completitud de la muestra
vis_miss(muestra)


# create a dataset with all variables recoded as 1 if missing o otherwise
db2 <- train_personas2 %>% 
  mutate_all(~ ifelse(!is.na(.), 1, 0))
## drop  variables with not missing or  with all missing.

db2 <-  db2 %>%  select(which(apply(db2, 2, sd) > 0))


M <- cor(db2)
corrplot(M) 

#### vamos a tolerar NA para menos de 12 variables
# Contar los NA por fila en el conjunto de datos 'train_personas2'
NA_count <- rowSums(is.na(train_personas2))

# Filtrar las filas que tengan NA en al menos 12 variables
NA_at_least_12 <- sum(NA_count >= 12)

# Mostrar el número de observaciones con al menos 12 NA
print(NA_at_least_12)
rm(NA_count, NA_at_least_12, db2, muestra)

######Filtrar

# Contar los NA por fila en el conjunto de datos 'train_personas2'
NA_count <- rowSums(is.na(train_personas2))

# Filtrar las filas que tengan menos de 12 NA (mantener las que no cumplen con el criterio)
train_personas2_clean <- train_personas2[NA_count < 12, ]

# Eliminar los objetos creados
rm(NA_count)


# Calcular la tasa de completitud con las observaciones eliminadas
completitud <- sapply(train_personas2_clean, function(x) sum(!is.na(x)) / length(x))
# Crear un dataframe con las tasas de completitud
tasa_completitud_c <- data.frame(
  Variable = names(completitud),
  Tasa_Completitud = completitud
)
#######
###Procesar las variables 
######

###Dummies 

# Crear un vector con los nombres de las variables
v <- c("Sexo", "Dinero_Otros_Hogares", "Pagos_Arriendos", "Ingreso_Desempleado_Otro", "Afiliacion_Seguridad_Social")
# Aplicar table train_personas2_clean
lapply(v, function(var) table(train_personas2_clean[[var]], useNA = "ifany"))

# Reemplazar los valores 2 por 0 en cada una de las variables
train_personas2_clean[v] <- lapply(train_personas2_clean[v], function(x) {
  x[x == 2] <- 0  # Reemplazar 2 por 0
  return(x)       # Retornar la variable modificada
})
# Verificar el resultado
head(train_personas2_clean[v])

########
####Relacion jefe

# Crear un vector con los niveles para la variable Parentesco_Jefe
valores <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
etiquetas <- c("Jefe (a) del hogar", 
               "Pareja, esposo(a), cónyuge", 
               "Hijo(a), hijastro(a)", 
               "Nieto(a)", 
               "Otro pariente", 
               "Empleado(a) del servicio", 
               "Pensionista", 
               "Trabajador", 
               "Otro no pariente")

# Asignar la variable Parentesco_Jefe como un factor con etiquetas
train_personas2_clean$Parentesco_Jefe <- factor(train_personas2_clean$Parentesco_Jefe, 
                                                levels = valores, 
                                                labels = etiquetas)

####Regimen seguridad Social

train_personas2_clean$Regimen_Seguridad_Social[train_personas2_clean$Regimen_Seguridad_Social == 9] <- NA
table(train_personas2_clean$Regimen_Seguridad_Social)

train_personas2_clean$Regimen_Seguridad_Social <- factor(
  train_personas2_clean$Regimen_Seguridad_Social,
  levels = c(1, 2, 3),
  labels = c("Contributivo (eps)", "Especial (fuerzas armadas, ecopetrol, universidades públicas)", "Subsidiado")
)

# Crear variables dummies para cada categoría de Cat_Total_Empleados
train_personas2_clean$Regimen_contributivo <- ifelse(train_personas2_clean$Regimen_Seguridad_Social == "Contributivo (eps)", 1, 0)
train_personas2_clean$Regimen_especial <- ifelse(train_personas2_clean$Regimen_Seguridad_Social == "Especial (fuerzas armadas, ecopetrol, universidades públicas)", 1, 0)
train_personas2_clean$Regimen_subsidiado <- ifelse(train_personas2_clean$Regimen_Seguridad_Social == "Subsidiado", 1, 0)

rm(imputacion, tasa_completitud, tasa_completitud_c, train_personas2,V, valores, nombre_nuevo)

#####Numero de trabajadores

table(train_personas2_clean$Total_Empleados)

train_personas2_clean$Total_Empleados <- cut(
  train_personas2_clean$Total_Empleados,
  breaks = c(-Inf, 1, 8, Inf),  # Definir los puntos de corte
  labels = c("trabaja solo", "Menos de 100", "Más de 100"),  # Etiquetas
  right = TRUE  # Incluir el límite derecho en el intervalo
)

table(train_personas2_clean$Total_Empleados)

# Crear variables dummies para cada categoría de Cat_Total_Empleados
train_personas2_clean$trabaja_solo <- ifelse(train_personas2_clean$Total_Empleados == "trabaja solo", 1, 0)
train_personas2_clean$Menos_100 <- ifelse(train_personas2_clean$Total_Empleados == "Menos de 100", 1, 0)
train_personas2_clean$Mas_100 <- ifelse(train_personas2_clean$Total_Empleados == "Más de 100", 1, 0)

# Verificar las nuevas variables dummies creadas
table(train_personas2_clean$trabaja_solo)
table(train_personas2_clean$Menos_100)
table(train_personas2_clean$Mas_100)
#########


######
#train_personas2_clean <- train_personas2_clean %>%
 # mutate(Sexo = ifelse(Sexo== 2, 0, Sexo),
  #mutate(Dinero_Otros_Hogares = ifelse(Dinero_Otros_Hogares== 2, 0, Dinero_Otros_Hogares),
###  #mutate(Pagos_Arriendos = ifelse(Pagos_Arriendos== 2, 0, Pagos_Arriendos),
  #mutate(Ingreso_Desempleado_Otro= ifelse(Ingreso_Desempleado_Otro= 2, 0, Ingreso_Desempleado_Otro),       
  #mutate(Afiliacion_Seguridad_Social= ifelse(Afiliacion_Seguridad_Social= 2, 0, Afiliacion_Seguridad_Social)       
  #)
######

lapply(v, function(var) table(train_personas2_clean[[var]], useNA = "ifany"))
  

# Reemplazar por NA cuando Afiliacion_Seguridad_Social es igual a 9
train_personas2_clean$Afiliacion_Seguridad_Social[train_personas2_clean$Afiliacion_Seguridad_Social == 9] <- NA
skim(train_personas2_clean)


#####Imputacion

# Definir el vector de variables
v <- c("Sexo", 
       "Dinero_Otros_Hogares", 
       "Pagos_Arriendos", 
       "Ingreso_Desempleado_Otro", 
       "Afiliacion_Seguridad_Social",
       "Tiempo_Trabajo_Continuo", 
       "Horas_Trabajo_Normales", 
       "trabaja_solo",
       "Menos_100",
       "Mas_100",
       "Regimen_contributivo",
       "Regimen_especial",
       "Regimen_subsidiado",
       "Cotizando_Pensiones",
       "Ocupado")  

# Seleccionar las columnas relevantes
train_personas2_clean_subset <- train_personas2_clean[, v]
# Imputar valores NA
imputacion <- mice(train_personas2_clean_subset, m = 1, method = 'pmm', seed = 123)
# Completar la imputación y reemplazar en el data frame original
train_personas2_clean[, v] <- complete(imputacion)
# Verifica el resultado
skim(train_personas2_clean)

#####
#procesar algunas variables
#####





#####
#Collapsar la base de datos
####
names(train_personas2_clean)

D <- c("Edad","Sexo", "Dinero_Otros_Hogares", "Pagos_Arriendos", 
       "Ingreso_Desempleado_Otro", "Afiliacion_Seguridad_Social",
       "Horas_Trabajo_Normales","trabaja_solo",
       "Menos_100",
       "Mas_100",
       "Regimen_contributivo",
       "Regimen_especial",
       "Regimen_subsidiado",
       "Cotizando_Pensiones")


# Crear el nuevo DataFrame train_h para incluir las dummies
train_h <- train_personas2_clean %>%
  group_by(id) %>%
  summarise(across(all_of(D), mean, na.rm = TRUE))


# Eliminar las observaciones donde Nivel_Educativo == 9
train_personas2_clean <- subset(train_personas2_clean, Nivel_Educativo != 9)

# Calcular el máximo de Nivel_Educativo por id
max_nivel_educ <- train_personas2_clean %>%
  group_by(id) %>%
  summarise(Max_Nivel_Educativo = max(Nivel_Educativo, na.rm = TRUE))

# Añadir la nueva variable a train_h
train_h <- merge(train_h, max_nivel_educ, by = "id", all.x = TRUE)

# Verificar los primeros registros de train_h después de añadir la variable
head(train_h)

table(train_personas2_clean$Parentesco_Jefe)

####
# Filtrar las observaciones donde Parentesco_Jefe es "Hijo(a), hijastro(a)"
n_hijos <- train_personas2_clean %>%
  filter(Parentesco_Jefe == "Hijo(a), hijastro(a)") %>%
  group_by(id) %>%
  summarise(n_hijos = n())

# Añadir la nueva variable n_hijos a train_h
train_h <- merge(train_h, n_hijos, by = "id", all.x = TRUE)

# Reemplazar NA por 0 en la columna n_hijos (para hogares sin hijos)
train_h$n_hijos[is.na(train_h$n_hijos)] <- 0

# Verificar los primeros registros de train_h después de añadir la variable
head(train_h)

###
skim(train_h)


##############
#hogares
##############

rm(tasa_completitud_filtrada, train_personas, train_personas2_clean_subset, train_personas2_clean, max_nivel_educ, n_hijos)

test_hogares <- read.csv("test_hogares.csv")

train_hogares <- train_hogares %>%rename(
  cuartos_totales= P5000,
  dormitorios= P5010)


skim(train_hogares)


train_hogares_subset <- train_hogares[, c("id", "cuartos_totales", "dormitorios", "Nper", "Pobre","depto")]

# Unir train_h con train_hogares_subset por la columna 'id'
train_h <- merge(train_h, train_hogares_subset, by = "id", all.x = TRUE)


#Reemplazar los valores de Max_Nivel_Educativo y renombrarla como max_educ
# Reemplazar los valores de max_nivel_educ y renombrarlo a max_educ
train_h <- train_h %>%
  mutate(max_educ = recode(Max_Nivel_Educativo,   
                           `1` = "ninguna",   
                           `2` = "basica",    
                           `3` = "basica",    
                           `4` = "basica",    
                           `5` = "basica",    
                           `6` = "superior"))  
table(train_h$max_educ)

# Eliminar filas con NA en max_educ
train_h <- train_h[!is.na(train_h$max_educ), ]

# Crear las variables dummy para max_educ nuevamente
dummies <- model.matrix(~ max_educ - 1, data = train_h)

# Verificar que el número de filas sea igual
nrow(train_h)
nrow(dummies)

# Agregar las dummies al conjunto de datos original
train_h <- cbind(train_h, dummies)
# Verificar los cambios
head(train_h)
names(train_h)
skim(train_h)

####Guardar la base
write.csv(train_h, file = "train_h.csv", row.names = FALSE)

