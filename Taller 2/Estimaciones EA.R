require("pacman")
p_load( tidyverse,
        glmnet,
        caret,
        MLmetrics,
        pROC,
        doParallel,
        ranger,
        haven,
        leaps,
        dplyr)

setwd("C:/Users/Esteban/Dropbox/Estudios/Big Data y Machine Learning/Talleres/Taller 2")

test = read_dta("data_test.dta")
train = read_dta("data_train.dta")
train = train %>% mutate(pobre=factor(pobre,levels=c(0,1),labels=c("No","Yes"))) 
train = train %>% mutate(oficio=ifelse(is.na(oficio),100,oficio)) 
test = test %>% mutate(oficio=ifelse(is.na(oficio),100,oficio)) 
train = train %>% mutate(dep_emp=nper/adul_trabaja) 
test = test %>% mutate(dep_emp=nper/adul_trabaja) 


cat_vars = c("depto", "vivienda_pagada", "vivienda_pagando", "vivienda_arriendo",
             "vivienda_usufructo", "vivienda_otro", "urbano", "ciudad", "seg_social",
             "seg_social_j",  "oficio", "ing_horasextra", "ing_horasextra_j",
             "primas", "primas_j", "bonificaciones", "bonificaciones12", "bonificaciones_j",
             "bonificaciones12_j", "sub_alimentacion", "sub_alimentacion_j",
             "sub_transporte", "sub_transporte_j", "sub_familiar", "sub_familiar_j",
             "sub_educativo", "sub_educativo_j", "salario_alimentos", "salario_alimentos_j",
             "salario_vivienda", "salario_vivienda_j", "transporte_empresa",
             "transporte_empresa_j", "salario_especie", "salario_especie_j", "prima12",
             "prima12_j", "prima_navidad12", "prima_navidad12_j", "prima_vacaciones12",
             "prima_vacaciones12_j", "viaticos12", "viaticos12_j", "salario_extra",
             "salario_extra_j", "cot_pension", "cot_pension_j", "otro_trabajo_negocio",
             "otro_trabajo_negocio_j", "mas_horas", "mas_horas_j", "intenta_mas_horas",
             "intenta_mas_horas_j", "disp_mas_horas", "disp_mas_horas_j",
             "int_cambio_trabajo", "int_cambio_trabajo_j", "primer_trabajo",
             "primer_trabajo_j", "ing_arr_pen", "ing_arr_pen_j", "ing_pension",
             "ing_pension_j", "ing_separacion", "ing_separacion_j", "ing_otros",
             "ing_otros_j", "ing_per_pais", "ing_per_pais_j", "ing_per_ext",
             "ing_per_ext_j", "ing_inst", "ing_inst_j", "ing_fin", "ing_fin_j",
             "ing_cesantias", "ing_cesantias_j", "ing_dif", "ing_dif_j", "ne_ninguno",
             "ne_ninguno_j", "ne_preescolar", "ne_preescolar_j", "ne_basica", "ne_basica_j",
             "ne_secundaria", "ne_secundaria_j", "ne_media", "ne_media_j", "ne_superior",
             "ne_superior_j", "empleado", "empleado_j", "domestico", "domestico_j",
             "independiente", "independiente_j", "sin_remuneracion", "sin_remuneracion_j",
             "jornalero", "jornalero_j", "tamano1", "tamano1_j", "tamano2", "tamano2_j",
             "tamano3", "tamano3_j", "menor_trabajando", "jefe_tedad", "tercera_edad",
             "jefe_mujer","ingreso_extra")

train = train %>% mutate_at(cat_vars, as.factor)
test = test %>% mutate_at(cat_vars, as.factor)


# Primero intentaremos ridge
model_form = pobre ~ arriendo + vivienda_pagada + vivienda_pagando + vivienda_arriendo +
                    vivienda_usufructo + vivienda_otro + urbano + ciudad +
                    per_hab + adul_trabaja + meses_empresa + poly(horas_trabajadas,2,raw=TRUE) +
                    seg_social + oficio + ing_horasextra + primas + bonificaciones +
                    sub_educativo + salario_vivienda + salario_especie + salario_extra +
                    cot_pension + otro_trabajo_negocio + mas_horas + int_cambio_trabajo +
                    primer_trabajo + ne_ninguno + ne_preescolar + ne_basica + ne_secundaria +
                    ne_media + ne_superior + empleado + domestico + independiente +
                    sin_remuneracion + jornalero + tamano1 + tamano2 + tamano3 +
                    menor_trabajando + jefe_tedad + tercera_edad + ingreso_extra +
                    ing_inst + ing_fin + ing_arr_pen + jefe_mujer + poly(edad,3,raw=TRUE) +
                    edad:ne_ninguno + edad:ne_preescolar + edad:ne_basica + 
                    edad:ne_secundaria + edad:ne_media + edad:ne_superior + edad:jefe_mujer
max_nvars = length(lm(model_form,data = train)$coefficients)-1
max_nvars # 164
fitControl = trainControl(method = "cv",
                          number = 10)
train$pobre2 = as.numeric(train$pobre)
Y = train$pobre2
X = model.matrix(model_form, data = train)
X = X[,-1]
ridge = glmnet(
  x=X,
  y=Y,
  alpha=0
  )
set.seed(202020)
ridge_caret = train(model_form,
                    data=train,
                    method='glmnet',
                    trControl=fitControl,
                    tuneGrid=expand.grid(alpha=0,
                                         lambda=ridge$lambda))
ridge_caret

predictSample <- test %>% mutate(pobre_lab = predict(ridge_caret,newdata = test, type ="raw")
                                ) %>% select(id,pobre_lab)
predictSample <- predictSample %>% 
                  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
                  select(id,pobre)
head(predictSample)
name <- paste0("Ridge_","01291277",".csv")
write.csv(predictSample,name,row.names =FALSE)

# Ahora intentemos con lasso
lasso = glmnet(
  x=X,
  y=Y,
  alpha=1
)
set.seed(202020)
lasso_caret = train(model_form,
                    data=train,
                    method='glmnet',
                    trControl=fitControl,
                    tuneGrid=expand.grid(alpha=1,
                                         lambda=lasso$lambda))
lasso_caret

predictSample <- test %>% mutate(pobre_lab = predict(lasso_caret,newdata = test, type ="raw")
) %>% select(id,pobre_lab)
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)
table(predictSample$pobre)
name <- paste0("Lasso_","2.256543e-05",".csv")
write.csv(predictSample,name,row.names =FALSE)


# Lo anterior era con variables combinadas entre el jefe y el conjugue del hogar
# Ahora voy a intentar solo con las del jefe
# Lasso se demora mucho, por lo que usaré ridge
# El nivel de precisión disminuyó con esta iteración.
model_form = pobre ~ arriendo + vivienda_pagada + vivienda_pagando + vivienda_arriendo +
  vivienda_usufructo + vivienda_otro + urbano + ciudad +
  per_hab + adul_trabaja + meses_empresa_j + poly(horas_trabajadas_j,2,raw=TRUE) +
  seg_social_j + oficio + ing_horasextra_j + primas_j + bonificaciones_j +
  sub_educativo_j + salario_vivienda_j + salario_especie_j + salario_extra_j +
  cot_pension_j + otro_trabajo_negocio_j + mas_horas_j + int_cambio_trabajo_j +
  primer_trabajo_j + ne_ninguno_j + ne_preescolar_j + ne_basica_j + ne_secundaria_j +
  ne_media_j + ne_superior_j + empleado_j + domestico_j + independiente_j +
  sin_remuneracion_j + jornalero_j + tamano1_j + tamano2_j + tamano3_j +
  menor_trabajando + jefe_tedad + tercera_edad + ingreso_extra +
  ing_inst_j + ing_fin_j + ing_arr_pen_j + jefe_mujer + poly(edad_j,3,raw=TRUE) +
  edad_j:ne_ninguno_j + edad_j:ne_preescolar_j + edad_j:ne_basica_j + 
  edad_j:ne_secundaria_j + edad_j:ne_media_j + edad_j:ne_superior_j + edad_j:jefe_mujer

fitControl = trainControl(method = "cv",
                          number = 10)
train$pobre2 = as.numeric(train$pobre)
Y = train$pobre2
X = model.matrix(model_form, data = train)
X = X[,-1]
ridge = glmnet(
  x=X,
  y=Y,
  alpha=0
)
set.seed(202020)
ridge_caret = train(model_form,
                    data=train,
                    method='glmnet',
                    trControl=fitControl,
                    tuneGrid=expand.grid(alpha=0,
                                         lambda=ridge$lambda))
ridge_caret

predictSample <- test %>% mutate(pobre_lab = predict(ridge_caret,newdata = test, type ="raw")
) %>% select(id,pobre_lab)
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)
name <- paste0("Ridge_","01291277",".csv")
write.csv(predictSample,name,row.names =FALSE)
   

# LOGIT my guest
model_form = pobre ~ arriendo + vivienda_pagada + vivienda_pagando + vivienda_arriendo +
  vivienda_usufructo + vivienda_otro + urbano + ciudad +
  per_hab + adul_trabaja + poly(horas_trabajadas,2,raw=TRUE) +
  oficio + salario_extra + cot_pension + otro_trabajo_negocio +
  ne_ninguno + ne_preescolar + ne_basica + ne_secundaria +
  ne_media + ne_superior + empleado + tamano1 + tamano2 + tamano3 +
  menor_trabajando + jefe_tedad + ingreso_extra +
  jefe_mujer + poly(edad,3,raw=TRUE) +
  edad:ne_superior + edad:jefe_mujer
max_nvars = length(lm(model_form,data = train)$coefficients)-1
max_nvars # 140

ctrl =  trainControl(method = "cv",
                    number =10,
                    classProbs =TRUE,
                    savePredictions =TRUE,
                    verbose=FALSE
                    )

set.seed(202020)
logit <- train(model_form,
               data =train,
               method ="glm",
               family ="binomial",
               trControl =ctrl
               )
logit

predictSample <- test %>% mutate(pobre_lab = predict(logit,newdata = test, type ="raw")
) %>% select(id,pobre_lab)
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)
table(predictSample$pobre)
name <- paste0("Logit","","_simple","",".csv")
write.csv(predictSample,name,row.names =FALSE)

# Logit lambda

lambda <- 10^seq(-1, -4, length = 100)
grid <- expand.grid("alpha" = seq(0,1,by=.1), lambda = lambda)
set.seed(202020)
logit_lambda <- train(model_form,
  method = "glmnet",
  data = train,
  family = "binomial",
  trControl = ctrl,
  tuneGrid = grid,
  preProcess = c("center", "scale")
)
logit_lambda

predictSample <- test %>% mutate(pobre_lab = predict(logit_lambda,newdata = test, type ="raw")
) %>% select(id,pobre_lab)
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)
table(predictSample$pobre)
name <- paste0("Logit","_lambda_","1e04",".csv")
write.csv(predictSample,name,row.names =FALSE)

## Volvemos a las otras variables e intentamos EN

model_form = pobre ~ arriendo + vivienda_pagada + vivienda_pagando + vivienda_arriendo +
  vivienda_usufructo + vivienda_otro + urbano + ciudad +
  per_hab + adul_trabaja + meses_empresa + poly(horas_trabajadas,2,raw=TRUE) +
  seg_social + oficio + ing_horasextra + primas + bonificaciones +
  sub_educativo + salario_vivienda + salario_especie + salario_extra +
  cot_pension + otro_trabajo_negocio + mas_horas + int_cambio_trabajo +
  primer_trabajo + ne_ninguno + ne_preescolar + ne_basica + ne_secundaria +
  ne_media + ne_superior + empleado + domestico + independiente +
  sin_remuneracion + jornalero + tamano1 + tamano2 + tamano3 +
  menor_trabajando + jefe_tedad + tercera_edad + ingreso_extra +
  ing_inst + ing_fin + ing_arr_pen + jefe_mujer + poly(edad,3,raw=TRUE) +
  edad:ne_ninguno + edad:ne_preescolar + edad:ne_basica + 
  edad:ne_secundaria + edad:ne_media + edad:ne_superior + edad:jefe_mujer
max_nvars = length(lm(model_form,data = train)$coefficients)-1
max_nvars # 164
fitControl = trainControl(method = "cv",
                          number = 10)
train$pobre2 = as.numeric(train$pobre)
Y = train$pobre2
X = model.matrix(model_form, data = train)
X = X[,-1]

tuneGrid <- expand.grid(alpha=seq(0,1,0.05),
                        lambda=seq(0.1,2,0.1))
set.seed(202020)
EN_caret = train(model_form,
                 data=train,
                 method='glmnet',
                 trControl=fitControl,
                 tuneGrid=tuneGrid)
EN_caret
EN_caret$ bestTune

predictSample <- test %>% mutate(pobre_lab = predict(EN_caret,newdata = test, type ="raw")
) %>% select(id,pobre_lab)
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)
name <- paste0("EN_lambda","","_alpha_","",".csv")
write.csv(predictSample,name,row.names =FALSE)

# Model tuning

multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

ctrl_multiStats<- trainControl(method = "cv",
                               number = 10,
                               summaryFunction = multiStats,
                               classProbs = TRUE,
                               verbose=FALSE,
                               savePredictions = T)

lambda <- 10^seq(-1, -4, length = 100)
grid <- expand.grid("alpha" = seq(0,1,by=.2), lambda = lambda)

set.seed(1410)
model_tuning <- train(model_form,
  method = "glmnet",
  data = train,
  family = "binomial",
  tuneGrid = grid,
  preProcess = c("center", "scale"),
  trControl = ctrl_multiStats,
  metric = "Sens"
)

model_tuning

predictSample <- test %>% mutate(pobre_lab = predict(model_tuning,newdata = test, type ="raw")
) %>% select(id,pobre_lab)
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)
name <- paste0("model_tuning",".csv")
write.csv(predictSample,name,row.names =FALSE)
