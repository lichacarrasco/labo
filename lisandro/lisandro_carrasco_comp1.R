# FE LISANDRO

require(data.table)
require(dplyr)
require(tibble)
require(rpart)

setwd('C:/Users/lisan/OneDrive/Escritorio/MAESTRIA/eyf')
dataset <- fread( "./datasets/competencia1_2022.csv" )

# 0 - Faltantes -----------------------------------------------------------

col_nan <- data.frame((apply(is.na(dataset), MARGIN = 2, FUN=sum)))
names(col_nan)[1] <- "faltantes"
col_nan <- col_nan%>%
  rownames_to_column("columna")%>%
  filter(faltantes > 0)                                                         # 46 cols con nans

dataset[is.na(dataset)] <- 0                                                    # Imputo a 0 porque sí 
any(is.na(dataset))


dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]   # Estos nans no se imputan

# 1 - Ideas Zulip ---------------------------------------------------------

# * 1 - Data drifting -----------------------------------------------------

dataset[ foto_mes==202103, Visa_fultimo_cierre := ifelse(Visa_fultimo_cierre== 1, 4, Visa_fultimo_cierre) ]
dataset[ foto_mes==202103, Visa_fultimo_cierre := ifelse(Visa_fultimo_cierre== 7, 11, Visa_fultimo_cierre) ]
dataset[ foto_mes==202103, Visa_fultimo_cierre := ifelse(Visa_fultimo_cierre== 21, 25, Visa_fultimo_cierre) ]
dataset[ foto_mes==202103, Visa_fultimo_cierre := ifelse(Visa_fultimo_cierre== 14, 18, Visa_fultimo_cierre) ]
dataset[ foto_mes==202103, Visa_fultimo_cierre := ifelse(Visa_fultimo_cierre== 28, 32, Visa_fultimo_cierre) ]
dataset[ foto_mes==202103, Visa_fultimo_cierre := ifelse(Visa_fultimo_cierre== 35, 39, Visa_fultimo_cierre) ]
dataset[ foto_mes==202103, Visa_fultimo_cierre := ifelse(Visa_fultimo_cierre > 39, Visa_fultimo_cierre + 4, Visa_fultimo_cierre) ]

dataset[ foto_mes==202103, Master_fultimo_cierre := ifelse(Master_fultimo_cierre== 1, 4, Master_fultimo_cierre) ]
dataset[ foto_mes==202103, Master_fultimo_cierre := ifelse(Master_fultimo_cierre== 7, 11, Master_fultimo_cierre) ]
dataset[ foto_mes==202103, Master_fultimo_cierre := ifelse(Master_fultimo_cierre== 21, 25, Master_fultimo_cierre) ]
dataset[ foto_mes==202103, Master_fultimo_cierre := ifelse(Master_fultimo_cierre== 14, 18, Master_fultimo_cierre) ]
dataset[ foto_mes==202103, Master_fultimo_cierre := ifelse(Master_fultimo_cierre== 28, 32, Master_fultimo_cierre) ]
dataset[ foto_mes==202103, Master_fultimo_cierre := ifelse(Master_fultimo_cierre== 35, 39, Master_fultimo_cierre) ]
dataset[ foto_mes==202103, Master_fultimo_cierre := ifelse(Master_fultimo_cierre > 39, Master_fultimo_cierre + 4, Master_fultimo_cierre) ]

# * 2 - Nuevas features ---------------------------------------------------

dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]
dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]
dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]
dataset[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
dataset[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]
dataset[ , campo9 := mcaja_ahorro > 0 ]
dataset[ , campo10 := mcaja_ahorro_adicional > 0 ]
dataset[ , campo11 := mcaja_ahorro_dolares > 0 ]
dataset[ , campo12 := 1/2 * sqrt ( mcaja_ahorro^2 + mcuenta_corriente^2 ) ]
dataset[ , campo13 := ( cliente_edad < 30 & mpayroll > 400000 ) ]

# 2 - Ideas propias -------------------------------------------------------

dataset <- dataset %>% 
  select(-c(cforex, cforex_buy, mforex_buy, cforex_sell, 
            mforex_sell, ctrx_quarter)) %>%                                      # Por irrelevantes según f.i. anteriores
  mutate(
    fraccion_vida   = cliente_antiguedad / (cliente_edad * 12),
    productos_comi  = mcomisiones / cproductos,
    monto_total     = mcuenta_corriente + mcuenta_corriente_adicional + mcaja_ahorro +
                      mcaja_ahorro_adicional + mcaja_ahorro_dolares,
    monto_prod      = monto_total / cproductos,
    pasamanos       = ifelse(ctarjeta_debito_transacciones == 1, 1, 0),
    edad_sueldo     = mpayroll / cliente_edad,
    mingresos       = mpayroll + mpayroll2,
    sueldo_otros    = mpayroll / mpayroll2,
    minversiones    = mplazo_fijo_dolares + mplazo_fijo_pesos + minversion1_pesos +
                      minversion1_dolares + minversion2,
    mdeuda          = mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios,
    gastos_tc       = mtarjeta_visa_consumo + mtarjeta_master_consumo,
    compromiso      = ( mingresos + minversiones ) / mdeuda + gastos_tc,
    cseguros        = cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales,
    cdebitos_aut    = ccuenta_debitos_automaticos + ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos,
    pagos_deb       = cdebitos_aut + cpagodeservicios + cpagomiscuentas,
    cdescuentos     = ctarjeta_visa_descuentos + ctarjeta_master_descuentos + ccajeros_propios_descuentos,
    mdescuentos     = mtarjeta_visa_descuentos + mtarjeta_master_descuentos + mcajeros_propios_descuentos
  )

# * 1 . Visa y Master -----------------------------------------------------

dataset <- dataset %>% 
  mutate(
    vm_delinquency     = ifelse(Master_delinquency + Visa_delinquency > 0, 1, 0),
    vm_mfinanc_limi    = Master_mfinanciacion_limite +  Visa_mfinanciacion_limite,
    vm_msaldototal     = Master_msaldototal + Visa_msaldototal,
    vm_msaldopesos     = Master_msaldopesos + Visa_msaldopesos,
    vm_msaldodolar     = Master_msaldodolares + Visa_msaldodolares,
    vm_mconsumopesos   = Master_mconsumospesos + Visa_mconsumospesos,
    vm_mconsumodolar   = Master_mconsumosdolares + Visa_mconsumosdolares,
    vm_mlimitecompra   = Master_mlimitecompra + Visa_mlimitecompra,
    vm_madelantopesos  = Master_madelantopesos + Visa_madelantopesos,
    vm_madelantodolar  = Master_madelantodolares + Visa_madelantodolares,
    vm_mpagado         = Master_mpagado + Visa_mpagado,
    vm_mpagospesos     = Master_mpagospesos + Visa_mpagospesos,
    vm_mpagosdolar     = Master_mpagosdolares + Visa_mpagosdolares,
    vm_mconsumototal   = Master_mconsumototal + Visa_mconsumototal,
    vm_cconsumos       = Master_cconsumos + Visa_cconsumos,
    vm_cadelantosefe   = Master_cadelantosefectivo + Visa_cadelantosefectivo,
    vm_mpagominimo     = Master_mpagominimo + Visa_mpagominimo,
    vm_msaldo_limite   = vm_msaldototal / vm_mlimitecompra,
    vm_mpago_limite    = vm_mpagado / vm_mlimitecompra,
    vm_mpagodol_limite = vm_mpagosdolar / vm_mlimitecompra,
    vm_mpago_consumo   = vm_mpagado / vm_mconsumototal,
    vm_mpagomin_limite = vm_mpagominimo / vm_mlimitecompra
    )

# 3 - Ranking -------------------------------------------------------------

rankeado <- dataset %>% 
  mutate(
    across(
      .cols = c(where(~is.numeric(.)), -c(numero_de_cliente, foto_mes)),
      .fns  = ~frank(., ties.method = 'dense')
    )
  )


#write.csv(rankeado, 'dataset_compe1_rankeado.csv', row.names = FALSE)
#write.csv(dataset, 'dataset_compe1_feature.csv', row.names = FALSE)


# 4 - Canarios ------------------------------------------------------------

#for( i in 1:40 ) rankeado[ , paste0("canarito", i ) :=  runif( nrow(rankeado)) ]


# 5 - Split ---------------------------------------------------------------

dtrain  <- rankeado[ foto_mes==202101 ]  
dapply  <- rankeado[ foto_mes==202103 ]  


# 6 - Entrenamiento -------------------------------------------------------

modelo  <- rpart(formula   = "clase_binaria ~ . -clase_ternaria -mcomisiones_mantenimiento -Visa_mpagado",
                 data      = dtrain,  
                 xval      = 5,
                 cp        = -0.3104728,
                 minsplit  = 1132,   
                 minbucket = 112,   
                 maxdepth  = 10 ) 

#modelo$frame[ modelo$frame$var %like% "canarito", "complexity"] <- -666
#modelo_pruned  <- prune(  modelo, -666 )


# 7 - Prediccion ----------------------------------------------------------

prediccion  <- predict( object=  modelo_pruned,
                        newdata= dapply,
                        type = "prob")

dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]

set.seed(c(311203, 120929, 181213, 912031, 201809))  
dfinal[ , azar := runif( nrow(dapply) ) ]

setorder( dfinal, -prob_SI, azar )

for( corte  in  c(  9000 ) )
{
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]
  
  
  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], 
          file= paste0( "./exp/compe_1/entrega_sin_canario_2",  corte, ".csv"),
          sep=  "," )
}
