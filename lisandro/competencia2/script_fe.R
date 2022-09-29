# FEATURE ENGINEERING LISANDRO

require(data.table)
require(dplyr)

setwd('C:/Users/lisan/OneDrive/Escritorio/MAESTRIA/eyf')
dataset <- fread( "./datasets/competencia2_2022.csv.gz" )


# 1 - Binaria -------------------------------------------------------------

# col_nan <- data.frame((apply(is.na(dataset), MARGIN = 2, FUN=sum)))
# names(col_nan)[1] <- "faltantes"
# col_nan <- col_nan%>%
#   rownames_to_column("columna")%>%
#   filter(faltantes > 0)                                                         # 46 cols con nans
# 
# # dataset[is.na(dataset)] <- 0                                                    # Imputo a 0 porque sí 
# # any(is.na(dataset))

dataset[ foto_mes==202103, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]   # Estos nans no se imputan


# 2 - Variables Visa - Master ---------------------------------------------

#  * 1 - Combinacion ------------------------------------------------------

dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

# * 2 - Nuevas opciones ---------------------------------------------------

dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]


# 3 - Otras ideas catedra -------------------------------------------------

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


# 4 - Ideas propias -------------------------------------------------------

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
    cdebitos_aut    = ccuenta_debitos_automaticos + ctarjeta_visa_debitos_automaticos 
                      + ctarjeta_master_debitos_automaticos,
    pagos_deb       = cdebitos_aut + cpagodeservicios + cpagomiscuentas,
    cdescuentos     = ctarjeta_visa_descuentos + ctarjeta_master_descuentos + ccajeros_propios_descuentos,
    mdescuentos     = mtarjeta_visa_descuentos + mtarjeta_master_descuentos + mcajeros_propios_descuentos,
    desc_gastos     = gastos_tc / mdescuentos,
    limite_ingresos = mv_mlimitecompra/mingresos
  )

# 5 - Ideas Github --------------------------------------------------------


#  * 1 - Agostina Simonelli -----------------------------------------------

dataset[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
dataset[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]
dataset[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
dataset[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
dataset[, f_mcomisiones  := mcomisiones_mantenimiento+mcomisiones_otras]

# 2 - Lucas Trevisani -----------------------------------------------------

dataset[, balance := total_activos - total_deuda]
dataset[, ratio_deuda := total_deuda / (total_activos + 1)]
dataset[, has_debito_transacciones := ifelse(dataset$ctarjeta_debito_transacciones > 0, 1, 0) ]
dataset[, has_visa := ifelse(dataset$ctarjeta_visa > 0, 1, 0) ]
dataset[, has_visa_transacciones := ifelse(dataset$ctarjeta_visa_transacciones > 0, 1, 0) ]
dataset[, has_master := ifelse(dataset$ctarjeta_master > 0, 1, 0) ]
dataset[, has_master_transacciones := ifelse(dataset$ctarjeta_master_transacciones > 0, 1, 0) ]
dataset[, has_payroll := ifelse(dataset$cpayroll_trx + dataset$cpayroll2_trx  > 0, 1, 0) ]
dataset[, has_pmc := ifelse(dataset$cpagomiscuentas  > 0, 1, 0) ]
dataset[, has_da := ifelse(dataset$ccuenta_debitos_automaticos + dataset$ctarjeta_visa_debitos_automaticos + dataset$ctarjeta_master_debitos_automaticos  > 0, 1, 0) ]

# 6 - Data & Concept drifting ---------------------------------------------

# dataset <- select(dataset, )

dataset_fe <- dataset

setwd('C:/Users/lisan/OneDrive/Escritorio/MAESTRIA/eyf/labo/lisandro/competencia2')
fwrite(dataset_fe, 'dataset_fe_c2.csv')

