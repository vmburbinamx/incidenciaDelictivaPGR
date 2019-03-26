# --------LIBRERIAS--------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)
library(readxl)

# --------PRIMERA PARTE (2001-2006)--------------------------------
nombres_ajustados_2001_2006 <- c("FECHA", "DELEGACION", "CLAVE_INEGI_AGEE",
               "DELITOS_CONTRA_LA_SALUD__PRODUCCION", "DELITOS_CONTRA_LA_SALUD_TRANSPORTE",
               "DELITOS_CONTRA_LA_SALUD_TRAFICO", "DELITOS_CONTRA_LA_SALUD_COMERCIO",
               "DELITOS_CONTRA_LA_SALUD_SUMINISTRO", "DELITOS_CONTRA_LA_SALUD_POSESION",
               "DELITOS_CONTRA_LA_SALUD_OTROS", "DELITOS_DIVERSOS_INSTITUCIONES_DE_BANCA_Y_CREDITO",
               "DELITOS_DIVERSOS_FISCALES", "DELITOS_DIVERSOS_PATRIMONIALES",
               "DELITOS_DIVERSOS_AMBIENTALES", "DELITOS_DIVERSOS_PROPIEDAD_INTELECTUAL_E_INDUSTRIAL",
               "DELITOS_DIVERSOS_SERVIDOR_PUBLICO", "DELITOS_DIVERSOS_LFAFYE",
               "DELITOS_DIVERSOS_ASOCIACION_DELICTUOSA", "DELITOS_DIVERSOS_ROBO_EN_CARRETERA",
               "DELITOS_DIVERSOS_LEY_GENERAL_DE_POBLACION",
               "DELITOS_DIVERSOS_ATAQUES_A_LAS_VIAS_GENERALES_DE_COMUNICACION",
               "DELITOS_DIVERSOS_CULPOSOS_POR_TRANSITO_DE_VEHICULOS",
               "DELITOS_DIVERSOS_OTRAS_LEYES_ESPECIALES", "DELITOS_DIVERSOS_LFCDO",
               "DELITOS_DIVERSOS_DELITOS_ELECTORALES", "DELITOS_DIVERSOS_OTROS")

data_2001_2006 <- read_csv("INCIDENCIA DELICTIVA FEDERAL 2001-2006_Datos abiertos.csv", col_names = nombres_ajustados_2001_2006, skip = 1)
data_2001_2006 <- as.data.frame(data_2001_2006)

#------------- Clean up
# "M\u0090XICO"
valueToBeReplaced <- "M\u0090XICO"
newValue <- "MEXICO"
data_2001_2006$DELEGACION <- str_replace_all(data_2001_2006$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "M�XICO",
valueToBeReplaced <- "M�XICO"
newValue <- "MEXICO"
data_2001_2006$DELEGACION <- str_replace_all(data_2001_2006$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "MICHOAC�N,
valueToBeReplaced <- "MICHOAC�N"
newValue <- "MICHOACAN"
data_2001_2006$DELEGACION <- str_replace_all(data_2001_2006$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "NUEVO LE�N
valueToBeReplaced <- "NUEVO LE�N"
newValue <- "NUEVO LEON"
data_2001_2006$DELEGACION <- str_replace_all(data_2001_2006$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# QUERETARO
valueToBeReplaced <- "QUER�TARO"
newValue <- "QUERETARO"
data_2001_2006$DELEGACION <- str_replace_all(data_2001_2006$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# SAN LUIS
valueToBeReplaced <- "SAN LUIS POTOS�"
newValue <- "SAN LUIS POTOSI"
data_2001_2006$DELEGACION <- str_replace_all(data_2001_2006$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# YUCATAN
valueToBeReplaced <- "YUCAT�N"
newValue <- "YUCATAN"
data_2001_2006$DELEGACION <- str_replace_all(data_2001_2006$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)


data_2001_2006 <- data_2001_2006 %>% mutate(YEAR = as.numeric(substr(FECHA,1,4)),
                                            MONTH = as.numeric(substr(FECHA, nchar(FECHA)-1, nchar(FECHA))),
                                            DATE = as.Date(ISOdate(YEAR, MONTH, 1)))

data_2001_2006$FECHA <- NULL
data_2001_2006$YEAR <- NULL
data_2001_2006$MONTH <- NULL
data_2001_2006$CLAVE_INEGI_AGEE <- NULL

data_2001_2006 <- data_2001_2006 %>% gather("DELITO", "TOTAL", 2:(ncol(data_2001_2006)-1))

theData <- data_2001_2006

remove(data_2001_2006, nombres_ajustados_2001_2006)

# --------SEGUNDA PARTE (2007-2011)--------------------------------
nombres_ajustados_2007_2011 <- c("FECHA", "DELEGACION", "CLAVE_INEGI_AGEE",
                                 "DELITOS_CONTRA_LA_SALUD__PRODUCCION",
                                 "DELITOS_CONTRA_LA_SALUD_TRANSPORTE",
                                 "DELITOS_CONTRA_LA_SALUD_TRAFICO",
                                 "DELITOS_CONTRA_LA_SALUD_COMERCIO",
                                 "DELITOS_CONTRA_LA_SALUD_SUMINISTRO",
                                 "DELITOS_CONTRA_LA_SALUD_POSESION",
                                 "DELITOS_CONTRA_LA_SALUD_OTROS",
                                 "DELITOS_DIVERSOS_INSTITUCIONES_DE_BANCA_Y_CREDITO",
                                 "DELITOS_DIVERSOS_FISCALES", "DELITOS_DIVERSOS_PATRIMONIALES",
                                 "DELITOS_DIVERSOS_AMBIENTALES", "DELITOS_DIVERSOS_PROPIEDAD_INTELECTUAL_E_INDUSTRIAL",
                                 "DELITOS_DIVERSOS_SERVIDOR_PUBLICO", "DELITOS_DIVERSOS_LFAFYE",
                                 "DELITOS_DIVERSOS_LEY_GENERAL_DE_POBLACION",
                                 "DELITOS_DIVERSOS_ATAQUES_A_LAS_VIAS_GENERALES_DE_COMUNICACION",
                                 "DELITOS_DIVERSOS_CONTRA_LA_INTEGRIDA_CORPORAL",
                                 "DELITOS_DIVERSOS_OTRAS_LEYES_ESPECIALES",
                                 "DELITOS_DIVERSOS_LFCDO", "DELITOS_DIVERSOS_DELITOS_ELECTORALES",
                                 "DELITOS_DIVERSOS_OTROS")

data_2007_2011 <- read_csv("INCIDENCIA DELICTIVA FEDERAL 2007-2011_Datos Abiertos.csv", col_names = nombres_ajustados_2007_2011, skip = 1)
# rm(list = ls())
data_2007_2011 <- as.data.frame(data_2007_2011)

#------------- Clean up
# "M\u0090XICO"
valueToBeReplaced <- "M\u0090XICO"
newValue <- "MEXICO"
data_2007_2011$DELEGACION <- str_replace_all(data_2007_2011$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "M�XICO",
valueToBeReplaced <- "M�XICO"
newValue <- "MEXICO"
data_2007_2011$DELEGACION <- str_replace_all(data_2007_2011$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "MICHOAC�N,
valueToBeReplaced <- "MICHOAC�N"
newValue <- "MICHOACAN"
data_2007_2011$DELEGACION <- str_replace_all(data_2007_2011$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "NUEVO LE�N
valueToBeReplaced <- "NUEVO LE�N"
newValue <- "NUEVO LEON"
data_2007_2011$DELEGACION <- str_replace_all(data_2007_2011$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# QUERETARO
valueToBeReplaced <- "QUER�TARO"
newValue <- "QUERETARO"
data_2007_2011$DELEGACION <- str_replace_all(data_2007_2011$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# SAN LUIS
valueToBeReplaced <- "SAN LUIS POTOS�"
newValue <- "SAN LUIS POTOSI"
data_2007_2011$DELEGACION <- str_replace_all(data_2007_2011$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# YUCATAN
valueToBeReplaced <- "YUCAT�N"
newValue <- "YUCATAN"
data_2007_2011$DELEGACION <- str_replace_all(data_2007_2011$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

data_2007_2011 <- data_2007_2011 %>% mutate(YEAR = as.numeric(substr(FECHA,1,4)),
                                            MONTH = as.numeric(substr(FECHA, nchar(FECHA)-1, nchar(FECHA))),
                                            DATE = as.Date(ISOdate(YEAR, MONTH, 1)))

data_2007_2011$FECHA <- NULL
data_2007_2011$YEAR <- NULL
data_2007_2011$MONTH <- NULL
data_2007_2011$CLAVE_INEGI_AGEE <- NULL
data_2007_2011 <- data_2007_2011 %>% gather("DELITO", "TOTAL", 2:(ncol(data_2007_2011)-1))
theData <- bind_rows(data_2007_2011, theData)
remove(data_2007_2011, nombres_ajustados_2007_2011)

# --------TERCERA PARTE (2007-2011)--------------------------------

nombres_ajustados_2012_2015 <- c("FECHA", "DELEGACION", "CLAVE_INEGI_AGEE",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD__PRODUCCION",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_TRANSPORTE",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_TRAFICO",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_COMERCIO",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_SUMINISTRO",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_POSESION",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_OTROS",
                                 "CODIGO_PENAL_FEDERAL_CPF_COMETIDOS_POR_SERVIDORES_PUBLICOS",
                                 "CODIGO_PENAL_FEDERAL_CPF_CONTRA_EL_AMBIENTE_Y_LA_GESTION_AMBIENTAL",
                                 "CODIGO_PENAL_FEDERAL_CPF_CONTRA_LA_INTEGRIDAD_CORPORAL",
                                 "CODIGO_PENAL_FEDERAL_CPF_ELECTORALES",
                                 "CODIGO_PENAL_FEDERAL_CPF_EN_MATERIA_DE_DERECHOS_DE_AUTOR",
                                 "CODIGO_PENAL_FEDERAL_CPF_FALSEDAD_TITULO_DECIMO_TERCERO",
                                 "CODIGO_PENAL_FEDERAL_CPF_PATRIMONIALES",
                                 "CODIGO_PENAL_FEDERAL_CPF_VIAS_DE_COMUNICACION_Y_CORRESPONDENCIA",
                                 "CODIGO_PENAL_FEDERAL_CPF_OTROS_DELITOS_DEL_CPF",
                                 "CODIGO_FISCAL_DE_LA_FEDERACION_CFF", "LEY_DE_LA_PROPIEDAD_INDUSTRIAL_LPI",
                                 "LEY_DE_VIAS_GENERALES_DE_COMUNICACION_LVGC", "LEY_FEDERAL_DEL_DERECHO_DE_AUTOR_LFDA",
                                 "LEY_FEDERAL_DE_ARMAS_DE_FUEGO_Y_EXPLOSIVOS_LFAFE", "LEY_DE_MIGRACION_LM",
                                 "LEY_GENERAL_DE_SALUD_LGS_CONTRA_LA_SALUD_EN_SU_MODALIDAD_DE_NARCOMENUDEO",
                                 "LEY_GENERAL_DE_SALUD_LGS_OTROS_DELITOS_PREVISTOS_EN_LA_LGS",
                                 "LEY_FEDERAL_CONTRA_LA_DELINCUENCIA_ORGANIZADA_LFCDO_CONTRA_LA_SALUD",
                                 "LEY_FEDERAL_CONTRA_LA_DELINCUENCIA_ORGANIZADA_LFCDO_OTROS_DELITOS_PREVISTOS_EN_LA_LFCDO",
                                 "LEYES_DE_INSTITUCIONES_DE_CREDITO_INVERSION_FIANZAS_Y_SEGUROS",
                                 "OTRAS_LEYES_ESPECIALES")

data_2012_2015_06 <- read_csv("INCIDENCIA DELICTIVA FEDERAL 2012-Junio 2015.csv", col_names = nombres_ajustados_2012_2015, skip = 1)
data_2012_2015_06 <- as.data.frame(data_2012_2015_06)

#------------- Clean up
# "M\u0090XICO"
valueToBeReplaced <- "M\u0090XICO"
newValue <- "MEXICO"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "M�XICO",
valueToBeReplaced <- "M�XICO"
newValue <- "MEXICO"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "MICHOAC�N,
valueToBeReplaced <- "MICHOAC�N"
newValue <- "MICHOACAN"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "NUEVO LE�N
valueToBeReplaced <- "NUEVO LE�N"
newValue <- "NUEVO LEON"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# QUERETARO
valueToBeReplaced <- "QUER�TARO"
newValue <- "QUERETARO"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "QUER\u0090TARO"
valueToBeReplaced <- "QUER\u0090TARO"
newValue <- "QUERETARO"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# SAN LUIS
valueToBeReplaced <- "SAN LUIS POTOS�"
newValue <- "SAN LUIS POTOSI"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# YUCATAN
valueToBeReplaced <- "YUCAT�N"
newValue <- "YUCATAN"
data_2012_2015_06$DELEGACION <- str_replace_all(data_2012_2015_06$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

data_2012_2015_06 <- data_2012_2015_06 %>% mutate(YEAR = as.numeric(substr(FECHA,1,4)),
                                            MONTH = as.numeric(substr(FECHA, nchar(FECHA)-1, nchar(FECHA))),
                                            DATE = as.Date(ISOdate(YEAR, MONTH, 1)))

data_2012_2015_06$FECHA <- NULL
data_2012_2015_06$YEAR <- NULL
data_2012_2015_06$MONTH <- NULL
data_2012_2015_06$CLAVE_INEGI_AGEE <- NULL
data_2012_2015_06 <- data_2012_2015_06 %>% gather("DELITO", "TOTAL", 2:(ncol(data_2012_2015_06)-1))
theData <- bind_rows(data_2012_2015_06, theData)
remove(data_2012_2015_06, nombres_ajustados_2012_2015)

# --------CUARTA PARTE (2007-2011)--------------------------------

nombres_ajustados_2015_2017 <- c("FECHA", "DELEGACION", "CLAVE_INEGI_AGEE",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_PRODUCCION",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_TRANSPORTE",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_TRAFICO",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_COMERCIO",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_SUMINISTRO",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_POSESION",
                                 "CODIGO_PENAL_FEDERAL_CPF_DELITOS_CONTRA_LA_SALUD_OTROS",
                                 "CODIGO_PENAL_FEDERAL_CPF_COMETIDOS_POR_SERVIDORES_PUBLICOS",
                                 "CODIGO_PENAL_FEDERAL_CPF_CONTRA_EL_AMBIENTE_Y_LA_GESTION_AMBIENTAL",
                                 "CODIGO_PENAL_FEDERAL_CPF_CONTRA_LA_INTEGRIDAD_CORPORAL",
                                 "CODIGO_PENAL_FEDERAL_CPF_ELECTORALES",
                                 "CODIGO_PENAL_FEDERAL_CPF_EN_MATERIA_DE_DERECHOS_DE_AUTOR",
                                 "CODIGO_PENAL_FEDERAL_CPF_FALSEDAD_TITULO_DECIMO_TERCERO",
                                 "CODIGO_PENAL_FEDERAL_CPF_PATRIMONIALES",
                                 "CODIGO_PENAL_FEDERAL_CPF_VIAS_DE_COMUNICACION_Y_CORRESPONDENCIA",
                                 "CODIGO_PENAL_FEDERAL_CPF_OTROS_DELITOS_DEL_CPF",
                                 "CODIGO_FISCAL_DE_LA_FEDERACION_CFF",
                                 "LEY_DE_LA_PROPIEDAD_INDUSTRIAL_LPI",
                                 "LEY_DE_VIAS_GENERALES_DE_COMUNICACION_LVGC",
                                 "LEY_FEDERAL_DEL_DERECHO_DE_AUTOR_LFDA",
                                 "LEY_FEDERAL_DE_ARMAS_DE_FUEGO_Y_EXPLOSIVOS_LFAFE",
                                 "LEY_DE_MIGRACION_LM",
                                 "LEY_GENERAL_DE_SALUD_LGS_CONTRA_LA_SALUD_EN_SU_MODALIDAD_DE_NARCOMENUDEO",
                                 "LEY_GENERAL_DE_SALUD_LGS_OTROS_DELITOS_PREVISTOS_EN_LA_LGS",
                                 "LEY_FEDERAL_CONTRA_LA_DELINCUENCIA_ORGANIZADA_LFCDO_CONTRA_LA_SALUD",
                                 "LEY_FEDERAL_CONTRA_LA_DELINCUENCIA_ORGANIZADA_LFCDO_OTROS_DELITOS_PREVISTOS_EN_LA_LFCDO",
                                 "LEYES_DE_INSTITUCIONES_DE_CREDITO_INVERSION_FIANZAS_Y_SEGUROS",
                                 "OTRAS_LEYES_ESPECIALES")

data_2015_2017 <- read_csv("Incidencia Delictiva Federal 2015 - 2017.csv", col_names = nombres_ajustados_2015_2017, skip = 1)
data_2015_2017 <- as.data.frame(data_2015_2017)

#------------- Clean up
# "M\u0090XICO"
valueToBeReplaced <- "M\u0090XICO"
newValue <- "MEXICO"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "M�XICO",
valueToBeReplaced <- "M�XICO"
newValue <- "MEXICO"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "MICHOAC�N,
valueToBeReplaced <- "MICHOAC�N"
newValue <- "MICHOACAN"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "NUEVO LE�N
valueToBeReplaced <- "NUEVO LE�N"
newValue <- "NUEVO LEON"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# QUERETARO
valueToBeReplaced <- "QUER�TARO"
newValue <- "QUERETARO"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "QUER\u0090TARO"
valueToBeReplaced <- "QUER\u0090TARO"
newValue <- "QUERETARO"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# SAN LUIS
valueToBeReplaced <- "SAN LUIS POTOS�"
newValue <- "SAN LUIS POTOSI"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# YUCATAN
valueToBeReplaced <- "YUCAT�N"
newValue <- "YUCATAN"
data_2015_2017$DELEGACION <- str_replace_all(data_2015_2017$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

data_2015_2017 <- data_2015_2017 %>% mutate(YEAR = as.numeric(substr(FECHA,1,4)),
                                                  MONTH = as.numeric(substr(FECHA, nchar(FECHA)-1, nchar(FECHA))),
                                                  DATE = as.Date(ISOdate(YEAR, MONTH, 1)))

data_2015_2017$FECHA <- NULL
data_2015_2017$YEAR <- NULL
data_2015_2017$MONTH <- NULL
data_2015_2017$CLAVE_INEGI_AGEE <- NULL
data_2015_2017 <- data_2015_2017 %>% gather("DELITO", "TOTAL", 2:(ncol(data_2015_2017)-1))
theData <- bind_rows(data_2015_2017, theData)
remove(data_2015_2017, nombres_ajustados_2015_2017)


#------------- Last clean up
# "SAN LUIS POTOSÖ"
valueToBeReplaced <- "SAN LUIS POTOSÖ"
newValue <- "SAN LUIS POTOSI"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "SAN LUIS POTOSÍ"
valueToBeReplaced <- "SAN LUIS POTOSÍ"
newValue <- "SAN LUIS POTOSI"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "DISTRITO FEDERAL"
valueToBeReplaced <- "DISTRITO FEDERAL"
newValue <- "CIUDAD DE MEXICO"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "CIUDAD DE MEXICO"
valueToBeReplaced <- "CIUDAD DE MÉXICO"
newValue <- "CIUDAD DE MEXICO"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "MICHOACÁN"
valueToBeReplaced <- "MICHOACÁN"
newValue <- "MICHOACAN"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "QUERÉTARO"
valueToBeReplaced <- "QUERÉTARO"
newValue <- "QUERETARO"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "YUCATÁN"
valueToBeReplaced <- "YUCATÁN"
newValue <- "YUCATAN"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "MÉXICO"
valueToBeReplaced <- "^MÉXICO$"
newValue <- "ESTADO DE MEXICO"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "MEXICO"
valueToBeReplaced <- "^MEXICO$"
newValue <- "ESTADO DE MEXICO"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "ESTADO DE MEXICO"
valueToBeReplaced <- "ESTADO DE MÉXICO"
newValue <- "ESTADO DE MEXICO"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# "NUEVO LEÓN"
valueToBeReplaced <- "NUEVO LEÓN"
newValue <- "NUEVO LEON"
theData$DELEGACION <- str_replace_all(theData$DELEGACION, valueToBeReplaced, newValue)
remove(valueToBeReplaced, newValue)

# GET REFERENCES
delitos <- readRDS("delitos.rds")

# COMBINE INFORMATION
theData <- left_join(theData, delitos)

# EXPORT TO EXCEL
write_xlsx(theData, path = "theData.xlsx")

# SAVE DATA
saveRDS(theData, file = "theData.rds")
# delitos <- unique(theData$DELITO)
# delitos <- as.data.frame(delitos)
# write_xlsx(delitos, path = "delitos.xlsx")
# delitos <- read_excel("delitos.xlsx")
# REMOVE
rm(list = ls())
