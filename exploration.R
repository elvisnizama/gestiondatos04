download.file("https://www.datosabiertos.gob.pe/sites/default/files/DATASET_DE_DATOS_LLAMADAS_CENTRAL_EMERGENCIAS.csv", "emergencias_db.csv")

library(readr)
df <-read.csv("emergencias_db.csv", sep=";")

#1) Explorando el dataframe
df$num_llamadas <- 1
head(df)
nrow(df)
length(df)
colnames(df)
summary(df)
str(df)


#2) #Cambiar el formato de las columnas con valores fecha
df$FECHA_LLAMADA <- as.Date(as.character(df$FECHA_LLAMADA ), "%Y%m%d")
df$FECHA_CORTE <- as.Date(as.character(df$FECHA_CORTE ), "%Y%m%d")
str(df[c('FECHA_LLAMADA','FECHA_CORTE')])

aggregate(df["num_llamadas"], by=df["FECHA_CORTE"], sum)
aggregate(df["num_llamadas"], by=df["FECHA_LLAMADA"], sum)

#3) Agrupar las llamadas por ZONA
df |> group_by(ZONA) %>% 
  summarise_at(vars(num_llamadas),
               list(llamadas = sum))



#4) Verificar valores únicos en cada columna
#Se elimina aquellas columnas con poca variabilidad 
#Son aquellas que tienen uno o dos valores únicos y no aportan mucha información
#unique(df_detail[, c("AA_HH")])
df %>% drop_na()
nrow(df)
df_detail <-  df[, c("UNIDAD", "TIPI_TIPO_DETE", "DIANOMBRE", "TURNO","DETE_CANTIDAD", "AA_HH","num_llamadas")]
df_detail <-df_detail[!(df_detail$AA_HH=="" | df_detail$AA_HH=="NULL"),]
nrow(df_detail)

df_ubicacion <- unique(df[, c("ZONA", "SECTOR", "SUBSECTOR", "AA_HH")])
df_ubicacion <-df_ubicacion[!(df_ubicacion$ZONA=="" | df_ubicacion$ZONA=="NULL"),]
nrow(df_ubicacion)
head(df_ubicacion[,c("ZONA","AA_HH")])



#4) Verificar valores únicos en cada columna
valoresUnicos <- function(df_){ 
  for (col in colnames(df_)) {
    unique_count <- length(unique(df_[[col]]))
    cat("Column", col, "tiene", unique_count, "valores únicos\n")
  }
}
valoresUnicos(df_detail)
valoresUnicos(df_ubicacion)

##############################
#1) adicional 1: 
agg_tbl <- df_detail %>% group_by(DIANOMBRE, TURNO) %>% 
  summarise(across(num_llamadas, sum))

agg_tbl
nrow(agg_tbl)

new_wide_data <- agg_tbl |> 
  pivot_wider(names_from = TURNO, values_from = num_llamadas)
nrow(new_wide_data)


