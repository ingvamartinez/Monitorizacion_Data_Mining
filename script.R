library(dplyr)
library(tidyr)
library(stringr)

# Obtenemos la Data
data_path <-"~/Monitorizacion_y_Data_Mining-VIU/Actividades/Actividad_3/epa-http (1)/epa-http.csv"
data<-read.csv(data_path, col.names = c('Request IP','Request_DATE', 'Request_Solicitud', 'Status', 'Size') , sep=" ", header = FALSE)
# Relizamos la siguientes operaciones para obtener un DF con todos los datos
data_sol<-as.data.frame(data$V3)
data_sol <- str_split_fixed(data$Request_Solicitud," ",3)
data_sol <- as.data.frame(data_sol)
data_sol<- rename(data_sol,c(RequestType = V1, RequestUrl = V2, RequestProtocol = V3))
datafull<- cbind.data.frame(data,data_sol)


#Pregunta 2

print('Numero de Ips sin errores')
#Seleccionamos las variables que necesitamos
ip_errors<- subset(datafull, select=c(Request.IP,Status))
ip_errors<- as.data.frame(ip_errors)
#Agregamos una columna para clasificar si tiene error
ip_errors$error <- ifelse(ip_errors$Status != 200,1,0)
ip_errors<-as.data.frame(subset(ip_errors,error == 1))
g_errors<- ip_errors %>%  group_by(Request.IP) %>% 
   summarise(sum = sum(error))
#Realizamos la suma aritmetica para obtener la cantidad de los usuarios con errores y sin errores
IP_error<- length(unique(g_errors$Request.IP))
IP_total<- length(unique(datafull$Request.IP))
IP_noerror <- IP_total-IP_error
print(paste('Usuarios Totales=',IP_total))
print(paste('Usuarios sin errores=', IP_noerror))
print(paste('Usuarios con errores=', IP_error))


#Pregunta 3

#Analizar los distintos tipos de peticiones HTTP (GET, POST, PUT, DELETE) 
#gestionadas por el servidor, identificando la frecuencia de cada una de estas.

frec_Prot_http<- data.frame(table(datafull$RequestType))

#Repetir el análisis, esta vez filtrando previamente aquellas peticiones 
#correspondientes a recursos ofrecidos de tipo imagen.

tipo_imagen<- grepl(pattern = ".*[png|jpg|gif|ico]$", datafull$RequestUrl)
tipo_imagen<- as.data.frame(tipo_imagen)
datafull2<-cbind.data.frame(datafull,tipo_imagen)
table_df2<- datafull2 %>% select(RequestType,tipo_imagen)
table_df2<- as.data.frame(subset(table_df2,tipo_imagen == TRUE))
frec_prot_img<- data.frame(table(table_df2$RequestType))
                     