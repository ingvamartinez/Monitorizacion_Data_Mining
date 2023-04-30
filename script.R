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
# Realizamos las conversiones necesarias para tener las variables con el tipo de dato deseado

#Pregunta 2
print('Numero de Ips sin errores')
ip_errors<- subset(datafull, select=c(Request.IP,Status))
ip_errors<- as.data.frame(ip_errors)
ip_errors$error <- ifelse(ip_errors$Status != 200,1,0)
ip_errors<-as.data.frame(subset(ip_errors,error == 1))
g_errors<- ip_errors %>%  group_by(Request.IP) %>% 
   summarise(sum = sum(error))

IP_error<- length(unique(g_errors$Request.IP))
IP_total<- length(unique(datafull$Request.IP))
IP_noerror <- IP_total-IP_error
print(paste('Usuarios Totales=',IP_total))
print(paste('Usuarios sin errores=', IP_noerror))
print(paste('Usuarios con errores=', IP_error))