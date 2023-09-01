library(readxl)
library(tidyverse)
#Primero escojemos la ruta con el siguiente comando
file.choose()
#luego en la variable ruta_excel copiamos lo que nos resulto de file.choose
ruta_excel <- "C:\\Users\\viciv\\OneDrive\\Documents\\1002-A\\Servicio social\\Trabajos de R\\Para git\\Estres.xlsx"
#importamos la hoja deseada que queremos en este caso es la hoja de las puntuaciones normales
puntuaciones <- read_excel(ruta_excel,
                           sheet = 'PuntuacionesN')
#importamos la hoja deseada que queremos en este caso es la hoja de las puntuacionesT
puntuacionesT <- read_excel(ruta_excel,
                            sheet = 'PuntuacionesTN')
#Esto es para mostrar los dataframes que se han creado
puntuaciones
puntuacionesT
#Grupos
PCIC <- puntuacionesT %>% 
  filter(Grupo == 'PCIC-001' | Grupo == 'PCIC-002' | Grupo == 'PCIC-003')
PCIC

PCIM <- puntuacionesT %>% 
  filter(Grupo== 'PCIM-001' | Grupo== 'PCIM-002' | Grupo== 'PCIM-003' | Grupo== 'PCIM-004')
PCIM
PCIMA <- puntuacionesT %>% 
  filter(Grupo== 'PCIMA-001' | Grupo== 'PCIMA-002' | Grupo== 'PCIMA-003')
PCIMA

PCID <- puntuacionesT %>% 
  filter(Grupo== 'PCID-001' | Grupo== 'PCID-002' | Grupo== 'PCID-003' | Grupo== 'PCID-004')
PCID

PCLCE <- puntuacionesT %>% 
  filter(Grupo== 'PCLCE-001' | Grupo== 'PCLCE-002' | Grupo== 'PCLCE-003')
PCLCE

PCIE <- puntuacionesT %>% 
  filter(Grupo== 'PCIE-001')
PCIE

PCICV <-  puntuacionesT %>% 
  filter(Grupo== 'PCICV-001' | Grupo== 'PCICV-002' | Grupo== 'PCICV-003')
PCICV

PCII <-  puntuacionesT %>% 
  filter(Grupo== 'PCII-001' | Grupo== 'PCII-002')
PCII

PCIFA <- puntuacionesT %>% 
  filter(Grupo== 'PCIFA-001')
PCIFA

PCLMA <- puntuacionesT %>% 
  filter(Grupo== 'PCLMA-001')
PCLMA

#Genero
HOMBRES <- puntuacionesT %>% 
  filter(Genero== 'Masculino')
HOMBRES

MUJERES <- puntuacionesT %>% 
  filter(Genero== 'Femenino')
MUJERES

#Region
CANADA <- puntuacionesT %>% 
  filter(Region== 'Cañada')
CANADA

COSTA <- puntuacionesT %>% 
  filter(Region== 'Costa')
COSTA

ISTMO <- puntuacionesT %>% 
  filter(Region== 'Istmo')
ISTMO

MIXTECA <- puntuacionesT %>% 
  filter(Region== 'Mixteca')
MIXTECA

PAPALOAPAN <- puntuacionesT %>% 
  filter(Region== 'Papaloapan')
PAPALOAPAN

SIERRANORTE <- puntuacionesT %>% 
  filter(Region== 'Sierra Norte')
SIERRANORTE

SIERRASUR <- puntuacionesT %>% 
  filter(Region== 'Sierra Sur')
SIERRASUR

VALLESCENTRALES <- puntuacionesT %>% 
  filter(Region== 'Valles Centrales')
VALLESCENTRALES

OTRO <- puntuacionesT %>% 
  filter(Region== 'Otro')
OTRO

#Grafica gausiana para cada grupo, respecto a cada aspecto
#x <- seq(min(PCIC$`Puntuacion T Estres`), max(PCIC$`Puntuacion T Estres`), length = 1000)
#densidad <- dnorm(x, mean = mean(PCIC$`Puntuacion T Estres`), sd = sd(PCIC$`Puntuacion T Estres`))
#plot(x, densidad, type = "l", col = rgb(0.8,0.4,0.1,0.7), lwd = 2,
 #    main = "Estres",
  #   xlab = "Valor", ylab = "Densidad")

#Ahora vamosa comparar las graficas de cada grupo en cada aspecto
#Estres
x <- seq(min(PCIC$`Puntuacion T Estres`, 
             PCIM$`Puntuacion T Estres`, 
             PCIMA$`Puntuacion T Estres`,
             PCID$`Puntuacion T Estres`,
             PCLCE$`Puntuacion T Estres`,
             PCIE$`Puntuacion T Estres`,
             PCICV$`Puntuacion T Estres`,
             PCII$`Puntuacion T Estres`,
             PCIFA$`Puntuacion T Estres`,
             PCLMA$`Puntuacion T Estres`), 
         max(PCIC$`Puntuacion T Estres`, 
             PCIM$`Puntuacion T Estres`, 
             PCIMA$`Puntuacion T Estres`,
             PCID$`Puntuacion T Estres`,
             PCLCE$`Puntuacion T Estres`,
             PCIE$`Puntuacion T Estres`,
             PCICV$`Puntuacion T Estres`,
             PCII$`Puntuacion T Estres`,
             PCIFA$`Puntuacion T Estres`,
             PCLMA$`Puntuacion T Estres`), length = 1000)
densidad_T1 <- dnorm(x, mean = mean(PCIC$`Puntuacion T Estres`), sd = sd(PCIC$`Puntuacion T Estres`))
densidad_T2 <- dnorm(x, mean = mean(PCIM$`Puntuacion T Estres`), sd = sd(PCIM$`Puntuacion T Estres`))
densidad_T3 <- dnorm(x, mean = mean(PCIMA$`Puntuacion T Estres`), sd = sd(PCIMA$`Puntuacion T Estres`))
densidad_T4 <- dnorm(x, mean = mean(PCID$`Puntuacion T Estres`), sd = sd(PCID$`Puntuacion T Estres`))
densidad_T5 <- dnorm(x, mean = mean(PCLCE$`Puntuacion T Estres`), sd = sd(PCLCE$`Puntuacion T Estres`))
densidad_T6 <- dnorm(x, mean = mean(PCIE$`Puntuacion T Estres`), sd = sd(PCIE$`Puntuacion T Estres`))
densidad_T7 <- dnorm(x, mean = mean(PCICV$`Puntuacion T Estres`), sd = sd(PCICV$`Puntuacion T Estres`))
densidad_T8 <- dnorm(x, mean = mean(PCII$`Puntuacion T Estres`), sd = sd(PCII$`Puntuacion T Estres`))
densidad_T9 <- dnorm(x, mean = mean(PCIFA$`Puntuacion T Estres`), sd = sd(PCIFA$`Puntuacion T Estres`))
densidad_T10 <- dnorm(x, mean = mean(PCLMA$`Puntuacion T Estres`), sd = sd(PCIMA$`Puntuacion T Estres`))

plot(x, densidad_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación estres por carrera",
     xlab = "Valor", ylab = "Densidad")
lines(x, densidad_T2, col = "red", lwd = 2)
lines(x, densidad_T3, col = "green", lwd = 2)
lines(x, densidad_T4, col = "#AB47BC", lwd = 2)
lines(x, densidad_T5, col = "#009688", lwd = 2)
lines(x, densidad_T6, col = "#FFEB3B", lwd = 2)
lines(x, densidad_T7, col = "#FB8C00", lwd = 2)
lines(x, densidad_T8, col = "#6D4C41", lwd = 2)
lines(x, densidad_T9, col = "#9E9E9E", lwd = 2)
lines(x, densidad_T10, col = "#263238", lwd = 2)
legend("topright", legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2)






#Salud
x2 <- seq(min(PCIC$`Puntuacion T Habitos de salud`, 
             PCIM$`Puntuacion T Habitos de salud`, 
             PCIMA$`Puntuacion T Habitos de salud`,
             PCID$`Puntuacion T Habitos de salud`,
             PCLCE$`Puntuacion T Habitos de salud`,
             PCIE$`Puntuacion T Habitos de salud`,
             PCICV$`Puntuacion T Habitos de salud`,
             PCII$`Puntuacion T Habitos de salud`,
             PCIFA$`Puntuacion T Habitos de salud`,
             PCLMA$`Puntuacion T Habitos de salud`), 
         max(PCIC$`Puntuacion T Habitos de salud`, 
             PCIM$`Puntuacion T Habitos de salud`, 
             PCIMA$`Puntuacion T Habitos de salud`,
             PCID$`Puntuacion T Habitos de salud`,
             PCLCE$`Puntuacion T Habitos de salud`,
             PCIE$`Puntuacion T Habitos de salud`,
             PCICV$`Puntuacion T Habitos de salud`,
             PCII$`Puntuacion T Habitos de salud`,
             PCIFA$`Puntuacion T Habitos de salud`,
             PCLMA$`Puntuacion T Habitos de salud`), length = 1000)
densidad2_T1 <- dnorm(x2, mean = mean(PCIC$`Puntuacion T Habitos de salud`), sd = sd(PCIC$`Puntuacion T Habitos de salud`))
densidad2_T2 <- dnorm(x2, mean = mean(PCIM$`Puntuacion T Habitos de salud`), sd = sd(PCIM$`Puntuacion T Habitos de salud`))
densidad2_T3 <- dnorm(x2, mean = mean(PCIMA$`Puntuacion T Habitos de salud`), sd = sd(PCIMA$`Puntuacion T Habitos de salud`))
densidad2_T4 <- dnorm(x2, mean = mean(PCID$`Puntuacion T Habitos de salud`), sd = sd(PCID$`Puntuacion T Habitos de salud`))
densidad2_T5 <- dnorm(x2, mean = mean(PCLCE$`Puntuacion T Habitos de salud`), sd = sd(PCLCE$`Puntuacion T Habitos de salud`))
densidad2_T6 <- dnorm(x2, mean = mean(PCIE$`Puntuacion T Habitos de salud`), sd = sd(PCIE$`Puntuacion T Habitos de salud`))
densidad2_T7 <- dnorm(x2, mean = mean(PCICV$`Puntuacion T Habitos de salud`), sd = sd(PCICV$`Puntuacion T Habitos de salud`))
densidad2_T8 <- dnorm(x2, mean = mean(PCII$`Puntuacion T Habitos de salud`), sd = sd(PCII$`Puntuacion T Habitos de salud`))
densidad2_T9 <- dnorm(x2, mean = mean(PCIFA$`Puntuacion T Habitos de salud`), sd = sd(PCIFA$`Puntuacion T Habitos de salud`))
densidad2_T10 <- dnorm(x2, mean = mean(PCLMA$`Puntuacion T Habitos de salud`), sd = sd(PCIMA$`Puntuacion T Habitos de salud`))

#Se ajustan los limites de los ejes
xlim <- c(min(x2), max(x2))
ylim <- c(0, max(densidad2_T1, densidad2_T2, densidad2_T3, densidad2_T4, densidad2_T5,
                 densidad2_T6, densidad2_T7, densidad2_T8, densidad2_T9, densidad2_T10) * 1.2)

plot(x2, densidad2_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de salud por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim, ylim = ylim)
lines(x2, densidad2_T2, col = "red", lwd = 2)
lines(x2, densidad2_T3, col = "green", lwd = 2)
lines(x2, densidad2_T4, col = "#AB47BC", lwd = 2)
lines(x2, densidad2_T5, col = "#009688", lwd = 2)
lines(x2, densidad2_T6, col = "#FFEB3B", lwd = 2)
lines(x2, densidad2_T7, col = "#FB8C00", lwd = 2)
lines(x2, densidad2_T8, col = "#6D4C41", lwd = 2)
lines(x2, densidad2_T9, col = "#9E9E9E", lwd = 2)
lines(x2, densidad2_T10, col = "#263238", lwd = 2)
legend("topright", legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")

#Ejercicio
x3 <- seq(min(PCIC$`Puntuacion T Ejercicio`, 
              PCIM$`Puntuacion T Ejercicio`, 
              PCIMA$`Puntuacion T Ejercicio`,
              PCID$`Puntuacion T Ejercicio`,
              PCLCE$`Puntuacion T Ejercicio`,
              PCIE$`Puntuacion T Ejercicio`,
              PCICV$`Puntuacion T Ejercicio`,
              PCII$`Puntuacion T Ejercicio`,
              PCIFA$`Puntuacion T Ejercicio`,
              PCLMA$`Puntuacion T Ejercicio`), 
          max(PCIC$`Puntuacion T Ejercicio`, 
              PCIM$`Puntuacion T Ejercicio`, 
              PCIMA$`Puntuacion T Ejercicio`,
              PCID$`Puntuacion T Ejercicio`,
              PCLCE$`Puntuacion T Ejercicio`,
              PCIE$`Puntuacion T Ejercicio`,
              PCICV$`Puntuacion T Ejercicio`,
              PCII$`Puntuacion T Ejercicio`,
              PCIFA$`Puntuacion T Ejercicio`,
              PCLMA$`Puntuacion T Ejercicio`), length = 1000)
densidad3_T1 <- dnorm(x3, mean = mean(PCIC$`Puntuacion T Ejercicio`), sd = sd(PCIC$`Puntuacion T Ejercicio`))
densidad3_T2 <- dnorm(x3, mean = mean(PCIM$`Puntuacion T Ejercicio`), sd = sd(PCIM$`Puntuacion T Ejercicio`))
densidad3_T3 <- dnorm(x3, mean = mean(PCIMA$`Puntuacion T Ejercicio`), sd = sd(PCIMA$`Puntuacion T Ejercicio`))
densidad3_T4 <- dnorm(x3, mean = mean(PCID$`Puntuacion T Ejercicio`), sd = sd(PCID$`Puntuacion T Ejercicio`))
densidad3_T5 <- dnorm(x3, mean = mean(PCLCE$`Puntuacion T Ejercicio`), sd = sd(PCLCE$`Puntuacion T Ejercicio`))
densidad3_T6 <- dnorm(x3, mean = mean(PCIE$`Puntuacion T Ejercicio`), sd = sd(PCIE$`Puntuacion T Ejercicio`))
densidad3_T7 <- dnorm(x3, mean = mean(PCICV$`Puntuacion T Ejercicio`), sd = sd(PCICV$`Puntuacion T Ejercicio`))
densidad3_T8 <- dnorm(x3, mean = mean(PCII$`Puntuacion T Ejercicio`), sd = sd(PCII$`Puntuacion T Ejercicio`))
densidad3_T9 <- dnorm(x3, mean = mean(PCIFA$`Puntuacion T Ejercicio`), sd = sd(PCIFA$`Puntuacion T Ejercicio`))
densidad3_T10 <- dnorm(x3, mean = mean(PCLMA$`Puntuacion T Ejercicio`), sd = sd(PCIMA$`Puntuacion T Ejercicio`))

#Se ajustan los limites de los ejes
xlim3 <- c(min(x3), max(x3))
ylim3 <- c(0, max(densidad3_T1, densidad3_T2, densidad3_T3, densidad3_T4, densidad3_T5,
                 densidad3_T6, densidad3_T7, densidad3_T8, densidad3_T9, densidad3_T10) * 1.2)

plot(x3, densidad3_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de ejercicio por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim3, ylim = ylim3)
lines(x3, densidad3_T2, col = "red", lwd = 2)
lines(x3, densidad3_T3, col = "green", lwd = 2)
lines(x3, densidad3_T4, col = "#AB47BC", lwd = 2)
lines(x3, densidad3_T5, col = "#009688", lwd = 2)
lines(x3, densidad3_T6, col = "#FFEB3B", lwd = 2)
lines(x3, densidad3_T7, col = "#FB8C00", lwd = 2)
lines(x3, densidad3_T8, col = "#6D4C41", lwd = 2)
lines(x3, densidad3_T9, col = "#9E9E9E", lwd = 2)
lines(x3, densidad3_T10, col = "#263238", lwd = 2)
legend("topright", legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2)


#Descanso
x4 <- seq(min(PCIC$`Puntuacion T Descanso/sueño`, 
              PCIM$`Puntuacion T Descanso/sueño`, 
              PCIMA$`Puntuacion T Descanso/sueño`,
              PCID$`Puntuacion T Descanso/sueño`,
              PCLCE$`Puntuacion T Descanso/sueño`,
              PCIE$`Puntuacion T Descanso/sueño`,
              PCICV$`Puntuacion T Descanso/sueño`,
              PCII$`Puntuacion T Descanso/sueño`,
              PCIFA$`Puntuacion T Descanso/sueño`,
              PCLMA$`Puntuacion T Descanso/sueño`), 
          max(PCIC$`Puntuacion T Descanso/sueño`, 
              PCIM$`Puntuacion T Descanso/sueño`, 
              PCIMA$`Puntuacion T Descanso/sueño`,
              PCID$`Puntuacion T Descanso/sueño`,
              PCLCE$`Puntuacion T Descanso/sueño`,
              PCIE$`Puntuacion T Descanso/sueño`,
              PCICV$`Puntuacion T Descanso/sueño`,
              PCII$`Puntuacion T Descanso/sueño`,
              PCIFA$`Puntuacion T Descanso/sueño`,
              PCLMA$`Puntuacion T Descanso/sueño`), length = 1000)


densidad4_T1 <- dnorm(x4, mean = mean(PCIC$`Puntuacion T Descanso/sueño`), sd = sd(PCIC$`Puntuacion T Descanso/sueño`))
densidad4_T2 <- dnorm(x4, mean = mean(PCIM$`Puntuacion T Descanso/sueño`), sd = sd(PCIM$`Puntuacion T Descanso/sueño`))
densidad4_T3 <- dnorm(x4, mean = mean(PCIMA$`Puntuacion T Descanso/sueño`), sd = sd(PCIMA$`Puntuacion T Descanso/sueño`))
densidad4_T4 <- dnorm(x4, mean = mean(PCID$`Puntuacion T Descanso/sueño`), sd = sd(PCID$`Puntuacion T Descanso/sueño`))
densidad4_T5 <- dnorm(x4, mean = mean(PCLCE$`Puntuacion T Descanso/sueño`), sd = sd(PCLCE$`Puntuacion T Descanso/sueño`))
densidad4_T6 <- dnorm(x4, mean = mean(PCIE$`Puntuacion T Descanso/sueño`), sd = sd(PCIE$`Puntuacion T Descanso/sueño`))
densidad4_T7 <- dnorm(x4, mean = mean(PCICV$`Puntuacion T Descanso/sueño`), sd = sd(PCICV$`Puntuacion T Descanso/sueño`))
densidad4_T8 <- dnorm(x4, mean = mean(PCII$`Puntuacion T Descanso/sueño`), sd = sd(PCII$`Puntuacion T Descanso/sueño`))
densidad4_T9 <- dnorm(x4, mean = mean(PCIFA$`Puntuacion T Descanso/sueño`), sd = sd(PCIFA$`Puntuacion T Descanso/sueño`))
densidad4_T10 <- dnorm(x4, mean = mean(PCLMA$`Puntuacion T Descanso/sueño`), sd = sd(PCIMA$`Puntuacion T Descanso/sueño`))

#Se ajustan los limites de los ejes
xlim4 <- c(min(x4), max(x4))
ylim4 <- c(0, max(densidad4_T1, densidad4_T2, densidad4_T3, densidad4_T4, densidad4_T5,
                  densidad4_T6, densidad4_T7, densidad4_T8, densidad4_T9, densidad4_T10) * 1.2)

plot(x4, densidad4_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de descanso por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim4, ylim = ylim4)
lines(x4, densidad4_T2, col = "red", lwd = 2)
lines(x4, densidad4_T3, col = "green", lwd = 2)
lines(x4, densidad4_T4, col = "#AB47BC", lwd = 2)
lines(x4, densidad4_T5, col = "#009688", lwd = 2)
lines(x4, densidad4_T6, col = "#FFEB3B", lwd = 2)
lines(x4, densidad4_T7, col = "#FB8C00", lwd = 2)
lines(x4, densidad4_T8, col = "#6D4C41", lwd = 2)
lines(x4, densidad4_T9, col = "#9E9E9E", lwd = 2)
lines(x4, densidad4_T10, col = "#263238", lwd = 2)
legend("topright", legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.8, box.col = "black")






#alimentacion
x5 <- seq(min(PCIC$`Puntuacion T Alimentacion/Nutricion`, 
              PCIM$`Puntuacion T Alimentacion/Nutricion`, 
              PCIMA$`Puntuacion T Alimentacion/Nutricion`,
              PCID$`Puntuacion T Alimentacion/Nutricion`,
              PCLCE$`Puntuacion T Alimentacion/Nutricion`,
              PCIE$`Puntuacion T Alimentacion/Nutricion`,
              PCICV$`Puntuacion T Alimentacion/Nutricion`,
              PCII$`Puntuacion T Alimentacion/Nutricion`,
              PCIFA$`Puntuacion T Alimentacion/Nutricion`,
              PCLMA$`Puntuacion T Alimentacion/Nutricion`), 
          max(PCIC$`Puntuacion T Alimentacion/Nutricion`, 
              PCIM$`Puntuacion T Alimentacion/Nutricion`, 
              PCIMA$`Puntuacion T Alimentacion/Nutricion`,
              PCID$`Puntuacion T Alimentacion/Nutricion`,
              PCLCE$`Puntuacion T Alimentacion/Nutricion`,
              PCIE$`Puntuacion T Alimentacion/Nutricion`,
              PCICV$`Puntuacion T Alimentacion/Nutricion`,
              PCII$`Puntuacion T Alimentacion/Nutricion`,
              PCIFA$`Puntuacion T Alimentacion/Nutricion`,
              PCLMA$`Puntuacion T Alimentacion/Nutricion`), length = 1000)


densidad5_T1 <- dnorm(x5, mean = mean(PCIC$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCIC$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T2 <- dnorm(x5, mean = mean(PCIM$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCIM$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T3 <- dnorm(x5, mean = mean(PCIMA$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCIMA$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T4 <- dnorm(x5, mean = mean(PCID$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCID$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T5 <- dnorm(x5, mean = mean(PCLCE$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCLCE$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T6 <- dnorm(x5, mean = mean(PCIE$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCIE$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T7 <- dnorm(x5, mean = mean(PCICV$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCICV$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T8 <- dnorm(x5, mean = mean(PCII$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCII$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T9 <- dnorm(x5, mean = mean(PCIFA$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCIFA$`Puntuacion T Alimentacion/Nutricion`))
densidad5_T10 <- dnorm(x5, mean = mean(PCLMA$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PCIMA$`Puntuacion T Alimentacion/Nutricion`))

#Se ajustan los limites de los ejes
xlim5 <- c(min(x5), max(x5))
ylim5 <- c(0, max(densidad5_T1, densidad5_T2, densidad5_T3, densidad5_T4, densidad5_T5,
                  densidad5_T6, densidad5_T7, densidad5_T8, densidad5_T9, densidad5_T10) * 1.2)

plot(x5, densidad5_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de alimentacion/nutricion por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim5, ylim = ylim5)
lines(x5, densidad5_T2, col = "red", lwd = 2)
lines(x5, densidad5_T3, col = "green", lwd = 2)
lines(x5, densidad5_T4, col = "#AB47BC", lwd = 2)
lines(x5, densidad5_T5, col = "#009688", lwd = 2)
lines(x5, densidad5_T6, col = "#FFEB3B", lwd = 2)
lines(x5, densidad5_T7, col = "#FB8C00", lwd = 2)
lines(x5, densidad5_T8, col = "#6D4C41", lwd = 2)
lines(x5, densidad5_T9, col = "#9E9E9E", lwd = 2)
lines(x5, densidad5_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#Prevencion
x6 <- seq(min(PCIC$`Puntuacion T Prevencion`, 
              PCIM$`Puntuacion T Prevencion`, 
              PCIMA$`Puntuacion T Prevencion`,
              PCID$`Puntuacion T Prevencion`,
              PCLCE$`Puntuacion T Prevencion`,
              PCIE$`Puntuacion T Prevencion`,
              PCICV$`Puntuacion T Prevencion`,
              PCII$`Puntuacion T Prevencion`,
              PCIFA$`Puntuacion T Prevencion`,
              PCLMA$`Puntuacion T Prevencion`), 
          max(PCIC$`Puntuacion T Prevencion`, 
              PCIM$`Puntuacion T Prevencion`, 
              PCIMA$`Puntuacion T Prevencion`,
              PCID$`Puntuacion T Prevencion`,
              PCLCE$`Puntuacion T Prevencion`,
              PCIE$`Puntuacion T Prevencion`,
              PCICV$`Puntuacion T Prevencion`,
              PCII$`Puntuacion T Prevencion`,
              PCIFA$`Puntuacion T Prevencion`,
              PCLMA$`Puntuacion T Prevencion`), length = 1000)


densidad6_T1 <- dnorm(x6, mean = mean(PCIC$`Puntuacion T Prevencion`), sd = sd(PCIC$`Puntuacion T Prevencion`))
densidad6_T2 <- dnorm(x6, mean = mean(PCIM$`Puntuacion T Prevencion`), sd = sd(PCIM$`Puntuacion T Prevencion`))
densidad6_T3 <- dnorm(x6, mean = mean(PCIMA$`Puntuacion T Prevencion`), sd = sd(PCIMA$`Puntuacion T Prevencion`))
densidad6_T4 <- dnorm(x6, mean = mean(PCID$`Puntuacion T Prevencion`), sd = sd(PCID$`Puntuacion T Prevencion`))
densidad6_T5 <- dnorm(x6, mean = mean(PCLCE$`Puntuacion T Prevencion`), sd = sd(PCLCE$`Puntuacion T Prevencion`))
densidad6_T6 <- dnorm(x6, mean = mean(PCIE$`Puntuacion T Prevencion`), sd = sd(PCIE$`Puntuacion T Prevencion`))
densidad6_T7 <- dnorm(x6, mean = mean(PCICV$`Puntuacion T Prevencion`), sd = sd(PCICV$`Puntuacion T Prevencion`))
densidad6_T8 <- dnorm(x6, mean = mean(PCII$`Puntuacion T Prevencion`), sd = sd(PCII$`Puntuacion T Prevencion`))
densidad6_T9 <- dnorm(x6, mean = mean(PCIFA$`Puntuacion T Prevencion`), sd = sd(PCIFA$`Puntuacion T Prevencion`))
densidad6_T10 <- dnorm(x6, mean = mean(PCLMA$`Puntuacion T Prevencion`), sd = sd(PCIMA$`Puntuacion T Prevencion`))

#Se ajustan los limites de los ejes
xlim6 <- c(min(x6), max(x6))
ylim6 <- c(0, max(densidad6_T1, densidad6_T2, densidad6_T3, densidad6_T4, densidad6_T5,
                  densidad6_T6, densidad6_T7, densidad6_T8, densidad6_T9, densidad6_T10) * 1.2)

plot(x6, densidad6_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de prevencion por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim6, ylim = ylim6)
lines(x6, densidad6_T2, col = "red", lwd = 2)
lines(x6, densidad6_T3, col = "green", lwd = 2)
lines(x6, densidad6_T4, col = "#AB47BC", lwd = 2)
lines(x6, densidad6_T5, col = "#009688", lwd = 2)
lines(x6, densidad6_T6, col = "#FFEB3B", lwd = 2)
lines(x6, densidad6_T7, col = "#FB8C00", lwd = 2)
lines(x6, densidad6_T8, col = "#6D4C41", lwd = 2)
lines(x6, densidad6_T9, col = "#9E9E9E", lwd = 2)
lines(x6, densidad6_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Conglomerado reactivos
x7 <- seq(min(PCIC$`Puntuacion T Conglomerado de reactivos`, 
              PCIM$`Puntuacion T Conglomerado de reactivos`, 
              PCIMA$`Puntuacion T Conglomerado de reactivos`,
              PCID$`Puntuacion T Conglomerado de reactivos`,
              PCLCE$`Puntuacion T Conglomerado de reactivos`,
              PCIE$`Puntuacion T Conglomerado de reactivos`,
              PCICV$`Puntuacion T Conglomerado de reactivos`,
              PCII$`Puntuacion T Conglomerado de reactivos`,
              PCIFA$`Puntuacion T Conglomerado de reactivos`,
              PCLMA$`Puntuacion T Conglomerado de reactivos`), 
          max(PCIC$`Puntuacion T Conglomerado de reactivos`, 
              PCIM$`Puntuacion T Conglomerado de reactivos`, 
              PCIMA$`Puntuacion T Conglomerado de reactivos`,
              PCID$`Puntuacion T Conglomerado de reactivos`,
              PCLCE$`Puntuacion T Conglomerado de reactivos`,
              PCIE$`Puntuacion T Conglomerado de reactivos`,
              PCICV$`Puntuacion T Conglomerado de reactivos`,
              PCII$`Puntuacion T Conglomerado de reactivos`,
              PCIFA$`Puntuacion T Conglomerado de reactivos`,
              PCLMA$`Puntuacion T Conglomerado de reactivos`), length = 1000)


densidad7_T1 <- dnorm(x7, mean = mean(PCIC$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCIC$`Puntuacion T Conglomerado de reactivos`))
densidad7_T2 <- dnorm(x7, mean = mean(PCIM$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCIM$`Puntuacion T Conglomerado de reactivos`))
densidad7_T3 <- dnorm(x7, mean = mean(PCIMA$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCIMA$`Puntuacion T Conglomerado de reactivos`))
densidad7_T4 <- dnorm(x7, mean = mean(PCID$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCID$`Puntuacion T Conglomerado de reactivos`))
densidad7_T5 <- dnorm(x7, mean = mean(PCLCE$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCLCE$`Puntuacion T Conglomerado de reactivos`))
densidad7_T6 <- dnorm(x7, mean = mean(PCIE$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCIE$`Puntuacion T Conglomerado de reactivos`))
densidad7_T7 <- dnorm(x7, mean = mean(PCICV$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCICV$`Puntuacion T Conglomerado de reactivos`))
densidad7_T8 <- dnorm(x7, mean = mean(PCII$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCII$`Puntuacion T Conglomerado de reactivos`))
densidad7_T9 <- dnorm(x7, mean = mean(PCIFA$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCIFA$`Puntuacion T Conglomerado de reactivos`))
densidad7_T10 <- dnorm(x7, mean = mean(PCLMA$`Puntuacion T Conglomerado de reactivos`), sd = sd(PCIMA$`Puntuacion T Conglomerado de reactivos`))

#Se ajustan los limites de los ejes
xlim7 <- c(min(x7)-5, max(x7)+5)
ylim7 <- c(0, max(densidad7_T1, densidad7_T2, densidad7_T3, densidad7_T4, densidad7_T5,
                  densidad7_T6, densidad7_T7, densidad7_T8, densidad7_T9, densidad7_T10) * 1.4)

plot(x7, densidad7_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación conglomerado reactivos",
     xlab = "Valor", ylab = "Densidad", xlim = xlim7, ylim = ylim7)
lines(x7, densidad7_T2, col = "red", lwd = 2)
lines(x7, densidad7_T3, col = "green", lwd = 2)
lines(x7, densidad7_T4, col = "#AB47BC", lwd = 2)
lines(x7, densidad7_T5, col = "#009688", lwd = 2)
lines(x7, densidad7_T6, col = "#FFEB3B", lwd = 2)
lines(x7, densidad7_T7, col = "#FB8C00", lwd = 2)
lines(x7, densidad7_T8, col = "#6D4C41", lwd = 2)
lines(x7, densidad7_T9, col = "#9E9E9E", lwd = 2)
lines(x7, densidad7_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")






#Conglomerado reactivos
x8 <- seq(min(PCIC$`Puntuacion T Red de apoyo social`, 
              PCIM$`Puntuacion T Red de apoyo social`, 
              PCIMA$`Puntuacion T Red de apoyo social`,
              PCID$`Puntuacion T Red de apoyo social`,
              PCLCE$`Puntuacion T Red de apoyo social`,
              PCIE$`Puntuacion T Red de apoyo social`,
              PCICV$`Puntuacion T Red de apoyo social`,
              PCII$`Puntuacion T Red de apoyo social`,
              PCIFA$`Puntuacion T Red de apoyo social`,
              PCLMA$`Puntuacion T Red de apoyo social`), 
          max(PCIC$`Puntuacion T Red de apoyo social`, 
              PCIM$`Puntuacion T Red de apoyo social`, 
              PCIMA$`Puntuacion T Red de apoyo social`,
              PCID$`Puntuacion T Red de apoyo social`,
              PCLCE$`Puntuacion T Red de apoyo social`,
              PCIE$`Puntuacion T Red de apoyo social`,
              PCICV$`Puntuacion T Red de apoyo social`,
              PCII$`Puntuacion T Red de apoyo social`,
              PCIFA$`Puntuacion T Red de apoyo social`,
              PCLMA$`Puntuacion T Red de apoyo social`), length = 1000)


densidad8_T1 <- dnorm(x8, mean = mean(PCIC$`Puntuacion T Red de apoyo social`), sd = sd(PCIC$`Puntuacion T Red de apoyo social`))
densidad8_T2 <- dnorm(x8, mean = mean(PCIM$`Puntuacion T Red de apoyo social`), sd = sd(PCIM$`Puntuacion T Red de apoyo social`))
densidad8_T3 <- dnorm(x8, mean = mean(PCIMA$`Puntuacion T Red de apoyo social`), sd = sd(PCIMA$`Puntuacion T Red de apoyo social`))
densidad8_T4 <- dnorm(x8, mean = mean(PCID$`Puntuacion T Red de apoyo social`), sd = sd(PCID$`Puntuacion T Red de apoyo social`))
densidad8_T5 <- dnorm(x8, mean = mean(PCLCE$`Puntuacion T Red de apoyo social`), sd = sd(PCLCE$`Puntuacion T Red de apoyo social`))
densidad8_T6 <- dnorm(x8, mean = mean(PCIE$`Puntuacion T Red de apoyo social`), sd = sd(PCIE$`Puntuacion T Red de apoyo social`))
densidad8_T7 <- dnorm(x8, mean = mean(PCICV$`Puntuacion T Red de apoyo social`), sd = sd(PCICV$`Puntuacion T Red de apoyo social`))
densidad8_T8 <- dnorm(x8, mean = mean(PCII$`Puntuacion T Red de apoyo social`), sd = sd(PCII$`Puntuacion T Red de apoyo social`))
densidad8_T9 <- dnorm(x8, mean = mean(PCIFA$`Puntuacion T Red de apoyo social`), sd = sd(PCIFA$`Puntuacion T Red de apoyo social`))
densidad8_T10 <- dnorm(x8, mean = mean(PCLMA$`Puntuacion T Red de apoyo social`), sd = sd(PCIMA$`Puntuacion T Red de apoyo social`))
#Se ajustan los limites de los ejes
xlim8 <- c(min(x8), max(x8))
ylim8 <- c(0, max(densidad8_T1, densidad8_T2, densidad8_T3, densidad8_T4, densidad8_T5,
                  densidad8_T6, densidad8_T7, densidad8_T8, densidad8_T9, densidad8_T10) * 1.2)

plot(x8, densidad8_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación red de apoyo socical por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim7, ylim = ylim7)
lines(x8, densidad8_T2, col = "red", lwd = 2)
lines(x8, densidad8_T3, col = "green", lwd = 2)
lines(x8, densidad8_T4, col = "#AB47BC", lwd = 2)
lines(x8, densidad8_T5, col = "#009688", lwd = 2)
lines(x8, densidad8_T6, col = "#FFEB3B", lwd = 2)
lines(x8, densidad8_T7, col = "#FB8C00", lwd = 2)
lines(x8, densidad8_T8, col = "#6D4C41", lwd = 2)
lines(x8, densidad8_T9, col = "#9E9E9E", lwd = 2)
lines(x8, densidad8_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Conducta tipo A
x9 <- seq(min(PCIC$`Puntuacion T Conducta tipo A`, 
              PCIM$`Puntuacion T Conducta tipo A`, 
              PCIMA$`Puntuacion T Conducta tipo A`,
              PCID$`Puntuacion T Conducta tipo A`,
              PCLCE$`Puntuacion T Conducta tipo A`,
              PCIE$`Puntuacion T Conducta tipo A`,
              PCICV$`Puntuacion T Conducta tipo A`,
              PCII$`Puntuacion T Conducta tipo A`,
              PCIFA$`Puntuacion T Conducta tipo A`,
              PCLMA$`Puntuacion T Conducta tipo A`), 
          max(PCIC$`Puntuacion T Conducta tipo A`, 
              PCIM$`Puntuacion T Conducta tipo A`, 
              PCIMA$`Puntuacion T Conducta tipo A`,
              PCID$`Puntuacion T Conducta tipo A`,
              PCLCE$`Puntuacion T Conducta tipo A`,
              PCIE$`Puntuacion T Conducta tipo A`,
              PCICV$`Puntuacion T Conducta tipo A`,
              PCII$`Puntuacion T Conducta tipo A`,
              PCIFA$`Puntuacion T Conducta tipo A`,
              PCLMA$`Puntuacion T Conducta tipo A`), length = 1000)


densidad9_T1 <- dnorm(x9, mean = mean(PCIC$`Puntuacion T Conducta tipo A`), sd = sd(PCIC$`Puntuacion T Conducta tipo A`))
densidad9_T2 <- dnorm(x9, mean = mean(PCIM$`Puntuacion T Conducta tipo A`), sd = sd(PCIM$`Puntuacion T Conducta tipo A`))
densidad9_T3 <- dnorm(x9, mean = mean(PCIMA$`Puntuacion T Conducta tipo A`), sd = sd(PCIMA$`Puntuacion T Conducta tipo A`))
densidad9_T4 <- dnorm(x9, mean = mean(PCID$`Puntuacion T Conducta tipo A`), sd = sd(PCID$`Puntuacion T Conducta tipo A`))
densidad9_T5 <- dnorm(x9, mean = mean(PCLCE$`Puntuacion T Conducta tipo A`), sd = sd(PCLCE$`Puntuacion T Conducta tipo A`))
densidad9_T6 <- dnorm(x9, mean = mean(PCIE$`Puntuacion T Conducta tipo A`), sd = sd(PCIE$`Puntuacion T Conducta tipo A`))
densidad9_T7 <- dnorm(x9, mean = mean(PCICV$`Puntuacion T Conducta tipo A`), sd = sd(PCICV$`Puntuacion T Conducta tipo A`))
densidad9_T8 <- dnorm(x9, mean = mean(PCII$`Puntuacion T Conducta tipo A`), sd = sd(PCII$`Puntuacion T Conducta tipo A`))
densidad9_T9 <- dnorm(x9, mean = mean(PCIFA$`Puntuacion T Conducta tipo A`), sd = sd(PCIFA$`Puntuacion T Conducta tipo A`))
densidad9_T10 <- dnorm(x9, mean = mean(PCLMA$`Puntuacion T Conducta tipo A`), sd = sd(PCIMA$`Puntuacion T Conducta tipo A`))

#Se ajustan los limites de los ejes
xlim9 <- c(min(x9), max(x9))
ylim9 <- c(0, max(densidad9_T1, densidad9_T2, densidad9_T3, densidad9_T4, densidad9_T5,
                  densidad9_T6, densidad9_T7, densidad9_T8, densidad9_T9, densidad9_T10) * 1.2)

plot(x9, densidad9_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación conducta tipo A por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim9, ylim = ylim9)
lines(x9, densidad9_T2, col = "red", lwd = 2)
lines(x9, densidad9_T3, col = "green", lwd = 2)
lines(x9, densidad9_T4, col = "#AB47BC", lwd = 2)
lines(x9, densidad9_T5, col = "#009688", lwd = 2)
lines(x9, densidad9_T6, col = "#FFEB3B", lwd = 2)
lines(x9, densidad9_T7, col = "#FB8C00", lwd = 2)
lines(x9, densidad9_T8, col = "#6D4C41", lwd = 2)
lines(x9, densidad9_T9, col = "#9E9E9E", lwd = 2)
lines(x9, densidad9_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")






#Fuerza cognitiva
x10 <- seq(min(PCIC$`Puntuacion T Fuerza cognitiva`, 
              PCIM$`Puntuacion T Fuerza cognitiva`, 
              PCIMA$`Puntuacion T Fuerza cognitiva`,
              PCID$`Puntuacion T Fuerza cognitiva`,
              PCLCE$`Puntuacion T Fuerza cognitiva`,
              PCIE$`Puntuacion T Fuerza cognitiva`,
              PCICV$`Puntuacion T Fuerza cognitiva`,
              PCII$`Puntuacion T Fuerza cognitiva`,
              PCIFA$`Puntuacion T Fuerza cognitiva`,
              PCLMA$`Puntuacion T Fuerza cognitiva`), 
          max(PCIC$`Puntuacion T Fuerza cognitiva`, 
              PCIM$`Puntuacion T Fuerza cognitiva`, 
              PCIMA$`Puntuacion T Fuerza cognitiva`,
              PCID$`Puntuacion T Fuerza cognitiva`,
              PCLCE$`Puntuacion T Fuerza cognitiva`,
              PCIE$`Puntuacion T Fuerza cognitiva`,
              PCICV$`Puntuacion T Fuerza cognitiva`,
              PCII$`Puntuacion T Fuerza cognitiva`,
              PCIFA$`Puntuacion T Fuerza cognitiva`,
              PCLMA$`Puntuacion T Fuerza cognitiva`), length = 1000)


densidad10_T1 <- dnorm(x10, mean = mean(PCIC$`Puntuacion T Fuerza cognitiva`), sd = sd(PCIC$`Puntuacion T Fuerza cognitiva`))
densidad10_T2 <- dnorm(x10, mean = mean(PCIM$`Puntuacion T Fuerza cognitiva`), sd = sd(PCIM$`Puntuacion T Fuerza cognitiva`))
densidad10_T3 <- dnorm(x10, mean = mean(PCIMA$`Puntuacion T Fuerza cognitiva`), sd = sd(PCIMA$`Puntuacion T Fuerza cognitiva`))
densidad10_T4 <- dnorm(x10, mean = mean(PCID$`Puntuacion T Fuerza cognitiva`), sd = sd(PCID$`Puntuacion T Fuerza cognitiva`))
densidad10_T5 <- dnorm(x10, mean = mean(PCLCE$`Puntuacion T Fuerza cognitiva`), sd = sd(PCLCE$`Puntuacion T Fuerza cognitiva`))
densidad10_T6 <- dnorm(x10, mean = mean(PCIE$`Puntuacion T Fuerza cognitiva`), sd = sd(PCIE$`Puntuacion T Fuerza cognitiva`))
densidad10_T7 <- dnorm(x10, mean = mean(PCICV$`Puntuacion T Fuerza cognitiva`), sd = sd(PCICV$`Puntuacion T Fuerza cognitiva`))
densidad10_T8 <- dnorm(x10, mean = mean(PCII$`Puntuacion T Fuerza cognitiva`), sd = sd(PCII$`Puntuacion T Fuerza cognitiva`))
densidad10_T9 <- dnorm(x10, mean = mean(PCIFA$`Puntuacion T Fuerza cognitiva`), sd = sd(PCIFA$`Puntuacion T Fuerza cognitiva`))
densidad10_T10 <- dnorm(x10, mean = mean(PCLMA$`Puntuacion T Fuerza cognitiva`), sd = sd(PCIMA$`Puntuacion T Fuerza cognitiva`))

#Se ajustan los limites de los ejes
xlim10 <- c(min(x10), max(x10))
ylim10 <- c(0, max(densidad10_T1, densidad10_T2, densidad10_T3, densidad10_T4, densidad10_T5,
                  densidad10_T6, densidad10_T7, densidad10_T8, densidad10_T9, densidad10_T10) * 1.2)

plot(x10, densidad10_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación fuerza cognitiva por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim10, ylim = ylim10)
lines(x10, densidad10_T2, col = "red", lwd = 2)
lines(x10, densidad10_T3, col = "green", lwd = 2)
lines(x10, densidad10_T4, col = "#AB47BC", lwd = 2)
lines(x10, densidad10_T5, col = "#009688", lwd = 2)
lines(x10, densidad10_T6, col = "#FFEB3B", lwd = 2)
lines(x10, densidad10_T7, col = "#FB8C00", lwd = 2)
lines(x10, densidad10_T8, col = "#6D4C41", lwd = 2)
lines(x10, densidad10_T9, col = "#9E9E9E", lwd = 2)
lines(x10, densidad10_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")








#Valoracion positiva
x11 <- seq(min(PCIC$`Puntuacion T Valoracion positiva`, 
               PCIM$`Puntuacion T Valoracion positiva`, 
               PCIMA$`Puntuacion T Valoracion positiva`,
               PCID$`Puntuacion T Valoracion positiva`,
               PCLCE$`Puntuacion T Valoracion positiva`,
               PCIE$`Puntuacion T Valoracion positiva`,
               PCICV$`Puntuacion T Valoracion positiva`,
               PCII$`Puntuacion T Valoracion positiva`,
               PCIFA$`Puntuacion T Valoracion positiva`,
               PCLMA$`Puntuacion T Valoracion positiva`), 
           max(PCIC$`Puntuacion T Valoracion positiva`, 
               PCIM$`Puntuacion T Valoracion positiva`, 
               PCIMA$`Puntuacion T Valoracion positiva`,
               PCID$`Puntuacion T Valoracion positiva`,
               PCLCE$`Puntuacion T Valoracion positiva`,
               PCIE$`Puntuacion T Valoracion positiva`,
               PCICV$`Puntuacion T Valoracion positiva`,
               PCII$`Puntuacion T Valoracion positiva`,
               PCIFA$`Puntuacion T Valoracion positiva`,
               PCLMA$`Puntuacion T Valoracion positiva`), length = 1000)


densidad11_T1 <- dnorm(x11, mean = mean(PCIC$`Puntuacion T Valoracion positiva`), sd = sd(PCIC$`Puntuacion T Valoracion positiva`))
densidad11_T2 <- dnorm(x11, mean = mean(PCIM$`Puntuacion T Valoracion positiva`), sd = sd(PCIM$`Puntuacion T Valoracion positiva`))
densidad11_T3 <- dnorm(x11, mean = mean(PCIMA$`Puntuacion T Valoracion positiva`), sd = sd(PCIMA$`Puntuacion T Valoracion positiva`))
densidad11_T4 <- dnorm(x11, mean = mean(PCID$`Puntuacion T Valoracion positiva`), sd = sd(PCID$`Puntuacion T Valoracion positiva`))
densidad11_T5 <- dnorm(x11, mean = mean(PCLCE$`Puntuacion T Valoracion positiva`), sd = sd(PCLCE$`Puntuacion T Valoracion positiva`))
densidad11_T6 <- dnorm(x11, mean = mean(PCIE$`Puntuacion T Valoracion positiva`), sd = sd(PCIE$`Puntuacion T Valoracion positiva`))
densidad11_T7 <- dnorm(x11, mean = mean(PCICV$`Puntuacion T Valoracion positiva`), sd = sd(PCICV$`Puntuacion T Valoracion positiva`))
densidad11_T8 <- dnorm(x11, mean = mean(PCII$`Puntuacion T Valoracion positiva`), sd = sd(PCII$`Puntuacion T Valoracion positiva`))
densidad11_T9 <- dnorm(x11, mean = mean(PCIFA$`Puntuacion T Valoracion positiva`), sd = sd(PCIFA$`Puntuacion T Valoracion positiva`))
densidad11_T10 <- dnorm(x11, mean = mean(PCLMA$`Puntuacion T Valoracion positiva`), sd = sd(PCIMA$`Puntuacion T Valoracion positiva`))

#Se ajustan los limites de los ejes
xlim11 <- c(min(x11), max(x11))
ylim11 <- c(0, max(densidad11_T1, densidad11_T2, densidad11_T3, densidad11_T4, densidad11_T5,
                   densidad11_T6, densidad11_T7, densidad11_T8, densidad11_T9, densidad11_T10) * 1.2)

plot(x11, densidad11_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación valoracion positiva por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim11, ylim = ylim11)
lines(x11, densidad11_T2, col = "red", lwd = 2)
lines(x11, densidad11_T3, col = "green", lwd = 2)
lines(x11, densidad11_T4, col = "#AB47BC", lwd = 2)
lines(x11, densidad11_T5, col = "#009688", lwd = 2)
lines(x11, densidad11_T6, col = "#FFEB3B", lwd = 2)
lines(x11, densidad11_T7, col = "#FB8C00", lwd = 2)
lines(x11, densidad11_T8, col = "#6D4C41", lwd = 2)
lines(x11, densidad11_T9, col = "#9E9E9E", lwd = 2)
lines(x11, densidad11_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Valoracion negativa
x12 <- seq(min(PCIC$`Puntuacion T Valoracion negativa`, 
               PCIM$`Puntuacion T Valoracion negativa`, 
               PCIMA$`Puntuacion T Valoracion negativa`,
               PCID$`Puntuacion T Valoracion negativa`,
               PCLCE$`Puntuacion T Valoracion negativa`,
               PCIE$`Puntuacion T Valoracion negativa`,
               PCICV$`Puntuacion T Valoracion negativa`,
               PCII$`Puntuacion T Valoracion negativa`,
               PCIFA$`Puntuacion T Valoracion negativa`,
               PCLMA$`Puntuacion T Valoracion negativa`), 
           max(PCIC$`Puntuacion T Valoracion negativa`, 
               PCIM$`Puntuacion T Valoracion negativa`, 
               PCIMA$`Puntuacion T Valoracion negativa`,
               PCID$`Puntuacion T Valoracion negativa`,
               PCLCE$`Puntuacion T Valoracion negativa`,
               PCIE$`Puntuacion T Valoracion negativa`,
               PCICV$`Puntuacion T Valoracion negativa`,
               PCII$`Puntuacion T Valoracion negativa`,
               PCIFA$`Puntuacion T Valoracion negativa`,
               PCLMA$`Puntuacion T Valoracion negativa`), length = 1000)


densidad12_T1 <- dnorm(x12, mean = mean(PCIC$`Puntuacion T Valoracion negativa`), sd = sd(PCIC$`Puntuacion T Valoracion negativa`))
densidad12_T2 <- dnorm(x12, mean = mean(PCIM$`Puntuacion T Valoracion negativa`), sd = sd(PCIM$`Puntuacion T Valoracion negativa`))
densidad12_T3 <- dnorm(x12, mean = mean(PCIMA$`Puntuacion T Valoracion negativa`), sd = sd(PCIMA$`Puntuacion T Valoracion negativa`))
densidad12_T4 <- dnorm(x12, mean = mean(PCID$`Puntuacion T Valoracion negativa`), sd = sd(PCID$`Puntuacion T Valoracion negativa`))
densidad12_T5 <- dnorm(x12, mean = mean(PCLCE$`Puntuacion T Valoracion negativa`), sd = sd(PCLCE$`Puntuacion T Valoracion negativa`))
densidad12_T6 <- dnorm(x12, mean = mean(PCIE$`Puntuacion T Valoracion negativa`), sd = sd(PCIE$`Puntuacion T Valoracion negativa`))
densidad12_T7 <- dnorm(x12, mean = mean(PCICV$`Puntuacion T Valoracion negativa`), sd = sd(PCICV$`Puntuacion T Valoracion negativa`))
densidad12_T8 <- dnorm(x12, mean = mean(PCII$`Puntuacion T Valoracion negativa`), sd = sd(PCII$`Puntuacion T Valoracion negativa`))
densidad12_T9 <- dnorm(x12, mean = mean(PCIFA$`Puntuacion T Valoracion negativa`), sd = sd(PCIFA$`Puntuacion T Valoracion negativa`))
densidad12_T10 <- dnorm(x12, mean = mean(PCLMA$`Puntuacion T Valoracion negativa`), sd = sd(PCIMA$`Puntuacion T Valoracion negativa`))

#Se ajustan los limites de los ejes
xlim12 <- c(min(x12), max(x12))
ylim12 <- c(0, max(densidad12_T1, densidad12_T2, densidad12_T3, densidad12_T4, densidad12_T5,
                   densidad12_T6, densidad12_T7, densidad12_T8, densidad12_T9, densidad12_T10) * 1.2)

plot(x12, densidad12_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación valoracion negativa por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim12, ylim = ylim12)
lines(x12, densidad12_T2, col = "red", lwd = 2)
lines(x12, densidad12_T3, col = "green", lwd = 2)
lines(x12, densidad12_T4, col = "#AB47BC", lwd = 2)
lines(x12, densidad12_T5, col = "#009688", lwd = 2)
lines(x12, densidad12_T6, col = "#FFEB3B", lwd = 2)
lines(x12, densidad12_T7, col = "#FB8C00", lwd = 2)
lines(x12, densidad12_T8, col = "#6D4C41", lwd = 2)
lines(x12, densidad12_T9, col = "#9E9E9E", lwd = 2)
lines(x12, densidad12_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Minimizacion de la amenaza
x13<- seq(min(PCIC$`Puntuacion T Minimizacion de la amenaza`, 
               PCIM$`Puntuacion T Minimizacion de la amenaza`, 
               PCIMA$`Puntuacion T Minimizacion de la amenaza`,
               PCID$`Puntuacion T Minimizacion de la amenaza`,
               PCLCE$`Puntuacion T Minimizacion de la amenaza`,
               PCIE$`Puntuacion T Minimizacion de la amenaza`,
               PCICV$`Puntuacion T Minimizacion de la amenaza`,
               PCII$`Puntuacion T Minimizacion de la amenaza`,
               PCIFA$`Puntuacion T Minimizacion de la amenaza`,
               PCLMA$`Puntuacion T Minimizacion de la amenaza`), 
           max(PCIC$`Puntuacion T Minimizacion de la amenaza`, 
               PCIM$`Puntuacion T Minimizacion de la amenaza`, 
               PCIMA$`Puntuacion T Minimizacion de la amenaza`,
               PCID$`Puntuacion T Minimizacion de la amenaza`,
               PCLCE$`Puntuacion T Minimizacion de la amenaza`,
               PCIE$`Puntuacion T Minimizacion de la amenaza`,
               PCICV$`Puntuacion T Minimizacion de la amenaza`,
               PCII$`Puntuacion T Minimizacion de la amenaza`,
               PCIFA$`Puntuacion T Minimizacion de la amenaza`,
               PCLMA$`Puntuacion T Minimizacion de la amenaza`), length = 1000)


densidad13_T1 <- dnorm(x13, mean = mean(PCIC$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCIC$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T2 <- dnorm(x13, mean = mean(PCIM$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCIM$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T3 <- dnorm(x13, mean = mean(PCIMA$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCIMA$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T4 <- dnorm(x13, mean = mean(PCID$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCID$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T5 <- dnorm(x13, mean = mean(PCLCE$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCLCE$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T6 <- dnorm(x13, mean = mean(PCIE$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCIE$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T7 <- dnorm(x13, mean = mean(PCICV$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCICV$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T8 <- dnorm(x13, mean = mean(PCII$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCII$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T9 <- dnorm(x13, mean = mean(PCIFA$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCIFA$`Puntuacion T Minimizacion de la amenaza`))
densidad13_T10 <- dnorm(x13, mean = mean(PCLMA$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PCIMA$`Puntuacion T Minimizacion de la amenaza`))

#Se ajustan los limites de los ejes
xlim13 <- c(min(x13), max(x13))
ylim13 <- c(0, max(densidad13_T1, densidad13_T2, densidad13_T3, densidad13_T4, densidad13_T5,
                   densidad13_T6, densidad13_T7, densidad13_T8, densidad13_T9, densidad13_T10) * 1.2)

plot(x13, densidad13_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación minimizacion de la amenaza por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim13, ylim = ylim13)
lines(x13, densidad13_T2, col = "red", lwd = 2)
lines(x13, densidad13_T3, col = "green", lwd = 2)
lines(x13, densidad13_T4, col = "#AB47BC", lwd = 2)
lines(x13, densidad13_T5, col = "#009688", lwd = 2)
lines(x13, densidad13_T6, col = "#FFEB3B", lwd = 2)
lines(x13, densidad13_T7, col = "#FB8C00", lwd = 2)
lines(x13, densidad13_T8, col = "#6D4C41", lwd = 2)
lines(x13, densidad13_T9, col = "#9E9E9E", lwd = 2)
lines(x13, densidad13_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")






#Concentracion en el problema
x14<- seq(min(PCIC$`Puntuacion T Concentracion en el problema`, 
              PCIM$`Puntuacion T Concentracion en el problema`, 
              PCIMA$`Puntuacion T Concentracion en el problema`,
              PCID$`Puntuacion T Concentracion en el problema`,
              PCLCE$`Puntuacion T Concentracion en el problema`,
              PCIE$`Puntuacion T Concentracion en el problema`,
              PCICV$`Puntuacion T Concentracion en el problema`,
              PCII$`Puntuacion T Concentracion en el problema`,
              PCIFA$`Puntuacion T Concentracion en el problema`,
              PCLMA$`Puntuacion T Concentracion en el problema`), 
          max(PCIC$`Puntuacion T Concentracion en el problema`, 
              PCIM$`Puntuacion T Concentracion en el problema`, 
              PCIMA$`Puntuacion T Concentracion en el problema`,
              PCID$`Puntuacion T Concentracion en el problema`,
              PCLCE$`Puntuacion T Concentracion en el problema`,
              PCIE$`Puntuacion T Concentracion en el problema`,
              PCICV$`Puntuacion T Concentracion en el problema`,
              PCII$`Puntuacion T Concentracion en el problema`,
              PCIFA$`Puntuacion T Concentracion en el problema`,
              PCLMA$`Puntuacion T Concentracion en el problema`), length = 1000)


densidad14_T1 <- dnorm(x14, mean = mean(PCIC$`Puntuacion T Concentracion en el problema`), sd = sd(PCIC$`Puntuacion T Concentracion en el problema`))
densidad14_T2 <- dnorm(x14, mean = mean(PCIM$`Puntuacion T Concentracion en el problema`), sd = sd(PCIM$`Puntuacion T Concentracion en el problema`))
densidad14_T3 <- dnorm(x14, mean = mean(PCIMA$`Puntuacion T Concentracion en el problema`), sd = sd(PCIMA$`Puntuacion T Concentracion en el problema`))
densidad14_T4 <- dnorm(x14, mean = mean(PCID$`Puntuacion T Concentracion en el problema`), sd = sd(PCID$`Puntuacion T Concentracion en el problema`))
densidad14_T5 <- dnorm(x14, mean = mean(PCLCE$`Puntuacion T Concentracion en el problema`), sd = sd(PCLCE$`Puntuacion T Concentracion en el problema`))
densidad14_T6 <- dnorm(x14, mean = mean(PCIE$`Puntuacion T Concentracion en el problema`), sd = sd(PCIE$`Puntuacion T Concentracion en el problema`))
densidad14_T7 <- dnorm(x14, mean = mean(PCICV$`Puntuacion T Concentracion en el problema`), sd = sd(PCICV$`Puntuacion T Concentracion en el problema`))
densidad14_T8 <- dnorm(x14, mean = mean(PCII$`Puntuacion T Concentracion en el problema`), sd = sd(PCII$`Puntuacion T Concentracion en el problema`))
densidad14_T9 <- dnorm(x14, mean = mean(PCIFA$`Puntuacion T Concentracion en el problema`), sd = sd(PCIFA$`Puntuacion T Concentracion en el problema`))
densidad14_T10 <- dnorm(x14, mean = mean(PCLMA$`Puntuacion T Concentracion en el problema`), sd = sd(PCIMA$`Puntuacion T Concentracion en el problema`))

#Se ajustan los limites de los ejes
xlim14 <- c(min(x14), max(x14))
ylim14 <- c(0, max(densidad14_T1, densidad14_T2, densidad14_T3, densidad14_T4, densidad14_T5,
                   densidad14_T6, densidad14_T7, densidad14_T8, densidad14_T9, densidad14_T10) * 1.2)

plot(x14, densidad14_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación concentracion en el problema por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim14, ylim = ylim14)
lines(x14, densidad14_T2, col = "red", lwd = 2)
lines(x14, densidad14_T3, col = "green", lwd = 2)
lines(x14, densidad14_T4, col = "#AB47BC", lwd = 2)
lines(x14, densidad14_T5, col = "#009688", lwd = 2)
lines(x14, densidad14_T6, col = "#FFEB3B", lwd = 2)
lines(x14, densidad14_T7, col = "#FB8C00", lwd = 2)
lines(x14, densidad14_T8, col = "#6D4C41", lwd = 2)
lines(x14, densidad14_T9, col = "#9E9E9E", lwd = 2)
lines(x14, densidad14_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")








#Bienestar psicologico
x15<- seq(min(PCIC$`Puntuacion T Bienestar psicologico`, 
              PCIM$`Puntuacion T Bienestar psicologico`, 
              PCIMA$`Puntuacion T Bienestar psicologico`,
              PCID$`Puntuacion T Bienestar psicologico`,
              PCLCE$`Puntuacion T Bienestar psicologico`,
              PCIE$`Puntuacion T Bienestar psicologico`,
              PCICV$`Puntuacion T Bienestar psicologico`,
              PCII$`Puntuacion T Bienestar psicologico`,
              PCIFA$`Puntuacion T Bienestar psicologico`,
              PCLMA$`Puntuacion T Bienestar psicologico`), 
          max(PCIC$`Puntuacion T Bienestar psicologico`, 
              PCIM$`Puntuacion T Bienestar psicologico`, 
              PCIMA$`Puntuacion T Bienestar psicologico`,
              PCID$`Puntuacion T Bienestar psicologico`,
              PCLCE$`Puntuacion T Bienestar psicologico`,
              PCIE$`Puntuacion T Bienestar psicologico`,
              PCICV$`Puntuacion T Bienestar psicologico`,
              PCII$`Puntuacion T Bienestar psicologico`,
              PCIFA$`Puntuacion T Bienestar psicologico`,
              PCLMA$`Puntuacion T Bienestar psicologico`), length = 1000)


densidad15_T1 <- dnorm(x15, mean = mean(PCIC$`Puntuacion T Concentracion en el problema`), sd = sd(PCIC$`Puntuacion T Concentracion en el problema`))
densidad15_T2 <- dnorm(x15, mean = mean(PCIM$`Puntuacion T Concentracion en el problema`), sd = sd(PCIM$`Puntuacion T Concentracion en el problema`))
densidad15_T3 <- dnorm(x15, mean = mean(PCIMA$`Puntuacion T Concentracion en el problema`), sd = sd(PCIMA$`Puntuacion T Concentracion en el problema`))
densidad15_T4 <- dnorm(x15, mean = mean(PCID$`Puntuacion T Concentracion en el problema`), sd = sd(PCID$`Puntuacion T Concentracion en el problema`))
densidad15_T5 <- dnorm(x15, mean = mean(PCLCE$`Puntuacion T Concentracion en el problema`), sd = sd(PCLCE$`Puntuacion T Concentracion en el problema`))
densidad15_T6 <- dnorm(x15, mean = mean(PCIE$`Puntuacion T Concentracion en el problema`), sd = sd(PCIE$`Puntuacion T Concentracion en el problema`))
densidad15_T7 <- dnorm(x15, mean = mean(PCICV$`Puntuacion T Concentracion en el problema`), sd = sd(PCICV$`Puntuacion T Concentracion en el problema`))
densidad15_T8 <- dnorm(x15, mean = mean(PCII$`Puntuacion T Concentracion en el problema`), sd = sd(PCII$`Puntuacion T Concentracion en el problema`))
densidad15_T9 <- dnorm(x15, mean = mean(PCIFA$`Puntuacion T Concentracion en el problema`), sd = sd(PCIFA$`Puntuacion T Concentracion en el problema`))
densidad15_T10 <- dnorm(x15, mean = mean(PCLMA$`Puntuacion T Concentracion en el problema`), sd = sd(PCIMA$`Puntuacion T Concentracion en el problema`))

#Se ajustan los limites de los ejes
xlim15 <- c(min(x15), max(x15))
ylim15 <- c(0, max(densidad15_T1, densidad15_T2, densidad15_T3, densidad15_T4, densidad15_T5,
                   densidad15_T6, densidad15_T7, densidad15_T8, densidad15_T9, densidad15_T10) * 1.2)

plot(x15, densidad15_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación bienestar psicologico por carrera",
     xlab = "Valor", ylab = "Densidad", xlim = xlim15, ylim = ylim15)
lines(x15, densidad15_T2, col = "red", lwd = 2)
lines(x15, densidad15_T3, col = "green", lwd = 2)
lines(x15, densidad15_T4, col = "#AB47BC", lwd = 2)
lines(x15, densidad15_T5, col = "#009688", lwd = 2)
lines(x15, densidad15_T6, col = "#FFEB3B", lwd = 2)
lines(x15, densidad15_T7, col = "#FB8C00", lwd = 2)
lines(x15, densidad15_T8, col = "#6D4C41", lwd = 2)
lines(x15, densidad15_T9, col = "#9E9E9E", lwd = 2)
lines(x15, densidad15_T10, col = "#263238", lwd = 2)
legend("topright",legend = c("PCIC", "PCIM", "PCIMA","PCID","PCLCE","PCIE","PCICV","PCII","PCIFA","PCLMA"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E","#263238"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")







#Estres por sexo
s <- seq(min(HOMBRES$`Puntuacion T Estres`, 
              MUJERES$`Puntuacion T Estres`), 
          max(HOMBRES$`Puntuacion T Estres`, 
              MUJERES$`Puntuacion T Estres`), length = 1000)
densidadS_T1 <- dnorm(s, mean = mean(HOMBRES$`Puntuacion T Estres`), sd = sd(HOMBRES$`Puntuacion T Estres`))
densidadS_T2 <- dnorm(s, mean = mean(MUJERES$`Puntuacion T Estres`), sd = sd(MUJERES$`Puntuacion T Estres`))


#Se ajustan los limites de los ejes
xlimS <- c(min(s), max(s))
ylimS <- c(0, max(densidadS_T1, densidadS_T2) * 1.2)

plot(s, densidadS_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de estres por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS, ylim = ylimS)
lines(s, densidadS_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")

#Salud por sexo
s2 <- seq(min(HOMBRES$`Puntuacion T Habitos de salud`, 
             MUJERES$`Puntuacion T Habitos de salud`), 
         max(HOMBRES$`Puntuacion T Habitos de salud`, 
             MUJERES$`Puntuacion T Habitos de salud`), length = 1000)
densidadS2_T1 <- dnorm(s, mean = mean(HOMBRES$`Puntuacion T Habitos de salud`), sd = sd(HOMBRES$`Puntuacion T Habitos de salud`))
densidadS2_T2 <- dnorm(s, mean = mean(MUJERES$`Puntuacion T Habitos de salud`), sd = sd(MUJERES$`Puntuacion T Habitos de salud`))


#Se ajustan los limites de los ejes
xlimS2 <- c(min(s2), max(s2))
ylimS2 <- c(0, max(densidadS2_T1, densidadS2_T2) * 1.2)

plot(s2, densidadS2_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de habitos de salud por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS2, ylim = ylimS2)
lines(s2, densidadS2_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")


#Ejercicio por sexo
s3 <- seq(min(HOMBRES$`Puntuacion T Ejercicio`, 
              MUJERES$`Puntuacion T Ejercicio`), 
          max(HOMBRES$`Puntuacion T Ejercicio`, 
              MUJERES$`Puntuacion T Ejercicio`), length = 1000)
densidadS3_T1 <- dnorm(s3, mean = mean(HOMBRES$`Puntuacion T Ejercicio`), sd = sd(HOMBRES$`Puntuacion T Ejercicio`))
densidadS3_T2 <- dnorm(s3, mean = mean(MUJERES$`Puntuacion T Ejercicio`), sd = sd(MUJERES$`Puntuacion T Ejercicio`))


#Se ajustan los limites de los ejes
xlimS3 <- c(min(s3), max(s3))
ylimS3 <- c(0, max(densidadS3_T1, densidadS3_T2) * 1.2)

plot(s3, densidadS3_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de habitos de ejercicio por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS3, ylim = ylimS3)
lines(s3, densidadS3_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")


#Descanso por sexo
s4 <- seq(min(HOMBRES$`Puntuacion T Descanso/sueño`, 
              MUJERES$`Puntuacion T Descanso/sueño`), 
          max(HOMBRES$`Puntuacion T Descanso/sueño`, 
              MUJERES$`Puntuacion T Descanso/sueño`), length = 1000)
densidadS4_T1 <- dnorm(s4, mean = mean(HOMBRES$`Puntuacion T Descanso/sueño`), sd = sd(HOMBRES$`Puntuacion T Descanso/sueño`))
densidadS4_T2 <- dnorm(s4, mean = mean(MUJERES$`Puntuacion T Descanso/sueño`), sd = sd(MUJERES$`Puntuacion T Descanso/sueño`))


#Se ajustan los limites de los ejes
xlimS4 <- c(min(s4), max(s4))
ylimS4 <- c(0, max(densidadS4_T1, densidadS4_T2) * 1.2)

plot(s4, densidadS4_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de habitos de sueño por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS4, ylim = ylimS4)
lines(s4, densidadS4_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")



#Alimentacion por sexo
s5 <- seq(min(HOMBRES$`Puntuacion T Alimentacion/Nutricion`, 
              MUJERES$`Puntuacion T Alimentacion/Nutricion`), 
          max(HOMBRES$`Puntuacion T Alimentacion/Nutricion`, 
              MUJERES$`Puntuacion T Alimentacion/Nutricion`), length = 1000)
densidadS5_T1 <- dnorm(s5, mean = mean(HOMBRES$`Puntuacion T Alimentacion/Nutricion`), sd = sd(HOMBRES$`Puntuacion T Alimentacion/Nutricion`))
densidadS5_T2 <- dnorm(s5, mean = mean(MUJERES$`Puntuacion T Alimentacion/Nutricion`), sd = sd(MUJERES$`Puntuacion T Alimentacion/Nutricion`))


#Se ajustan los limites de los ejes
xlimS5 <- c(min(s5), max(s5))
ylimS5 <- c(0, max(densidadS5_T1, densidadS5_T2) * 1.2)

plot(s5, densidadS5_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de habitos de alimentacion por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS5, ylim = ylimS5)
lines(s5, densidadS5_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Prevencion por sexo
s6 <- seq(min(HOMBRES$`Puntuacion T Prevencion`, 
              MUJERES$`Puntuacion T Prevencion`), 
          max(HOMBRES$`Puntuacion T Prevencion`, 
              MUJERES$`Puntuacion T Prevencion`), length = 1000)
densidadS6_T1 <- dnorm(s6, mean = mean(HOMBRES$`Puntuacion T Prevencion`), sd = sd(HOMBRES$`Puntuacion T Prevencion`))
densidadS6_T2 <- dnorm(s6, mean = mean(MUJERES$`Puntuacion T Prevencion`), sd = sd(MUJERES$`Puntuacion T Prevencion`))


#Se ajustan los limites de los ejes
xlimS6 <- c(min(s6), max(s6))
ylimS6 <- c(0, max(densidadS6_T1, densidadS6_T2) * 1.2)

plot(s6, densidadS6_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de habitos de prevencion por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS6, ylim = ylimS6)
lines(s6, densidadS6_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")






#Conglomerado de reactivos por sexo
s7 <- seq(min(HOMBRES$`Puntuacion T Conglomerado de reactivos`, 
              MUJERES$`Puntuacion T Conglomerado de reactivos`), 
          max(HOMBRES$`Puntuacion T Conglomerado de reactivos`, 
              MUJERES$`Puntuacion T Conglomerado de reactivos`), length = 1000)
densidadS7_T1 <- dnorm(s7, mean = mean(HOMBRES$`Puntuacion T Conglomerado de reactivos`), sd = sd(HOMBRES$`Puntuacion T Conglomerado de reactivos`))
densidadS7_T2 <- dnorm(s7, mean = mean(MUJERES$`Puntuacion T Conglomerado de reactivos`), sd = sd(MUJERES$`Puntuacion T Conglomerado de reactivos`))


#Se ajustan los limites de los ejes
xlimS7 <- c(min(s7), max(s7))
ylimS7 <- c(0, max(densidadS7_T1, densidadS7_T2) * 1.2)

plot(s7, densidadS7_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de conglomerado de reactivos por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS7, ylim = ylimS7)
lines(s7, densidadS7_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#Red de apoyo social por sexo
s8 <- seq(min(HOMBRES$`Puntuacion T Red de apoyo social`, 
              MUJERES$`Puntuacion T Red de apoyo social`), 
          max(HOMBRES$`Puntuacion T Red de apoyo social`, 
              MUJERES$`Puntuacion T Red de apoyo social`), length = 1000)
densidadS8_T1 <- dnorm(s8, mean = mean(HOMBRES$`Puntuacion T Red de apoyo social`), sd = sd(HOMBRES$`Puntuacion T Red de apoyo social`))
densidadS8_T2 <- dnorm(s8, mean = mean(MUJERES$`Puntuacion T Red de apoyo social`), sd = sd(MUJERES$`Puntuacion T Red de apoyo social`))


#Se ajustan los limites de los ejes
xlimS8 <- c(min(s8), max(s8))
ylimS8 <- c(0, max(densidadS8_T1, densidadS8_T2) * 1.2)

plot(s8, densidadS8_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de red de apoyo social por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS8, ylim = ylimS8)
lines(s8, densidadS8_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#Conducta tipo A por sexo
s9 <- seq(min(HOMBRES$`Puntuacion T Conducta tipo A`, 
              MUJERES$`Puntuacion T Conducta tipo A`), 
          max(HOMBRES$`Puntuacion T Conducta tipo A`, 
              MUJERES$`Puntuacion T Conducta tipo A`), length = 1000)
densidadS9_T1 <- dnorm(s9, mean = mean(HOMBRES$`Puntuacion T Conducta tipo A`), sd = sd(HOMBRES$`Puntuacion T Conducta tipo A`))
densidadS9_T2 <- dnorm(s9, mean = mean(MUJERES$`Puntuacion T Conducta tipo A`), sd = sd(MUJERES$`Puntuacion T Conducta tipo A`))


#Se ajustan los limites de los ejes
xlimS9 <- c(min(s9), max(s9))
ylimS9 <- c(0, max(densidadS9_T1, densidadS9_T2) * 1.2)

plot(s9, densidadS9_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de conducta tipo A por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS9, ylim = ylimS9)
lines(s9, densidadS9_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")



#Fuerza cognitiva por sexo
s10 <- seq(min(HOMBRES$`Puntuacion T Fuerza cognitiva`, 
              MUJERES$`Puntuacion T Fuerza cognitiva`), 
          max(HOMBRES$`Puntuacion T Fuerza cognitiva`, 
              MUJERES$`Puntuacion T Fuerza cognitiva`), length = 1000)
densidadS10_T1 <- dnorm(s10, mean = mean(HOMBRES$`Puntuacion T Fuerza cognitiva`), sd = sd(HOMBRES$`Puntuacion T Fuerza cognitiva`))
densidadS10_T2 <- dnorm(s10, mean = mean(MUJERES$`Puntuacion T Fuerza cognitiva`), sd = sd(MUJERES$`Puntuacion T Fuerza cognitiva`))


#Se ajustan los limites de los ejes
xlimS10 <- c(min(s10), max(s10))
ylimS10 <- c(0, max(densidadS10_T1, densidadS10_T2) * 1.2)

plot(s10, densidadS10_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de fuerza cognitiva por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS10, ylim = ylimS10)
lines(s10, densidadS10_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#Valoracion positiva por sexo
s11 <- seq(min(HOMBRES$`Puntuacion T Valoracion positiva`, 
               MUJERES$`Puntuacion T Valoracion positiva`), 
           max(HOMBRES$`Puntuacion T Valoracion positiva`, 
               MUJERES$`Puntuacion T Valoracion positiva`), length = 1000)
densidadS11_T1 <- dnorm(s11, mean = mean(HOMBRES$`Puntuacion T Valoracion positiva`), sd = sd(HOMBRES$`Puntuacion T Valoracion positiva`))
densidadS11_T2 <- dnorm(s11, mean = mean(MUJERES$`Puntuacion T Valoracion positiva`), sd = sd(MUJERES$`Puntuacion T Valoracion positiva`))


#Se ajustan los limites de los ejes
xlimS11 <- c(min(s11), max(s11))
ylimS11 <- c(0, max(densidadS11_T1, densidadS11_T2) * 1.2)

plot(s11, densidadS11_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de valoracion pisitiva por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS11, ylim = ylimS11)
lines(s11, densidadS11_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")



#Valoracion negativa por sexo
s12 <- seq(min(HOMBRES$`Puntuacion T Valoracion negativa`, 
               MUJERES$`Puntuacion T Valoracion negativa`), 
           max(HOMBRES$`Puntuacion T Valoracion negativa`, 
               MUJERES$`Puntuacion T Valoracion negativa`), length = 1000)
densidadS12_T1 <- dnorm(s12, mean = mean(HOMBRES$`Puntuacion T Valoracion negativa`), sd = sd(HOMBRES$`Puntuacion T Valoracion negativa`))
densidadS12_T2 <- dnorm(s12, mean = mean(MUJERES$`Puntuacion T Valoracion negativa`), sd = sd(MUJERES$`Puntuacion T Valoracion negativa`))


#Se ajustan los limites de los ejes
xlimS12 <- c(min(s12), max(s12))
ylimS12 <- c(0, max(densidadS12_T1, densidadS12_T2) * 1.2)

plot(s12, densidadS12_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de valoracion negativa por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS12, ylim = ylimS12)
lines(s12, densidadS12_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Minimizacion de la amenaza por sexo
s13 <- seq(min(HOMBRES$`Puntuacion T Minimizacion de la amenaza`, 
               MUJERES$`Puntuacion T Minimizacion de la amenaza`), 
           max(HOMBRES$`Puntuacion T Minimizacion de la amenaza`, 
               MUJERES$`Puntuacion T Minimizacion de la amenaza`), length = 1000)
densidadS13_T1 <- dnorm(s13, mean = mean(HOMBRES$`Puntuacion T Minimizacion de la amenaza`), sd = sd(HOMBRES$`Puntuacion T Minimizacion de la amenaza`))
densidadS13_T2 <- dnorm(s13, mean = mean(MUJERES$`Puntuacion T Minimizacion de la amenaza`), sd = sd(MUJERES$`Puntuacion T Minimizacion de la amenaza`))


#Se ajustan los limites de los ejes
xlimS13 <- c(min(s13), max(s13))
ylimS13 <- c(0, max(densidadS13_T1, densidadS13_T2) * 1.2)

plot(s13, densidadS13_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de minimizacion de la amenaza por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS13, ylim = ylimS13)
lines(s13, densidadS13_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")







#Concentracion en el problema de la amenaza por sexo
s14 <- seq(min(HOMBRES$`Puntuacion T Concentracion en el problema`, 
               MUJERES$`Puntuacion T Concentracion en el problema`), 
           max(HOMBRES$`Puntuacion T Concentracion en el problema`, 
               MUJERES$`Puntuacion T Concentracion en el problema`), length = 1000)
densidadS14_T1 <- dnorm(s14, mean = mean(HOMBRES$`Puntuacion T Concentracion en el problema`), sd = sd(HOMBRES$`Puntuacion T Concentracion en el problema`))
densidadS14_T2 <- dnorm(s14, mean = mean(MUJERES$`Puntuacion T Concentracion en el problema`), sd = sd(MUJERES$`Puntuacion T Concentracion en el problema`))


#Se ajustan los limites de los ejes
xlimS14 <- c(min(s14), max(s14))
ylimS14 <- c(0, max(densidadS14_T1, densidadS14_T2) * 1.2)

plot(s14, densidadS14_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de concentracion en el problema por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS14, ylim = ylimS14)
lines(s14, densidadS14_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#Bienestar psicologico de la amenaza por sexo
s15 <- seq(min(HOMBRES$`Puntuacion T Bienestar psicologico`, 
               MUJERES$`Puntuacion T Bienestar psicologico`), 
           max(HOMBRES$`Puntuacion T Bienestar psicologico`, 
               MUJERES$`Puntuacion T Bienestar psicologico`), length = 1000)
densidadS15_T1 <- dnorm(s15, mean = mean(HOMBRES$`Puntuacion T Bienestar psicologico`), sd = sd(HOMBRES$`Puntuacion T Bienestar psicologico`))
densidadS15_T2 <- dnorm(s15, mean = mean(MUJERES$`Puntuacion T Bienestar psicologico`), sd = sd(MUJERES$`Puntuacion T Bienestar psicologico`))


#Se ajustan los limites de los ejes
xlimS15 <- c(min(s15), max(s15))
ylimS15 <- c(0, max(densidadS15_T1, densidadS15_T2) * 1.2)

plot(s15, densidadS15_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación de concentracion en el problema por sexo",
     xlab = "Valor", ylab = "Densidad", xlim = xlimS15, ylim = ylimS15)
lines(s15, densidadS15_T2, col = "red", lwd = 2)
legend("topright", legend = c("Hombres", "Mujeres"), 
       col = c("blue", "red"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")








#Estres por region
r <- seq(min(CANADA$`Puntuacion T Estres`, 
              COSTA$`Puntuacion T Estres`,
              ISTMO$`Puntuacion T Estres`,
              MIXTECA$`Puntuacion T Estres`,
              PAPALOAPAN$`Puntuacion T Estres`,
              SIERRANORTE$`Puntuacion T Estres`,
              SIERRASUR$`Puntuacion T Estres`,
              VALLESCENTRALES$`Puntuacion T Estres`,
              OTRO$`Puntuacion T Estres`), 
          max(CANADA$`Puntuacion T Estres`, 
              COSTA$`Puntuacion T Estres`,
              ISTMO$`Puntuacion T Estres`,
              MIXTECA$`Puntuacion T Estres`,
              PAPALOAPAN$`Puntuacion T Estres`,
              SIERRANORTE$`Puntuacion T Estres`,
              SIERRASUR$`Puntuacion T Estres`,
              VALLESCENTRALES$`Puntuacion T Estres`,
              OTRO$`Puntuacion T Estres`), length = 1000)
densidadR_T1 <- dnorm(r, mean = mean(CANADA$`Puntuacion T Estres`), sd = sd(CANADA$`Puntuacion T Estres`))
densidadR_T2 <- dnorm(r, mean = mean(COSTA$`Puntuacion T Estres`), sd = sd(COSTA$`Puntuacion T Estres`))
densidadR_T3 <- dnorm(r, mean = mean(ISTMO$`Puntuacion T Estres`), sd = sd(ISTMO$`Puntuacion T Estres`))
densidadR_T4 <- dnorm(r, mean = mean(MIXTECA$`Puntuacion T Estres`), sd = sd(MIXTECA$`Puntuacion T Estres`))
densidadR_T5 <- dnorm(r, mean = mean(PAPALOAPAN$`Puntuacion T Estres`), sd = sd(PAPALOAPAN$`Puntuacion T Estres`))
densidadR_T6 <- dnorm(r, mean = mean(SIERRANORTE$`Puntuacion T Estres`), sd = sd(SIERRANORTE$`Puntuacion T Estres`))
densidadR_T7 <- dnorm(r, mean = mean(SIERRASUR$`Puntuacion T Estres`), sd = sd(SIERRASUR$`Puntuacion T Estres`))
densidadR_T8 <- dnorm(r, mean = mean(VALLESCENTRALES$`Puntuacion T Estres`), sd = sd(VALLESCENTRALES$`Puntuacion T Estres`))
densidadR_T9 <- dnorm(r, mean = mean(OTRO$`Puntuacion T Estres`), sd = sd(OTRO$`Puntuacion T Estres`))


#Se ajustan los limites de los ejes
xlimR <- c(min(r), max(r))
ylimR <- c(0, max(densidadR_T1, densidadR_T2, densidadR_T3, densidadR_T4, densidadR_T5,
                 densidadR_T6, densidadR_T7, densidadR_T8, densidadR_T9) * 1.2)

plot(r, densidadR_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación estres por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR, ylim = ylimR)
lines(r, densidadR_T2, col = "red", lwd = 2)
lines(r, densidadR_T3, col = "green", lwd = 2)
lines(r, densidadR_T4, col = "#AB47BC", lwd = 2)
lines(r, densidadR_T5, col = "#009688", lwd = 2)
lines(r, densidadR_T6, col = "#FFEB3B", lwd = 2)
lines(r, densidadR_T7, col = "#FB8C00", lwd = 2)
lines(r, densidadR_T8, col = "#6D4C41", lwd = 2)
lines(r, densidadR_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")



#Habitos de salud por region
r2 <- seq(min(CANADA$`Puntuacion T Habitos de salud`, 
             COSTA$`Puntuacion T Habitos de salud`,
             ISTMO$`Puntuacion T Habitos de salud`,
             MIXTECA$`Puntuacion T Habitos de salud`,
             PAPALOAPAN$`Puntuacion T Habitos de salud`,
             SIERRANORTE$`Puntuacion T Habitos de salud`,
             SIERRASUR$`Puntuacion T Habitos de salud`,
             VALLESCENTRALES$`Puntuacion T Habitos de salud`,
             OTRO$`Puntuacion T Habitos de salud`), 
         max(CANADA$`Puntuacion T Habitos de salud`, 
             COSTA$`Puntuacion T Habitos de salud`,
             ISTMO$`Puntuacion T Habitos de salud`,
             MIXTECA$`Puntuacion T Habitos de salud`,
             PAPALOAPAN$`Puntuacion T Habitos de salud`,
             SIERRANORTE$`Puntuacion T Habitos de salud`,
             SIERRASUR$`Puntuacion T Habitos de salud`,
             VALLESCENTRALES$`Puntuacion T Habitos de salud`,
             OTRO$`Puntuacion T Habitos de salud`), length = 1000)
densidadR2_T1 <- dnorm(r2, mean = mean(CANADA$`Puntuacion T Habitos de salud`), sd = sd(CANADA$`Puntuacion T Habitos de salud`))
densidadR2_T2 <- dnorm(r2, mean = mean(COSTA$`Puntuacion T Habitos de salud`), sd = sd(COSTA$`Puntuacion T Habitos de salud`))
densidadR2_T3 <- dnorm(r2, mean = mean(ISTMO$`Puntuacion T Habitos de salud`), sd = sd(ISTMO$`Puntuacion T Habitos de salud`))
densidadR2_T4 <- dnorm(r2, mean = mean(MIXTECA$`Puntuacion T Habitos de salud`), sd = sd(MIXTECA$`Puntuacion T Habitos de salud`))
densidadR2_T5 <- dnorm(r2, mean = mean(PAPALOAPAN$`Puntuacion T Habitos de salud`), sd = sd(PAPALOAPAN$`Puntuacion T Habitos de salud`))
densidadR2_T6 <- dnorm(r2, mean = mean(SIERRANORTE$`Puntuacion T Habitos de salud`), sd = sd(SIERRANORTE$`Puntuacion T Habitos de salud`))
densidadR2_T7 <- dnorm(r2, mean = mean(SIERRASUR$`Puntuacion T Habitos de salud`), sd = sd(SIERRASUR$`Puntuacion T Habitos de salud`))
densidadR2_T8 <- dnorm(r2, mean = mean(VALLESCENTRALES$`Puntuacion T Habitos de salud`), sd = sd(VALLESCENTRALES$`Puntuacion T Habitos de salud`))
densidadR2_T9 <- dnorm(r2, mean = mean(OTRO$`Puntuacion T Habitos de salud`), sd = sd(OTRO$`Puntuacion T Habitos de salud`))


#Se ajustan los limites de los ejes
xlimR2 <- c(min(r2), max(r2))
ylimR2 <- c(0, max(densidadR2_T1, densidadR2_T2, densidadR2_T3, densidadR2_T4, densidadR2_T5,
                  densidadR2_T6, densidadR2_T7, densidadR2_T8, densidadR2_T9) * 1.2)

plot(r2, densidadR2_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de salud por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR2, ylim = ylimR2)
lines(r2, densidadR2_T2, col = "red", lwd = 2)
lines(r2, densidadR2_T3, col = "green", lwd = 2)
lines(r2, densidadR2_T4, col = "#AB47BC", lwd = 2)
lines(r2, densidadR2_T5, col = "#009688", lwd = 2)
lines(r2, densidadR2_T6, col = "#FFEB3B", lwd = 2)
lines(r2, densidadR2_T7, col = "#FB8C00", lwd = 2)
lines(r2, densidadR2_T8, col = "#6D4C41", lwd = 2)
lines(r2, densidadR2_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")



#Habitos de ejercicio por region
r3 <- seq(min(CANADA$`Puntuacion T Ejercicio`, 
              COSTA$`Puntuacion T Ejercicio`,
              ISTMO$`Puntuacion T Ejercicio`,
              MIXTECA$`Puntuacion T Ejercicio`,
              PAPALOAPAN$`Puntuacion T Ejercicio`,
              SIERRANORTE$`Puntuacion T Ejercicio`,
              SIERRASUR$`Puntuacion T Ejercicio`,
              VALLESCENTRALES$`Puntuacion T Ejercicio`,
              OTRO$`Puntuacion T Ejercicio`), 
          max(CANADA$`Puntuacion T Ejercicio`, 
              COSTA$`Puntuacion T Ejercicio`,
              ISTMO$`Puntuacion T Ejercicio`,
              MIXTECA$`Puntuacion T Ejercicio`,
              PAPALOAPAN$`Puntuacion T Ejercicio`,
              SIERRANORTE$`Puntuacion T Ejercicio`,
              SIERRASUR$`Puntuacion T Ejercicio`,
              VALLESCENTRALES$`Puntuacion T Ejercicio`,
              OTRO$`Puntuacion T Ejercicio`), length = 1000)
densidadR3_T1 <- dnorm(r3, mean = mean(CANADA$`Puntuacion T Ejercicio`), sd = sd(CANADA$`Puntuacion T Ejercicio`))
densidadR3_T2 <- dnorm(r3, mean = mean(COSTA$`Puntuacion T Ejercicio`), sd = sd(COSTA$`Puntuacion T Ejercicio`))
densidadR3_T3 <- dnorm(r3, mean = mean(ISTMO$`Puntuacion T Ejercicio`), sd = sd(ISTMO$`Puntuacion T Ejercicio`))
densidadR3_T4 <- dnorm(r3, mean = mean(MIXTECA$`Puntuacion T Ejercicio`), sd = sd(MIXTECA$`Puntuacion T Ejercicio`))
densidadR3_T5 <- dnorm(r3, mean = mean(PAPALOAPAN$`Puntuacion T Ejercicio`), sd = sd(PAPALOAPAN$`Puntuacion T Ejercicio`))
densidadR3_T6 <- dnorm(r3, mean = mean(SIERRANORTE$`Puntuacion T Ejercicio`), sd = sd(SIERRANORTE$`Puntuacion T Ejercicio`))
densidadR3_T7 <- dnorm(r3, mean = mean(SIERRASUR$`Puntuacion T Ejercicio`), sd = sd(SIERRASUR$`Puntuacion T Ejercicio`))
densidadR3_T8 <- dnorm(r3, mean = mean(VALLESCENTRALES$`Puntuacion T Ejercicio`), sd = sd(VALLESCENTRALES$`Puntuacion T Ejercicio`))
densidadR3_T9 <- dnorm(r3, mean = mean(OTRO$`Puntuacion T Ejercicio`), sd = sd(OTRO$`Puntuacion T Ejercicio`))


#Se ajustan los limites de los ejes
xlimR3 <- c(min(r3), max(r3))
ylimR3 <- c(0, max(densidadR3_T1, densidadR3_T2, densidadR3_T3, densidadR3_T4, densidadR3_T5,
                   densidadR3_T6, densidadR3_T7, densidadR3_T8, densidadR3_T9) * 1.2)

plot(r3, densidadR3_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de ejercicio por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR3, ylim = ylimR3)
lines(r3, densidadR3_T2, col = "red", lwd = 2)
lines(r3, densidadR3_T3, col = "green", lwd = 2)
lines(r3, densidadR3_T4, col = "#AB47BC", lwd = 2)
lines(r3, densidadR3_T5, col = "#009688", lwd = 2)
lines(r3, densidadR3_T6, col = "#FFEB3B", lwd = 2)
lines(r3, densidadR3_T7, col = "#FB8C00", lwd = 2)
lines(r3, densidadR3_T8, col = "#6D4C41", lwd = 2)
lines(r3, densidadR3_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Habitos de descanso por region
r4 <- seq(min(CANADA$`Puntuacion T Descanso/sueño`, 
              COSTA$`Puntuacion T Descanso/sueño`,
              ISTMO$`Puntuacion T Descanso/sueño`,
              MIXTECA$`Puntuacion T Descanso/sueño`,
              PAPALOAPAN$`Puntuacion T Descanso/sueño`,
              SIERRANORTE$`Puntuacion T Descanso/sueño`,
              SIERRASUR$`Puntuacion T Descanso/sueño`,
              VALLESCENTRALES$`Puntuacion T Descanso/sueño`,
              OTRO$`Puntuacion T Descanso/sueño`), 
          max(CANADA$`Puntuacion T Descanso/sueño`, 
              COSTA$`Puntuacion T Descanso/sueño`,
              ISTMO$`Puntuacion T Descanso/sueño`,
              MIXTECA$`Puntuacion T Descanso/sueño`,
              PAPALOAPAN$`Puntuacion T Descanso/sueño`,
              SIERRANORTE$`Puntuacion T Descanso/sueño`,
              SIERRASUR$`Puntuacion T Descanso/sueño`,
              VALLESCENTRALES$`Puntuacion T Descanso/sueño`,
              OTRO$`Puntuacion T Descanso/sueño`), length = 1000)
densidadR4_T1 <- dnorm(r4, mean = mean(CANADA$`Puntuacion T Descanso/sueño`), sd = sd(CANADA$`Puntuacion T Descanso/sueño`))
densidadR4_T2 <- dnorm(r4, mean = mean(COSTA$`Puntuacion T Descanso/sueño`), sd = sd(COSTA$`Puntuacion T Descanso/sueño`))
densidadR4_T3 <- dnorm(r4, mean = mean(ISTMO$`Puntuacion T Descanso/sueño`), sd = sd(ISTMO$`Puntuacion T Descanso/sueño`))
densidadR4_T4 <- dnorm(r4, mean = mean(MIXTECA$`Puntuacion T Descanso/sueño`), sd = sd(MIXTECA$`Puntuacion T Descanso/sueño`))
densidadR4_T5 <- dnorm(r4, mean = mean(PAPALOAPAN$`Puntuacion T Descanso/sueño`), sd = sd(PAPALOAPAN$`Puntuacion T Descanso/sueño`))
densidadR4_T6 <- dnorm(r4, mean = mean(SIERRANORTE$`Puntuacion T Descanso/sueño`), sd = sd(SIERRANORTE$`Puntuacion T Descanso/sueño`))
densidadR4_T7 <- dnorm(r4, mean = mean(SIERRASUR$`Puntuacion T Descanso/sueño`), sd = sd(SIERRASUR$`Puntuacion T Descanso/sueño`))
densidadR4_T8 <- dnorm(r4, mean = mean(VALLESCENTRALES$`Puntuacion T Descanso/sueño`), sd = sd(VALLESCENTRALES$`Puntuacion T Descanso/sueño`))
densidadR4_T9 <- dnorm(r4, mean = mean(OTRO$`Puntuacion T Descanso/sueño`), sd = sd(OTRO$`Puntuacion T Descanso/sueño`))


#Se ajustan los limites de los ejes
xlimR4 <- c(min(r4), max(r4))
ylimR4 <- c(0, max(densidadR4_T1, densidadR4_T2, densidadR4_T3, densidadR4_T4, densidadR4_T5,
                   densidadR4_T6, densidadR4_T7, densidadR4_T8, densidadR4_T9) * 1.2)

plot(r4, densidadR4_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de descanso por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR4, ylim = ylimR4)
lines(r4, densidadR4_T2, col = "red", lwd = 2)
lines(r4, densidadR4_T3, col = "green", lwd = 2)
lines(r4, densidadR4_T4, col = "#AB47BC", lwd = 2)
lines(r4, densidadR4_T5, col = "#009688", lwd = 2)
lines(r4, densidadR4_T6, col = "#FFEB3B", lwd = 2)
lines(r4, densidadR4_T7, col = "#FB8C00", lwd = 2)
lines(r4, densidadR4_T8, col = "#6D4C41", lwd = 2)
lines(r4, densidadR4_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")



#Habitos de nutricion por region
r5 <- seq(min(CANADA$`Puntuacion T Alimentacion/Nutricion`, 
              COSTA$`Puntuacion T Alimentacion/Nutricion`,
              ISTMO$`Puntuacion T Alimentacion/Nutricion`,
              MIXTECA$`Puntuacion T Alimentacion/Nutricion`,
              PAPALOAPAN$`Puntuacion T Alimentacion/Nutricion`,
              SIERRANORTE$`Puntuacion T Alimentacion/Nutricion`,
              SIERRASUR$`Puntuacion T Alimentacion/Nutricion`,
              VALLESCENTRALES$`Puntuacion T Alimentacion/Nutricion`,
              OTRO$`Puntuacion T Alimentacion/Nutricion`), 
          max(CANADA$`Puntuacion T Alimentacion/Nutricion`, 
              COSTA$`Puntuacion T Alimentacion/Nutricion`,
              ISTMO$`Puntuacion T Alimentacion/Nutricion`,
              MIXTECA$`Puntuacion T Alimentacion/Nutricion`,
              PAPALOAPAN$`Puntuacion T Alimentacion/Nutricion`,
              SIERRANORTE$`Puntuacion T Alimentacion/Nutricion`,
              SIERRASUR$`Puntuacion T Alimentacion/Nutricion`,
              VALLESCENTRALES$`Puntuacion T Alimentacion/Nutricion`,
              OTRO$`Puntuacion T Alimentacion/Nutricion`), length = 1000)
densidadR5_T1 <- dnorm(r5, mean = mean(CANADA$`Puntuacion T Alimentacion/Nutricion`), sd = sd(CANADA$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T2 <- dnorm(r5, mean = mean(COSTA$`Puntuacion T Alimentacion/Nutricion`), sd = sd(COSTA$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T3 <- dnorm(r5, mean = mean(ISTMO$`Puntuacion T Alimentacion/Nutricion`), sd = sd(ISTMO$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T4 <- dnorm(r5, mean = mean(MIXTECA$`Puntuacion T Alimentacion/Nutricion`), sd = sd(MIXTECA$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T5 <- dnorm(r5, mean = mean(PAPALOAPAN$`Puntuacion T Alimentacion/Nutricion`), sd = sd(PAPALOAPAN$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T6 <- dnorm(r5, mean = mean(SIERRANORTE$`Puntuacion T Alimentacion/Nutricion`), sd = sd(SIERRANORTE$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T7 <- dnorm(r5, mean = mean(SIERRASUR$`Puntuacion T Alimentacion/Nutricion`), sd = sd(SIERRASUR$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T8 <- dnorm(r5, mean = mean(VALLESCENTRALES$`Puntuacion T Alimentacion/Nutricion`), sd = sd(VALLESCENTRALES$`Puntuacion T Alimentacion/Nutricion`))
densidadR5_T9 <- dnorm(r5, mean = mean(OTRO$`Puntuacion T Alimentacion/Nutricion`), sd = sd(OTRO$`Puntuacion T Alimentacion/Nutricion`))


#Se ajustan los limites de los ejes
xlimR5 <- c(min(r5), max(r5))
ylimR5 <- c(0, max(densidadR5_T1, densidadR5_T2, densidadR5_T3, densidadR5_T4, densidadR5_T5,
                   densidadR5_T6, densidadR5_T7, densidadR5_T8, densidadR5_T9) * 1.2)

plot(r5, densidadR5_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de nutricion por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR5, ylim = ylimR5)
lines(r5, densidadR5_T2, col = "red", lwd = 2)
lines(r5, densidadR5_T3, col = "green", lwd = 2)
lines(r5, densidadR5_T4, col = "#AB47BC", lwd = 2)
lines(r5, densidadR5_T5, col = "#009688", lwd = 2)
lines(r5, densidadR5_T6, col = "#FFEB3B", lwd = 2)
lines(r5, densidadR5_T7, col = "#FB8C00", lwd = 2)
lines(r5, densidadR5_T8, col = "#6D4C41", lwd = 2)
lines(r5, densidadR5_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")



#Habitos de prevencion por region
r6 <- seq(min(CANADA$`Puntuacion T Prevencion`, 
              COSTA$`Puntuacion T Prevencion`,
              ISTMO$`Puntuacion T Prevencion`,
              MIXTECA$`Puntuacion T Prevencion`,
              PAPALOAPAN$`Puntuacion T Prevencion`,
              SIERRANORTE$`Puntuacion T Prevencion`,
              SIERRASUR$`Puntuacion T Prevencion`,
              VALLESCENTRALES$`Puntuacion T Prevencion`,
              OTRO$`Puntuacion T Prevencion`), 
          max(CANADA$`Puntuacion T Prevencion`, 
              COSTA$`Puntuacion T Prevencion`,
              ISTMO$`Puntuacion T Prevencion`,
              MIXTECA$`Puntuacion T Prevencion`,
              PAPALOAPAN$`Puntuacion T Prevencion`,
              SIERRANORTE$`Puntuacion T Prevencion`,
              SIERRASUR$`Puntuacion T Prevencion`,
              VALLESCENTRALES$`Puntuacion T Prevencion`,
              OTRO$`Puntuacion T Prevencion`), length = 1000)
densidadR6_T1 <- dnorm(r6, mean = mean(CANADA$`Puntuacion T Prevencion`), sd = sd(CANADA$`Puntuacion T Prevencion`))
densidadR6_T2 <- dnorm(r6, mean = mean(COSTA$`Puntuacion T Prevencion`), sd = sd(COSTA$`Puntuacion T Prevencion`))
densidadR6_T3 <- dnorm(r6, mean = mean(ISTMO$`Puntuacion T Prevencion`), sd = sd(ISTMO$`Puntuacion T Prevencion`))
densidadR6_T4 <- dnorm(r6, mean = mean(MIXTECA$`Puntuacion T Prevencion`), sd = sd(MIXTECA$`Puntuacion T Prevencion`))
densidadR6_T5 <- dnorm(r6, mean = mean(PAPALOAPAN$`Puntuacion T Prevencion`), sd = sd(PAPALOAPAN$`Puntuacion T Prevencion`))
densidadR6_T6 <- dnorm(r6, mean = mean(SIERRANORTE$`Puntuacion T Prevencion`), sd = sd(SIERRANORTE$`Puntuacion T Prevencion`))
densidadR6_T7 <- dnorm(r6, mean = mean(SIERRASUR$`Puntuacion T Prevencion`), sd = sd(SIERRASUR$`Puntuacion T Prevencion`))
densidadR6_T8 <- dnorm(r6, mean = mean(VALLESCENTRALES$`Puntuacion T Prevencion`), sd = sd(VALLESCENTRALES$`Puntuacion T Prevencion`))
densidadR6_T9 <- dnorm(r6, mean = mean(OTRO$`Puntuacion T Prevencion`), sd = sd(OTRO$`Puntuacion T Prevencion`))


#Se ajustan los limites de los ejes
xlimR6 <- c(min(r6), max(r6))
ylimR6 <- c(0, max(densidadR6_T1, densidadR6_T2, densidadR6_T3, densidadR6_T4, densidadR6_T5,
                   densidadR6_T6, densidadR6_T7, densidadR6_T8, densidadR6_T9) * 1.2)
ylimR6

plot(r6, densidadR6_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de prevencion por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR6, ylim = ylimR6)
lines(r6, densidadR6_T2, col = "red", lwd = 2)
lines(r6, densidadR6_T3, col = "green", lwd = 2)
lines(r6, densidadR6_T4, col = "#AB47BC", lwd = 2)
lines(r6, densidadR6_T5, col = "#009688", lwd = 2)
lines(r6, densidadR6_T6, col = "#FFEB3B", lwd = 2)
lines(r6, densidadR6_T7, col = "#FB8C00", lwd = 2)
lines(r6, densidadR6_T8, col = "#6D4C41", lwd = 2)
lines(r6, densidadR6_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Conglomerado de reactivos por region
r7 <- seq(min(CANADA$`Puntuacion T Conglomerado de reactivos`, 
              COSTA$`Puntuacion T Conglomerado de reactivos`,
              ISTMO$`Puntuacion T Conglomerado de reactivos`,
              MIXTECA$`Puntuacion T Conglomerado de reactivos`,
              PAPALOAPAN$`Puntuacion T Conglomerado de reactivos`,
              SIERRANORTE$`Puntuacion T Conglomerado de reactivos`,
              SIERRASUR$`Puntuacion T Conglomerado de reactivos`,
              VALLESCENTRALES$`Puntuacion T Conglomerado de reactivos`,
              OTRO$`Puntuacion T Conglomerado de reactivos`), 
          max(CANADA$`Puntuacion T Conglomerado de reactivos`, 
              COSTA$`Puntuacion T Conglomerado de reactivos`,
              ISTMO$`Puntuacion T Conglomerado de reactivos`,
              MIXTECA$`Puntuacion T Conglomerado de reactivos`,
              PAPALOAPAN$`Puntuacion T Conglomerado de reactivos`,
              SIERRANORTE$`Puntuacion T Conglomerado de reactivos`,
              SIERRASUR$`Puntuacion T Conglomerado de reactivos`,
              VALLESCENTRALES$`Puntuacion T Conglomerado de reactivos`,
              OTRO$`Puntuacion T Conglomerado de reactivos`), length = 1000)
densidadR7_T1 <- dnorm(r7, mean = mean(CANADA$`Puntuacion T Conglomerado de reactivos`), sd = sd(CANADA$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T2 <- dnorm(r7, mean = mean(COSTA$`Puntuacion T Conglomerado de reactivos`), sd = sd(COSTA$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T3 <- dnorm(r7, mean = mean(ISTMO$`Puntuacion T Conglomerado de reactivos`), sd = sd(ISTMO$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T4 <- dnorm(r7, mean = mean(MIXTECA$`Puntuacion T Conglomerado de reactivos`), sd = sd(MIXTECA$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T5 <- dnorm(r7, mean = mean(PAPALOAPAN$`Puntuacion T Conglomerado de reactivos`), sd = sd(PAPALOAPAN$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T6 <- dnorm(r7, mean = mean(SIERRANORTE$`Puntuacion T Conglomerado de reactivos`), sd = sd(SIERRANORTE$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T7 <- dnorm(r7, mean = mean(SIERRASUR$`Puntuacion T Conglomerado de reactivos`), sd = sd(SIERRASUR$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T8 <- dnorm(r7, mean = mean(VALLESCENTRALES$`Puntuacion T Conglomerado de reactivos`), sd = sd(VALLESCENTRALES$`Puntuacion T Conglomerado de reactivos`))
densidadR7_T9 <- dnorm(r7, mean = mean(OTRO$`Puntuacion T Conglomerado de reactivos`), sd = sd(OTRO$`Puntuacion T Conglomerado de reactivos`))


#Se ajustan los limites de los ejes
xlimR7 <- c(min(r7), max(r7))
ylimR7 <- c(0, max(densidadR7_T1, densidadR7_T2, densidadR7_T3, densidadR7_T4, densidadR7_T5,
                   densidadR7_T6, densidadR7_T7, densidadR7_T8, densidadR7_T9) * 1.2)

plot(r7, densidadR6_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación habitos de prevencion por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR7, ylim = ylimR6)
lines(r7, densidadR6_T2, col = "red", lwd = 2)
lines(r7, densidadR6_T3, col = "green", lwd = 2)
lines(r7, densidadR6_T4, col = "#AB47BC", lwd = 2)
lines(r7, densidadR6_T5, col = "#009688", lwd = 2)
lines(r7, densidadR6_T6, col = "#FFEB3B", lwd = 2)
lines(r7, densidadR6_T7, col = "#FB8C00", lwd = 2)
lines(r7, densidadR6_T8, col = "#6D4C41", lwd = 2)
lines(r7, densidadR6_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Red de apoyo social por region
r8 <- seq(min(CANADA$`Puntuacion T Red de apoyo social`, 
              COSTA$`Puntuacion T Red de apoyo social`,
              ISTMO$`Puntuacion T Red de apoyo social`,
              MIXTECA$`Puntuacion T Red de apoyo social`,
              PAPALOAPAN$`Puntuacion T Red de apoyo social`,
              SIERRANORTE$`Puntuacion T Red de apoyo social`,
              SIERRASUR$`Puntuacion T Red de apoyo social`,
              VALLESCENTRALES$`Puntuacion T Red de apoyo social`,
              OTRO$`Puntuacion T Red de apoyo social`), 
          max(CANADA$`Puntuacion T Red de apoyo social`, 
              COSTA$`Puntuacion T Red de apoyo social`,
              ISTMO$`Puntuacion T Red de apoyo social`,
              MIXTECA$`Puntuacion T Red de apoyo social`,
              PAPALOAPAN$`Puntuacion T Red de apoyo social`,
              SIERRANORTE$`Puntuacion T Red de apoyo social`,
              SIERRASUR$`Puntuacion T Red de apoyo social`,
              VALLESCENTRALES$`Puntuacion T Red de apoyo social`,
              OTRO$`Puntuacion T Red de apoyo social`), length = 1000)
densidadR8_T1 <- dnorm(r8, mean = mean(CANADA$`Puntuacion T Red de apoyo social`), sd = sd(CANADA$`Puntuacion T Red de apoyo social`))
densidadR8_T2 <- dnorm(r8, mean = mean(COSTA$`Puntuacion T Red de apoyo social`), sd = sd(COSTA$`Puntuacion T Red de apoyo social`))
densidadR8_T3 <- dnorm(r8, mean = mean(ISTMO$`Puntuacion T Red de apoyo social`), sd = sd(ISTMO$`Puntuacion T Red de apoyo social`))
densidadR8_T4 <- dnorm(r8, mean = mean(MIXTECA$`Puntuacion T Red de apoyo social`), sd = sd(MIXTECA$`Puntuacion T Red de apoyo social`))
densidadR8_T5 <- dnorm(r8, mean = mean(PAPALOAPAN$`Puntuacion T Red de apoyo social`), sd = sd(PAPALOAPAN$`Puntuacion T Red de apoyo social`))
densidadR8_T6 <- dnorm(r8, mean = mean(SIERRANORTE$`Puntuacion T Red de apoyo social`), sd = sd(SIERRANORTE$`Puntuacion T Red de apoyo social`))
densidadR8_T7 <- dnorm(r8, mean = mean(SIERRASUR$`Puntuacion T Red de apoyo social`), sd = sd(SIERRASUR$`Puntuacion T Red de apoyo social`))
densidadR8_T8 <- dnorm(r8, mean = mean(VALLESCENTRALES$`Puntuacion T Red de apoyo social`), sd = sd(VALLESCENTRALES$`Puntuacion T Red de apoyo social`))
densidadR8_T9 <- dnorm(r8, mean = mean(OTRO$`Puntuacion T Red de apoyo social`), sd = sd(OTRO$`Puntuacion T Red de apoyo social`))


#Se ajustan los limites de los ejes
xlimR8 <- c(min(r8), max(r8))
ylimR8 <- c(0, max(densidadR8_T1, densidadR8_T2, densidadR8_T3, densidadR8_T4, densidadR8_T5,
                   densidadR8_T6, densidadR8_T7, densidadR8_T8, densidadR8_T9) * 1.2)

plot(r8, densidadR8_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación red de apoyo social por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR8, ylim = ylimR8)
lines(r8, densidadR8_T2, col = "red", lwd = 2)
lines(r8, densidadR8_T3, col = "green", lwd = 2)
lines(r8, densidadR8_T4, col = "#AB47BC", lwd = 2)
lines(r8, densidadR8_T5, col = "#009688", lwd = 2)
lines(r8, densidadR8_T6, col = "#FFEB3B", lwd = 2)
lines(r8, densidadR8_T7, col = "#FB8C00", lwd = 2)
lines(r8, densidadR8_T8, col = "#6D4C41", lwd = 2)
lines(r8, densidadR8_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#conducta tipo A por region
r9 <- seq(min(CANADA$`Puntuacion T Conducta tipo A`, 
              COSTA$`Puntuacion T Conducta tipo A`,
              ISTMO$`Puntuacion T Conducta tipo A`,
              MIXTECA$`Puntuacion T Conducta tipo A`,
              PAPALOAPAN$`Puntuacion T Conducta tipo A`,
              SIERRANORTE$`Puntuacion T Conducta tipo A`,
              SIERRASUR$`Puntuacion T Conducta tipo A`,
              VALLESCENTRALES$`Puntuacion T Conducta tipo A`,
              OTRO$`Puntuacion T Conducta tipo A`), 
          max(CANADA$`Puntuacion T Conducta tipo A`, 
              COSTA$`Puntuacion T Conducta tipo A`,
              ISTMO$`Puntuacion T Conducta tipo A`,
              MIXTECA$`Puntuacion T Conducta tipo A`,
              PAPALOAPAN$`Puntuacion T Conducta tipo A`,
              SIERRANORTE$`Puntuacion T Conducta tipo A`,
              SIERRASUR$`Puntuacion T Conducta tipo A`,
              VALLESCENTRALES$`Puntuacion T Conducta tipo A`,
              OTRO$`Puntuacion T Conducta tipo A`), length = 1000)
densidadR9_T1 <- dnorm(r9, mean = mean(CANADA$`Puntuacion T Conducta tipo A`), sd = sd(CANADA$`Puntuacion T Conducta tipo A`))
densidadR9_T2 <- dnorm(r9, mean = mean(COSTA$`Puntuacion T Conducta tipo A`), sd = sd(COSTA$`Puntuacion T Conducta tipo A`))
densidadR9_T3 <- dnorm(r9, mean = mean(ISTMO$`Puntuacion T Conducta tipo A`), sd = sd(ISTMO$`Puntuacion T Conducta tipo A`))
densidadR9_T4 <- dnorm(r9, mean = mean(MIXTECA$`Puntuacion T Conducta tipo A`), sd = sd(MIXTECA$`Puntuacion T Conducta tipo A`))
densidadR9_T5 <- dnorm(r9, mean = mean(PAPALOAPAN$`Puntuacion T Conducta tipo A`), sd = sd(PAPALOAPAN$`Puntuacion T Conducta tipo A`))
densidadR9_T6 <- dnorm(r9, mean = mean(SIERRANORTE$`Puntuacion T Conducta tipo A`), sd = sd(SIERRANORTE$`Puntuacion T Conducta tipo A`))
densidadR9_T7 <- dnorm(r9, mean = mean(SIERRASUR$`Puntuacion T Conducta tipo A`), sd = sd(SIERRASUR$`Puntuacion T Conducta tipo A`))
densidadR9_T8 <- dnorm(r9, mean = mean(VALLESCENTRALES$`Puntuacion T Conducta tipo A`), sd = sd(VALLESCENTRALES$`Puntuacion T Conducta tipo A`))
densidadR9_T9 <- dnorm(r9, mean = mean(OTRO$`Puntuacion T Conducta tipo A`), sd = sd(OTRO$`Puntuacion T Conducta tipo A`))


#Se ajustan los limites de los ejes
xlimR9 <- c(min(r9), max(r9))
ylimR9 <- c(0, max(densidadR9_T1, densidadR9_T2, densidadR9_T3, densidadR9_T4, densidadR9_T5,
                   densidadR9_T6, densidadR9_T7, densidadR9_T8, densidadR9_T9) * 1.2)

plot(r9, densidadR9_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación conducta tipo A por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR9, ylim = ylimR9)
lines(r9, densidadR9_T2, col = "red", lwd = 2)
lines(r9, densidadR9_T3, col = "green", lwd = 2)
lines(r9, densidadR9_T4, col = "#AB47BC", lwd = 2)
lines(r9, densidadR9_T5, col = "#009688", lwd = 2)
lines(r9, densidadR9_T6, col = "#FFEB3B", lwd = 2)
lines(r9, densidadR9_T7, col = "#FB8C00", lwd = 2)
lines(r9, densidadR9_T8, col = "#6D4C41", lwd = 2)
lines(r9, densidadR9_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#fuerza cognitiva por region
r10 <- seq(min(CANADA$`Puntuacion T Valoracion positiva`, 
               COSTA$`Puntuacion T Valoracion positiva`,
               ISTMO$`Puntuacion T Valoracion positiva`,
               MIXTECA$`Puntuacion T Valoracion positiva`,
               PAPALOAPAN$`Puntuacion T Valoracion positiva`,
               SIERRANORTE$`Puntuacion T Valoracion positiva`,
               SIERRASUR$`Puntuacion T Valoracion positiva`,
               VALLESCENTRALES$`Puntuacion T Valoracion positiva`,
               OTRO$`Puntuacion T Valoracion positiva`), 
           max(CANADA$`Puntuacion T Valoracion positiva`, 
               COSTA$`Puntuacion T Valoracion positiva`,
               ISTMO$`Puntuacion T Valoracion positiva`,
               MIXTECA$`Puntuacion T Valoracion positiva`,
               PAPALOAPAN$`Puntuacion T Valoracion positiva`,
               SIERRANORTE$`Puntuacion T Valoracion positiva`,
               SIERRASUR$`Puntuacion T Valoracion positiva`,
               VALLESCENTRALES$`Puntuacion T Valoracion positiva`,
               OTRO$`Puntuacion T Valoracion positiva`), length = 1000)
densidadR10_T1 <- dnorm(r10, mean = mean(CANADA$`Puntuacion T Valoracion positiva`), sd = sd(CANADA$`Puntuacion T Valoracion positiva`))
densidadR10_T2 <- dnorm(r10, mean = mean(COSTA$`Puntuacion T Valoracion positiva`), sd = sd(COSTA$`Puntuacion T Valoracion positiva`))
densidadR10_T3 <- dnorm(r10, mean = mean(ISTMO$`Puntuacion T Valoracion positiva`), sd = sd(ISTMO$`Puntuacion T Valoracion positiva`))
densidadR10_T4 <- dnorm(r10, mean = mean(MIXTECA$`Puntuacion T Valoracion positiva`), sd = sd(MIXTECA$`Puntuacion T Valoracion positiva`))
densidadR10_T5 <- dnorm(r10, mean = mean(PAPALOAPAN$`Puntuacion T Valoracion positiva`), sd = sd(PAPALOAPAN$`Puntuacion T Valoracion positiva`))
densidadR10_T6 <- dnorm(r10, mean = mean(SIERRANORTE$`Puntuacion T Valoracion positiva`), sd = sd(SIERRANORTE$`Puntuacion T Valoracion positiva`))
densidadR10_T7 <- dnorm(r10, mean = mean(SIERRASUR$`Puntuacion T Valoracion positiva`), sd = sd(SIERRASUR$`Puntuacion T Valoracion positiva`))
densidadR10_T8 <- dnorm(r10, mean = mean(VALLESCENTRALES$`Puntuacion T Valoracion positiva`), sd = sd(VALLESCENTRALES$`Puntuacion T Valoracion positiva`))
densidadR10_T9 <- dnorm(r10, mean = mean(OTRO$`Puntuacion T Valoracion positiva`), sd = sd(OTRO$`Puntuacion T Valoracion positiva`))


#Se ajustan los limites de los ejes
xlimR10 <- c(min(r10), max(r10))
ylimR10 <- c(0, max(densidadR10_T1, densidadR10_T2, densidadR10_T3, densidadR10_T4, densidadR10_T5,
                    densidadR10_T6, densidadR10_T7, densidadR10_T8, densidadR10_T9) * 1.2)

plot(r10, densidadR10_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación valoracion positiva por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR10, ylim = ylimR10)
lines(r10, densidadR10_T2, col = "red", lwd = 2)
lines(r10, densidadR10_T3, col = "green", lwd = 2)
lines(r10, densidadR10_T4, col = "#AB47BC", lwd = 2)
lines(r10, densidadR10_T5, col = "#009688", lwd = 2)
lines(r10, densidadR10_T6, col = "#FFEB3B", lwd = 2)
lines(r10, densidadR10_T7, col = "#FB8C00", lwd = 2)
lines(r10, densidadR10_T8, col = "#6D4C41", lwd = 2)
lines(r10, densidadR10_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#valoracion negativa por region
r11 <- seq(min(CANADA$`Puntuacion T Valoracion negativa`,
               COSTA$`Puntuacion T Valoracion negativa`,
               ISTMO$`Puntuacion T Valoracion negativa`,
               MIXTECA$`Puntuacion T Valoracion negativa`,
               PAPALOAPAN$`Puntuacion T Valoracion negativa`,
               SIERRANORTE$`Puntuacion T Valoracion negativa`,
               SIERRASUR$`Puntuacion T Valoracion negativa`,
               VALLESCENTRALES$`Puntuacion T Valoracion negativa`,
               OTRO$`Puntuacion T Valoracion negativa`), 
           max(CANADA$`Puntuacion T Valoracion negativa`, 
               COSTA$`Puntuacion T Valoracion negativa`,
               ISTMO$`Puntuacion T Valoracion negativa`,
               MIXTECA$`Puntuacion T Valoracion negativa`,
               PAPALOAPAN$`Puntuacion T Valoracion negativa`,
               SIERRANORTE$`Puntuacion T Valoracion negativa`,
               SIERRASUR$`Puntuacion T Valoracion negativa`,
               VALLESCENTRALES$`Puntuacion T Valoracion negativa`,
               OTRO$`Puntuacion T Valoracion negativa`), length = 1000)
densidadR11_T1 <- dnorm(r11, mean = mean(CANADA$`Puntuacion T Valoracion negativa`), sd = sd(CANADA$`Puntuacion T Valoracion negativa`))
densidadR11_T2 <- dnorm(r11, mean = mean(COSTA$`Puntuacion T Valoracion negativa`), sd = sd(COSTA$`Puntuacion T Valoracion negativa`))
densidadR11_T3 <- dnorm(r11, mean = mean(ISTMO$`Puntuacion T Valoracion negativa`), sd = sd(ISTMO$`Puntuacion T Valoracion negativa`))
densidadR11_T4 <- dnorm(r11, mean = mean(MIXTECA$`Puntuacion T Valoracion negativa`), sd = sd(MIXTECA$`Puntuacion T Valoracion negativa`))
densidadR11_T5 <- dnorm(r11, mean = mean(PAPALOAPAN$`Puntuacion T Valoracion negativa`), sd = sd(PAPALOAPAN$`Puntuacion T Valoracion negativa`))
densidadR11_T6 <- dnorm(r11, mean = mean(SIERRANORTE$`Puntuacion T Valoracion negativa`), sd = sd(SIERRANORTE$`Puntuacion T Valoracion negativa`))
densidadR11_T7 <- dnorm(r11, mean = mean(SIERRASUR$`Puntuacion T Valoracion negativa`), sd = sd(SIERRASUR$`Puntuacion T Valoracion negativa`))
densidadR11_T8 <- dnorm(r11, mean = mean(VALLESCENTRALES$`Puntuacion T Valoracion negativa`), sd = sd(VALLESCENTRALES$`Puntuacion T Valoracion negativa`))
densidadR11_T9 <- dnorm(r11, mean = mean(OTRO$`Puntuacion T Valoracion negativa`), sd = sd(OTRO$`Puntuacion T Valoracion negativa`))


#Se ajustan los limites de los ejes
xlimR11 <- c(min(r11), max(r11))
ylimR11 <- c(0, max(densidadR11_T1, densidadR11_T2, densidadR11_T3, densidadR11_T4, densidadR11_T5,
                    densidadR11_T6, densidadR11_T7, densidadR11_T8, densidadR11_T9) * 1.2)

plot(r11, densidadR11_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación valoracion negativa por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR11, ylim = ylimR11)
lines(r11, densidadR11_T2, col = "red", lwd = 2)
lines(r11, densidadR11_T3, col = "green", lwd = 2)
lines(r11, densidadR11_T4, col = "#AB47BC", lwd = 2)
lines(r11, densidadR11_T5, col = "#009688", lwd = 2)
lines(r11, densidadR11_T6, col = "#FFEB3B", lwd = 2)
lines(r11, densidadR11_T7, col = "#FB8C00", lwd = 2)
lines(r11, densidadR11_T8, col = "#6D4C41", lwd = 2)
lines(r11, densidadR11_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")





#Minimizacion de la amenaza por region
r12 <- seq(min(CANADA$`Puntuacion T Minimizacion de la amenaza`, 
               COSTA$`Puntuacion T Minimizacion de la amenaza`,
               ISTMO$`Puntuacion T Minimizacion de la amenaza`,
               MIXTECA$`Puntuacion T Minimizacion de la amenaza`,
               PAPALOAPAN$`Puntuacion T Minimizacion de la amenaza`,
               SIERRANORTE$`Puntuacion T Minimizacion de la amenaza`,
               SIERRASUR$`Puntuacion T Minimizacion de la amenaza`,
               VALLESCENTRALES$`Puntuacion T Minimizacion de la amenaza`,
               OTRO$`Puntuacion T Minimizacion de la amenaza`), 
           max(CANADA$`Puntuacion T Minimizacion de la amenaza`, 
               COSTA$`Puntuacion T Minimizacion de la amenaza`,
               ISTMO$`Puntuacion T Minimizacion de la amenaza`,
               MIXTECA$`Puntuacion T Minimizacion de la amenaza`,
               PAPALOAPAN$`Puntuacion T Minimizacion de la amenaza`,
               SIERRANORTE$`Puntuacion T Minimizacion de la amenaza`,
               SIERRASUR$`Puntuacion T Minimizacion de la amenaza`,
               VALLESCENTRALES$`Puntuacion T Minimizacion de la amenaza`,
               OTRO$`Puntuacion T Minimizacion de la amenaza`), length = 1000)
densidadR12_T1 <- dnorm(r12, mean = mean(CANADA$`Puntuacion T Minimizacion de la amenaza`), sd = sd(CANADA$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T2 <- dnorm(r12, mean = mean(COSTA$`Puntuacion T Minimizacion de la amenaza`), sd = sd(COSTA$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T3 <- dnorm(r12, mean = mean(ISTMO$`Puntuacion T Minimizacion de la amenaza`), sd = sd(ISTMO$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T4 <- dnorm(r12, mean = mean(MIXTECA$`Puntuacion T Minimizacion de la amenaza`), sd = sd(MIXTECA$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T5 <- dnorm(r12, mean = mean(PAPALOAPAN$`Puntuacion T Minimizacion de la amenaza`), sd = sd(PAPALOAPAN$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T6 <- dnorm(r12, mean = mean(SIERRANORTE$`Puntuacion T Minimizacion de la amenaza`), sd = sd(SIERRANORTE$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T7 <- dnorm(r12, mean = mean(SIERRASUR$`Puntuacion T Minimizacion de la amenaza`), sd = sd(SIERRASUR$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T8 <- dnorm(r12, mean = mean(VALLESCENTRALES$`Puntuacion T Minimizacion de la amenaza`), sd = sd(VALLESCENTRALES$`Puntuacion T Minimizacion de la amenaza`))
densidadR12_T9 <- dnorm(r12, mean = mean(OTRO$`Puntuacion T Minimizacion de la amenaza`), sd = sd(OTRO$`Puntuacion T Minimizacion de la amenaza`))


#Se ajustan los limites de los ejes
xlimR12 <- c(min(r12), max(r12))
ylimR12 <- c(0, max(densidadR12_T1, densidadR12_T2, densidadR12_T3, densidadR12_T4, densidadR12_T5,
                    densidadR12_T6, densidadR12_T7, densidadR12_T8, densidadR12_T9) * 1.2)

plot(r12, densidadR12_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación minimizacion de la amenaza por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimr12, ylim = ylimR12)
lines(r12, densidadR12_T2, col = "red", lwd = 2)
lines(r12, densidadR12_T3, col = "green", lwd = 2)
lines(r12, densidadR12_T4, col = "#AB47BC", lwd = 2)
lines(r12, densidadR12_T5, col = "#009688", lwd = 2)
lines(r12, densidadR12_T6, col = "#FFEB3B", lwd = 2)
lines(r12, densidadR12_T7, col = "#FB8C00", lwd = 2)
lines(r12, densidadR12_T8, col = "#6D4C41", lwd = 2)
lines(r12, densidadR12_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")






#Concentracion en el problema por region
r13 <- seq(min(CANADA$`Puntuacion T Concentracion en el problema`, 
               COSTA$`Puntuacion T Concentracion en el problema`,
               ISTMO$`Puntuacion T Concentracion en el problema`,
               MIXTECA$`Puntuacion T Concentracion en el problema`,
               PAPALOAPAN$`Puntuacion T Concentracion en el problema`,
               SIERRANORTE$`Puntuacion T Concentracion en el problema`,
               SIERRASUR$`Puntuacion T Concentracion en el problema`,
               VALLESCENTRALES$`Puntuacion T Concentracion en el problema`,
               OTRO$`Puntuacion T Concentracion en el problema`), 
           max(CANADA$`Puntuacion T Concentracion en el problema`, 
               COSTA$`Puntuacion T Concentracion en el problema`,
               ISTMO$`Puntuacion T Concentracion en el problema`,
               MIXTECA$`Puntuacion T Concentracion en el problema`,
               PAPALOAPAN$`Puntuacion T Concentracion en el problema`,
               SIERRANORTE$`Puntuacion T Concentracion en el problema`,
               SIERRASUR$`Puntuacion T Concentracion en el problema`,
               VALLESCENTRALES$`Puntuacion T Concentracion en el problema`,
               OTRO$`Puntuacion T Concentracion en el problema`), length = 1000)
densidadR13_T1 <- dnorm(r13, mean = mean(CANADA$`Puntuacion T Concentracion en el problema`), sd = sd(CANADA$`Puntuacion T Concentracion en el problema`))
densidadR13_T2 <- dnorm(r13, mean = mean(COSTA$`Puntuacion T Concentracion en el problema`), sd = sd(COSTA$`Puntuacion T Concentracion en el problema`))
densidadR13_T3 <- dnorm(r13, mean = mean(ISTMO$`Puntuacion T Concentracion en el problema`), sd = sd(ISTMO$`Puntuacion T Concentracion en el problema`))
densidadR13_T4 <- dnorm(r13, mean = mean(MIXTECA$`Puntuacion T Concentracion en el problema`), sd = sd(MIXTECA$`Puntuacion T Concentracion en el problema`))
densidadR13_T5 <- dnorm(r13, mean = mean(PAPALOAPAN$`Puntuacion T Concentracion en el problema`), sd = sd(PAPALOAPAN$`Puntuacion T Concentracion en el problema`))
densidadR13_T6 <- dnorm(r13, mean = mean(SIERRANORTE$`Puntuacion T Concentracion en el problema`), sd = sd(SIERRANORTE$`Puntuacion T Concentracion en el problema`))
densidadR13_T7 <- dnorm(r13, mean = mean(SIERRASUR$`Puntuacion T Concentracion en el problema`), sd = sd(SIERRASUR$`Puntuacion T Concentracion en el problema`))
densidadR13_T8 <- dnorm(r13, mean = mean(VALLESCENTRALES$`Puntuacion T Concentracion en el problema`), sd = sd(VALLESCENTRALES$`Puntuacion T Concentracion en el problema`))
densidadR13_T9 <- dnorm(r13, mean = mean(OTRO$`Puntuacion T Concentracion en el problema`), sd = sd(OTRO$`Puntuacion T Concentracion en el problema`))


#Se ajustan los limites de los ejes
xlimr13 <- c(min(r13), max(r13))
ylimr13 <- c(0, max(densidadR13_T1, densidadR13_T2, densidadR13_T3, densidadR13_T4, densidadR13_T5,
                    densidadR13_T6, densidadR13_T7, densidadR13_T8, densidadR13_T9) * 1.2)

plot(r13, densidadR13_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación concentracion en el problema por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimr13, ylim = ylimr13)
lines(r13, densidadR13_T2, col = "red", lwd = 2)
lines(r13, densidadR13_T3, col = "green", lwd = 2)
lines(r13, densidadR13_T4, col = "#AB47BC", lwd = 2)
lines(r13, densidadR13_T5, col = "#009688", lwd = 2)
lines(r13, densidadR13_T6, col = "#FFEB3B", lwd = 2)
lines(r13, densidadR13_T7, col = "#FB8C00", lwd = 2)
lines(r13, densidadR13_T8, col = "#6D4C41", lwd = 2)
lines(r13, densidadR13_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.5, box.col = "black")




#Bienestar psicologico por region
r14 <- seq(min(CANADA$`Puntuacion T Bienestar psicologico`, 
               COSTA$`Puntuacion T Bienestar psicologico`,
               ISTMO$`Puntuacion T Bienestar psicologico`,
               MIXTECA$`Puntuacion T Bienestar psicologico`,
               PAPALOAPAN$`Puntuacion T Bienestar psicologico`,
               SIERRANORTE$`Puntuacion T Bienestar psicologico`,
               SIERRASUR$`Puntuacion T Bienestar psicologico`,
               VALLESCENTRALES$`Puntuacion T Bienestar psicologico`,
               OTRO$`Puntuacion T Bienestar psicologico`), 
           max(CANADA$`Puntuacion T Bienestar psicologico`, 
               COSTA$`Puntuacion T Bienestar psicologico`,
               ISTMO$`Puntuacion T Bienestar psicologico`,
               MIXTECA$`Puntuacion T Bienestar psicologico`,
               PAPALOAPAN$`Puntuacion T Bienestar psicologico`,
               SIERRANORTE$`Puntuacion T Bienestar psicologico`,
               SIERRASUR$`Puntuacion T Bienestar psicologico`,
               VALLESCENTRALES$`Puntuacion T Bienestar psicologico`,
               OTRO$`Puntuacion T Bienestar psicologico`), length = 1000)
densidadR14_T1 <- dnorm(r14, mean = mean(CANADA$`Puntuacion T Bienestar psicologico`), sd = sd(CANADA$`Puntuacion T Bienestar psicologico`))
densidadR14_T2 <- dnorm(r14, mean = mean(COSTA$`Puntuacion T Bienestar psicologico`), sd = sd(COSTA$`Puntuacion T Bienestar psicologico`))
densidadR14_T3 <- dnorm(r14, mean = mean(ISTMO$`Puntuacion T Bienestar psicologico`), sd = sd(ISTMO$`Puntuacion T Bienestar psicologico`))
densidadR14_T4 <- dnorm(r14, mean = mean(MIXTECA$`Puntuacion T Bienestar psicologico`), sd = sd(MIXTECA$`Puntuacion T Bienestar psicologico`))
densidadR14_T5 <- dnorm(r14, mean = mean(PAPALOAPAN$`Puntuacion T Bienestar psicologico`), sd = sd(PAPALOAPAN$`Puntuacion T Bienestar psicologico`))
densidadR14_T6 <- dnorm(r14, mean = mean(SIERRANORTE$`Puntuacion T Bienestar psicologico`), sd = sd(SIERRANORTE$`Puntuacion T Bienestar psicologico`))
densidadR14_T7 <- dnorm(r14, mean = mean(SIERRASUR$`Puntuacion T Bienestar psicologico`), sd = sd(SIERRASUR$`Puntuacion T Bienestar psicologico`))
densidadR14_T8 <- dnorm(r14, mean = mean(VALLESCENTRALES$`Puntuacion T Bienestar psicologico`), sd = sd(VALLESCENTRALES$`Puntuacion T Bienestar psicologico`))
densidadR14_T9 <- dnorm(r14, mean = mean(OTRO$`Puntuacion T Bienestar psicologico`), sd = sd(OTRO$`Puntuacion T Bienestar psicologico`))


#Se ajustan los limites de los ejes
xlimR14 <- c(min(r14), max(r14))
ylimR14 <- c(0, max(densidadR14_T1, densidadR14_T2, densidadR14_T3, densidadR14_T4, densidadR14_T5,
                    densidadR14_T6, densidadR14_T7, densidadR14_T8, densidadR14_T9) * 1.2)

plot(r14, densidadR14_T1, type = "l", col = "blue", lwd = 2,
     main = "Comparación bienestar psicologico por region",
     xlab = "Valor", ylab = "Densidad", xlim = xlimR14, ylim = ylimR14)
lines(r14, densidadR14_T2, col = "red", lwd = 2)
lines(r14, densidadR14_T3, col = "green", lwd = 2)
lines(r14, densidadR14_T4, col = "#AB47BC", lwd = 2)
lines(r14, densidadR14_T5, col = "#009688", lwd = 2)
lines(r14, densidadR14_T6, col = "#FFEB3B", lwd = 2)
lines(r14, densidadR14_T7, col = "#FB8C00", lwd = 2)
lines(r14, densidadR14_T8, col = "#6D4C41", lwd = 2)
lines(r14, densidadR14_T9, col = "#9E9E9E", lwd = 2)
legend("topright", legend = c("CANADA", "COSTA", "ISTMO","MIXTECA","PAPALOAPAN","SIERRANORTE","SIERRASUR","VALLESCENTRALES","OTRO"), 
       col = c("blue", "red", "green","#AB47BC","#009688","#FFEB3B","#FB8C00","#6D4C41","#9E9E9E"), lwd = 2, bg = "white", cex = 0.4, box.col = "black")

