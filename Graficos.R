# INSTALACIÓN DE R
# https://cran.rstudio.com/
# Download R for Windows -> base -> Download R 4.1.2 for Windows 

# INSTALACIÓN DE RStudio (IDE)
# https://www.rstudio.com/products/rstudio/download/#download
# Free -> Download RStudio for Windows


################ IMPORTACION Y LIMPIEZA DE DATOS ###############################

# Setear el directorio de trabajo
setwd("D:/OneDrive - fbioyf.unr.edu.ar/Seminarios/Workshop_PythonAndR_211115")


# Importo los datos de la planilla de Excel
if (!require("readxl")) {
  install.packages("readxl", dependencies = TRUE)
  library("readxl")
}

datos <-read_excel(path = "Cuantificacion.xlsx", sheet = "Sheet1", 
                   range="A1:Q187")

head(datos)

# Puede ser necesario convertir a data.frame
# datos <- as.data.frame(datos)

datos$Muestra # Accedo a una columna particular
datos$Cepa
datos$`ug/ml` # Para acceder a la columna %TIC tengo que escaparla


# Cambio los nombres a las columnas para que sean mas facilmente manipulables
colnames(datos)<-c("Muestra","Cepa","Vol", "DO", "Comp",
                   "Tr", "TIC_Area", "ug", "ug.ml",
                   "Ion74Area","Ion87Area", "Ion88Area",
                   "Factor_cc","AreaTIC/ml", "Area74/ml", "TotalTIC", "PorcTIC")
head(datos)


# Declaracion de variables para este dataset particular  #
columnasInteres <- c("Muestra","Cepa", "DO", "Comp", "ug", "PorcTIC")
datos <- datos[columnasInteres]
head(datos)

cepas <- c("Strain_A", "Strain_B", "Strain_C")
compuestos <- unique(datos$Comp)
compuestos
EST.INTERNO <- "11mC13"
compuestos.SinEI <- compuestos[compuestos != EST.INTERNO]


#Creo una columna donde combino muestra y cepa
datos$Replicas <- paste(datos$Cepa, datos$Muestra, sep= "-")
datosReplicas <- unique(datos$Replicas)
head(datos)


######## TRANSFORMACION DE LOS DATOS, CALCULO DE PROMEDIOS Y DESVIOS ###########

# Puedo fácilmente operar sobre 2 columnas
# Ejemplo: Agrego una variable que sea ug/DO
datos$'ug/DO' <- datos$ug / datos$DO
head(datos)


# Calculo del promedio y desvio de las variables ug, %TIC y ug/DO para cada compuesto

#Ejemplo, calculo con un ciclo for del promedio y desvío de la variable ug

for (cepa in cepas){
  for (compuesto in compuestos.SinEI){
    print(paste("Cepa: ", cepa, "; Compuesto: ", compuesto, 
                "; Mean: ", round(mean(datos[datos$Cepa==cepa & datos$Comp== compuesto, 'ug']$ug),3),
                "; Sd: ", round(sd(datos[datos$Cepa==cepa & datos$Comp== compuesto, 'ug']$ug),3),
                sep = ""))
  }
}

# Mucho más sencillo usando el paquete dplyr
library(dplyr)
# dplyr es una libreria de tidyverse: R packages for data science
# de hecho, readxl, tambien es un paquete de tidyverse
# https://www.tidyverse.org/
# install.packages("tidyverse")
# library("tidyverse")


datosPromedio <-
  datos %>%
  group_by(Cepa, Comp) %>%
  summarise(media.ug = mean(ug), desv.ug = sd(ug),
            media.porcTIC = mean(PorcTIC), desv.porcTIC = sd(PorcTIC),
            media.ugDO = mean(`ug/DO`), desv.ugDO = sd(`ug/DO`),
            n = n())

head(as.data.frame(datosPromedio))



#### GRÁFICOS DE BARRAS ######

library(ggplot2)

# Graficamos la variable %TIC para todos los compuestos y las 2 replicas para cada cepa #
colnames(datos)
p1 <- 
  ggplot(data = datos)+
  aes_string(x = "Comp", y = "PorcTIC", fill = "Replicas")+
  geom_bar(stat="identity", position = position_dodge(), width=0.75)

p1


# Tuneamos el gráfico #####
p2 <- p1 + 
  # Barras con contorno negro
  geom_bar(stat="identity", position=position_dodge(), width=0.75, 
           color="black", linetype = "solid") +
  labs(x ="", y= "% TIC", fill= "Cepa", 
       title = "Todos los compuestos y réplicas")
  
p2  

library(cowplot) # Librería para poner el fondo blanco

p3 <- p2 + 
  scale_x_discrete(limits = compuestos.SinEI)+ #Para que no cambie el orden de los compuestos
  theme_cowplot(line_size = 1) + # Fondo blanco
  theme(legend.position="top", # Posicion de la leyenda
        legend.title=element_text(size=16), 
        legend.text = element_text(size=18),
        axis.title = element_text(size=20, face = "bold"), #tamaño del nombre de los ejes
        axis.text.x = element_text(angle = 45, hjust = 1,size=18), #tamaño de los compuestos
        axis.text.y = element_text(size=18), #tamaño de los numeros
        plot.margin = margin(0.5, 1, 0, 1, "cm")) #superior, derecho, abajo, izquierdo
p3

# Varias opciones para cambiar los colores de las barras 
library(RColorBrewer)
display.brewer.all()

p3 + scale_fill_brewer(palette="Greys")
p3 + scale_fill_brewer(palette="Set2")



## Cambio el nombre de los compuestos x los nombres bien escritos ####
#\u0394 es el unicode para el delta
compuestosBienEscritos <- c("2-mC12", "C12", "4-mC12", "8-mC12",
                            "2-mC13", "C13", "4-mC13", "11mC13", "2-mC14", "C14",
                            "C14:1\u039411", "8-mC14", "4-mC14", "10-mC14",
                            "C15:1\u03947", "12-mC14", "2-mC15", "C15",
                            "C15:1\u03949", "10-mC15", "2-mC16",
                            "C16", "C16:1\u03949", "12-mC16", "C17", "C17:1\u039410",
                            "C17:1\u03949","C18", "C18:1\u039411", "C19",
                            "C19:1\u039410")

for (i in 1:length(compuestosBienEscritos)){
  datos$Comp[datos$Comp==compuestos[i]] <- compuestosBienEscritos[i]
  datosPromedio$Comp[datosPromedio$Comp==compuestos[i]] <- compuestosBienEscritos[i]
}

compuestos.SinEI <- compuestosBienEscritos[compuestosBienEscritos != EST.INTERNO]

p4 <- 
  ggplot(data = datos)+
  aes_string(x = "Comp", y = "PorcTIC", fill = "Replicas")+
  geom_bar(stat="identity", position=position_dodge(), width=0.75,
           color="black", linetype = "solid") +
  scale_x_discrete(limits = compuestos.SinEI)+ #Para que no cambie el orden de los compuestos
  theme_cowplot(line_size = 1) + # Fondo blanco
  theme(legend.position="top", 
        legend.title=element_text(size=16),
        legend.text = element_text(size=18),
        axis.title = element_text(size=20, face = "bold"), #tamaño del nombre de los ejes
        axis.text.x = element_text(angle = 45, hjust = 1,size=18), #tamaño de los compuestos
        axis.text.y = element_text(size=18), #tamaño de los numeros
        plot.margin = margin(0.5, 1, 0, 1, "cm")) + #superior, derecho, abajo, izquierdo
  scale_fill_brewer(palette="Set2")+
  labs(x ="", y= "% TIC", fill= "Cepa", 
       title = "Todos los compuestos y réplicas")
p4

### Y SI QUIERO GRAFICAR OTRA VARIABLE?? ####
colnames(datos)
# Puedo copiar todo el código anterior e indicar que grafico otro eje Y
# Por ejemplo para ug/DO
p5 <- 
  ggplot(data = datos)+
  aes_string(x = "Comp", y = "ug/DO", fill = "Replicas")+
  geom_bar(stat="identity", position=position_dodge(), width=0.75,
           color="black", linetype = "solid") +
  scale_x_discrete(limits = compuestos.SinEI)+ #Para que no cambie el orden de los compuestos
  theme_cowplot(line_size = 1) + # Fondo blanco
  theme(legend.position="top", 
        legend.title=element_text(size=16),
        legend.text = element_text(size=18),
        axis.title = element_text(size=20, face = "bold"), #tamaño del nombre de los ejes
        axis.text.x = element_text(angle = 45, hjust = 1,size=18), #tamaño de los compuestos
        axis.text.y = element_text(size=18), #tamaño de los numeros
        plot.margin = margin(0.5, 1, 0, 1, "cm")) + #superior, derecho, abajo, izquierdo
  scale_fill_brewer(palette="Set2")+
  labs(x ="", y= "ug/OD", fill= "Cepa", 
         title = "Todos los compuestos y réplicas")
  
p5

p5 + ylab(expression(bold("\u03BCg/OD"["600nm"])))


# Con facet_wrap podemos graficar cada compuesto en una escala adecuada
ggplot(data = datos)+
  aes_string(x = "Cepa", y = "PorcTIC", fill = "Replicas")+
  geom_bar(stat="identity",width=0.75, position=position_dodge(), color="black")+
  facet_wrap(Comp ~ ., scales = "free")+
  scale_fill_brewer(palette="Set2")+
  labs(x="", y = "%TIC")+ 
  theme_cowplot(line_size = 1) +
  theme(axis.text.x = element_blank())


ggplot(
  #transformo a factores para q no me cambie el orden de los compuestos
  transform(datos, Comp = factor(Comp, levels = compuestosBienEscritos),
                 Cepa = factor(Cepa, levels= cepas),
                 Replicas = factor(Replicas, levels= datosReplicas)))+
  aes_string(x = "Cepa", y = "PorcTIC", fill = "Replicas")+
  geom_bar(stat="identity",width=0.75, position=position_dodge(), color="black")+
  scale_fill_brewer(palette="Set2")+
  facet_wrap(Comp ~ ., scales = "free")+
  labs(x="", y = "%TIC")+
  theme_cowplot(line_size = 1)+
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text = element_text(size=18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12), #Tamaño de los numeros del eje y
        strip.text = element_text(size = 12), #Cambiar el tamaño de letra del texto de facets
        plot.margin = margin(0.5, 1, 0, 1, "cm"))



# PARA GRAFICAR LOS PROMEDIOS, CAMBIO EL DATASET
p6 <- ggplot(
  #transformo a factores para q no me cambie el orden de los compuestos
  transform(datosPromedio, Comp = factor(Comp, levels = compuestosBienEscritos),
            Cepa = factor(Cepa, levels= cepas)))+
  aes_string(x = "Cepa", y = "media.porcTIC", fill = "Cepa")+
  geom_bar(stat="identity",width=0.75, position=position_dodge(), color="black")+
  scale_fill_brewer(palette="Set2")+
  facet_wrap(Comp ~ ., scales = "free")+
  labs(x="", y = "%TIC")+
  theme_cowplot(line_size = 1)+
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text = element_text(size=18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12), #Tamaño de los numeros del eje y
        strip.text = element_text(size = 12), #Cambiar el tamaño de letra del texto de facets
        plot.margin = margin(0.5, 1, 0, 1, "cm"))

p6

p7 <- p6+
  geom_errorbar(aes(ymax = media.porcTIC + desv.porcTIC, 
                    ymin = media.porcTIC - desv.porcTIC,
                    group = Cepa),
                width = .5, size=.5, colour="black", 
                position = position_dodge())

p7


### GUARDAR LA IMAGEN DEFINITIVA #####
png(filename = "AllFAMeanFacets.png", width = 18, height = 10, 
    units = "in", res = 200)
p7
dev.off()


###SELECCIONAR UN SUBSET DE COMPUESTOS #########


CompSatInsatEnOrden <- c("C12", "C13", "C14", "C14:1\u039411",
                         "C15:1\u03947", "C15", "C15:1\u03949",
                         "C16", "C16:1\u03949", "C17", "C17:1\u039410",
                         "C17:1\u03949", "C18", "C18:1\u039411", "C19",
                         "C19:1\u039410")

CompMetEnOrden <- c("2-mC12", "4-mC12", "8-mC12", "2-mC13",
                          "4-mC13", "2-mC14", "8-mC14", "4-mC14", "10-mC14",
                          "12-mC14", "2-mC15", "10-mC15", "2-mC16",
                          "12-mC16")


datos %>%
  filter(Comp %in% CompSatInsatEnOrden) %>%
  ggplot(aes_string(x = "Cepa", y = "media.porcTIC", fill = "Cepa"))+
           geom_bar(data = filter(datosPromedio, Comp %in% CompSatInsatEnOrden), stat="identity",width=0.75, position=position_dodge(), color="black")+
           scale_fill_brewer(palette="Set2")+
           facet_wrap(Comp ~ ., scales = "free")+
           labs(x="", y = "%TIC")+
           theme_cowplot(line_size = 1)+
           theme(legend.position="top",
                 legend.title=element_text(size=16),
                 legend.text = element_text(size=18),
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(size=12), #Tamaño de los numeros del eje y
                 strip.text = element_text(size = 12), #Cambiar el tamaño de letra del texto de facets
                 plot.margin = margin(0.5, 1, 0, 1, "cm"))




### MODULARIZAR CON FUNCIONES #####

source('Funciones.R')
variables <- c("PorcTIC", "ug", "ug/DO")

for (variable in variables){
  print(graficarTodosLosCompuestosyReplicas(datos= datos, X="Comp", 
                                            Y=variable,
                                            Fill="Replicas",
                                            compuestos.SinEI) +
          scale_fill_brewer(palette="Set2")+
          labs(title = "Todos los compuestos y replicas"))
}



# Compuestos en Facets para las 3 variables
for (variable in variables){
  print(graficarCompuestosFacets(datos, compuestos.SinEI, 
                                 Y=variable,
                                 Fill="Replicas")+
          scale_fill_brewer(palette="Set2")+
          labs(title = "Todos los compuestos y réplicas"))
}



graficarCompuestosFacetsPromedio(datosPromedio, CompMetEnOrden,
                                 Y="media.porcTIC", Fill="Cepa")+
  labs(title = "Metilados, réplicas promediadas")+
  scale_fill_brewer(palette="Set2")





#### CALCULO DE OTRAS VARIABLES#####


# CALCULO EL TOTAL DE ug PARA CADA CEPA
Total <-
  datos %>%
  group_by(Cepa, Replicas, DO) %>%
  summarise(total.ug = sum(ug), n = n())%>%
  mutate(total.ug.DO = total.ug/DO)

TotalPromedio <-
  Total %>%
  group_by(Cepa) %>%
  summarise(media.total.ug = mean(total.ug), 
            sd.total.ug = sd(total.ug),
            media.total.ugDO = mean(total.ug.DO), 
            sd.total.ugDO = sd(total.ug.DO),
            media.DO = mean(DO),
            desv.DO = sd(DO),
            n = n())

#TOTAL DE ug/DO SATURADOS E INSAT
TotalSatInsat <-
  datos[(datos$Comp %in% CompSatInsatEnOrden),] %>%
  group_by(Cepa, DO)%>%
  summarise(total.ug.DO = sum(ug/DO), n = n())

TotalSatInsatPromedio <-
  TotalSatInsat %>%
  group_by(Cepa)%>%
  summarise(media.ugDO = mean(total.ug.DO), desv.ugDO = sd(total.ug.DO))


# TOTAL DE ug/DO METILADOS
TotalMetilados <-
  datos[(datos$Comp %in% CompMetHidroxEnOrden),] %>%
  group_by(Cepa, Replicas, DO)%>%
  summarise(total.ug.DO = sum(ug/DO), n = n())

TotalMetiladosPromedio <-
  TotalMetilados %>%
  group_by(Cepa)%>%
  summarise(media.ugDO = mean(total.ug.DO), desv.ugDO = sd(total.ug.DO))

MetiladosPorcientoTotal <- TotalMetilados[,c(1,2,4)]
colnames(MetiladosPorcientoTotal) <- c("Cepa","Replicas","TotalMet.ugDO")
MetiladosPorcientoTotal$"TotalFA.ugDO" <- Total$total.ug.DO
MetiladosPorcientoTotal$"PorcMetDelTotal" <- (MetiladosPorcientoTotal$TotalMet.ugDO / MetiladosPorcientoTotal$TotalFA.ugDO)*100

MetiladosPorcientoTotalPromedio <-
  MetiladosPorcientoTotal %>%
  group_by(Cepa)%>%
  summarise(media.met.porcTotal = mean(PorcMetDelTotal), desv.met.porcTotal = sd(PorcMetDelTotal))


# Grafico OD600, ug totales y ug totales/OD para las 6 replicas #
p1<- graficarTodosLosCompuestosyReplicas(X="Replicas", Y="DO", Fill="Replicas",
                                         datos = Total, Limites = datosReplicas) +
  labs(y= "OD600nm") + 
  #scale_fill_brewer(palette="Set2")+
  geom_text(aes(label= DO), vjust =-0.3, size = 5)+
  theme(axis.text.x = element_blank())+
  guides(fill=guide_legend(nrow=6, title.position= "top"))

legend <- get_legend(p1)

p1 <- p1 + theme(legend.position = "none")

p2 <- graficarTodosLosCompuestosyReplicas(X="Replicas", Y="total.ug", Fill="Replicas",
                                          datos= Total, Limites = datosReplicas) +
  scale_y_continuous(expand=c(0,0),
                     limits = c(0,6000),
                     breaks=seq(0,6000,by=1000))+
  labs(y="Total \u03BCg") + #\u03BC es el unicode para el mu
  theme(axis.text.x = element_blank(),
        legend.position = "none")

p3 <- graficarTodosLosCompuestosyReplicas(X="Replicas", Y="total.ug.DO", Fill="Replicas",
                                          datos = Total, Limites = datosReplicas) + 
  scale_y_continuous(expand=c(0,0),
                     limits = c(0,750),
                     breaks=seq(0,700,by=100))+
  geom_text(aes(label= round(total.ug.DO,1)), vjust =-0.3, size = 5)+
  labs(y="Total \u03BCg/DO")+
  theme(axis.text.x = element_blank(),
        legend.position = "none")
grid.arrange(p1, p2, p3, legend,
             nrow=1,ncol=4,
             top=textGrob("TOTALES",gp = gpar(fontsize=18,font=3)))
