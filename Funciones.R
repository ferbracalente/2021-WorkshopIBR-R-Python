# GRAFICOS DE BARRAS ------------------------------------------------------
library(tidyr)
library(ggplot2)
library(cowplot)
library(wesanderson)
library(gridExtra)
library(gridGraphics)
library(viridis)


#Creo una funcion para redondear cualquier numero a su decena superior
# Ej: 
#roundUp(2) = 10
# roundUp(7) = 10# roundUp(10) = 20
# roundUp(101) = 200
# roundUp(170) = 200
roundUp <- function(x){
  if(x<100){
    round(x+5, digits = -1)}
  else{
    round(x+50, digits = -2)
  }
}

# Genero una funcion para hacer el grafico de barras de cualquier data set
# datos = dataFrame
# X= nombre de la columna que sera el eje x
# Y= nombre de la columna que ser? el eje y
# Fill= nombre de la columna que determinar? el color de las barras
# Limites = nombre de los compuestos a graficar
graficarTodosLosCompuestosyReplicas <- function(datos, X, Y, Fill, Limites){
  ggplot(data= datos)+
    aes_string(x = X, y = Y, fill = Fill)+
    geom_bar(stat="identity",width=0.75, position=position_dodge(), color="black", linetype = "solid")+
    scale_x_discrete(limits = Limites)+ #Para que no me cambie el orden de los compuestos
    scale_y_continuous(expand =c(0,0),limits=c(0,roundUp(max(datos[,Y], na.rm=T))),
                       breaks=seq(0, roundUp(max(datos[,Y], na.rm=T)), 
                                  by= if(max(datos[,Y], na.rm=T) > 100){50} else {10}))+
    labs(x="")+
    #theme_classic(base_line_size = 1)+
    theme_cowplot(line_size = 1)+
    theme(legend.position="top",
          #legend.title = element_blank(),
          legend.title=element_text(size=16),
          legend.text = element_text(size=18),
          axis.title = element_text(size=20, face = "bold"), #tama?o del nombre de los ejes
          axis.text.x = element_text(angle = 45, hjust = 1,size=18), #tana?o de los compuestos
          axis.text.y = element_text(size=18), #tama?o de los numeros
          plot.margin = margin(0.5, 1, 0, 1, "cm")) #superior, derecho, abajo, izquierdo
}


graficarCompuestosFacets<- function(data, lista_compuestos, Y, Fill){
  data_compuestos <-
    data %>%
    select(Replicas, Cepa, DO, Comp, ug, PorcTIC)%>%
    filter(Comp %in% lista_compuestos)
  ggplot(transform(data_compuestos, Comp = factor(Comp, levels = lista_compuestos),
                   Cepa = factor(Cepa, levels= cepas),
                   Replicas = factor(Replicas, levels= datosReplicas)))+ 
    #el transform es para q no me cambie el orden de los compuestos
    aes_string(x = "Cepa", y = Y, fill = Fill)+
    geom_bar(stat="identity",width=0.75, position=position_dodge(), color="black")+
    facet_wrap(Comp ~ ., scales = "free")+
    labs(x="", y = Y)+ #,y = "%")+
    theme_cowplot(line_size = 1)+
    theme(legend.position="top",
          legend.title=element_text(size=16),
          legend.text = element_text(size=18),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=12), #Tamaño de los numeros del eje y
          strip.text = element_text(size = 12), #Cambiar el tamaño de letra del texto de facets
          plot.margin = margin(0.5, 1, 0, 1, "cm"))#superior, derecho, abajo, izquierdo
}


graficarCompuestosFacetsPromedio<- function(data, lista_compuestos, Y, Fill){
  data_compuestos <-
    data %>%
    filter(Comp %in% lista_compuestos)
  ggplot(transform(data_compuestos, Comp = factor(Comp, levels = lista_compuestos),
                   Cepa = factor(Cepa, levels= cepas)))+ 
    #el transform es para q no me cambie el orden de los compuestos
    {if(Y == "media.ug")aes_string(x = "Cepa", y = "media.ug", fill = Fill)}+
    {if(Y == "media.porcTIC")aes_string(x = "Cepa", y = "media.porcTIC", fill = Fill)}+
    {if(Y == "media.ugDO")aes_string(x = "Cepa", y = "media.ugDO", fill = Fill)}+
    geom_bar(stat="identity",width=0.75, position=position_dodge(), color="black")+
    # Add error bars
    {if(Y == "media.ug")geom_errorbar(aes(ymax = media.ug + desv.ug, 
                                          ymin = media.ug - desv.ug,
                                          group = Cepa ),
                                      width = .5, size=.5, colour="black", position = position_dodge())}+
    {if(Y == "media.porcTIC")geom_errorbar(aes(ymax = media.porcTIC + desv.porcTIC, 
                                               ymin = media.porcTIC - desv.porcTIC,
                                               group = Cepa ),
                                           width = .5, size=.5, colour="black", position = position_dodge())} +
    {if(Y == "media.ugDO")geom_errorbar(aes(ymax = media.ugDO + desv.ugDO, 
                                            ymin = media.ugDO - desv.ugDO,
                                            group = Cepa ),
                                        width = .5, size=.5, colour="black", position = position_dodge())} +
    facet_wrap(Comp ~ ., scales = "free")+
    labs(x="")+#,y = "%")+
    {if(Y == "media.ug") ylab("ug promedio ")}+
    {if(Y == "media.porcTIC") ylab("% ácidos grasos totales")}+
    {if(Y == "media.ugDO") ylab(expression(bold("\u03BCg ácidos grasos/DO"["600nm"])))}+
    theme_cowplot(line_size = 1)+
    theme(legend.position="top",
          legend.title=element_text(size=16),
          legend.text = element_text(size=18),
          axis.title = element_text(size=18, face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=14), #Tamaño de los numeros del eje y
          strip.text = element_text(size = 16), #Cambiar el tamaño de letra del texto de facets
          plot.margin = margin(0.5, 1, 0, 1, "cm"))#superior, derecho, abajo, izquierdo
}

