#App for the project  "Quien nos representa"
#Code and Data by Sara Becerro Alonso and Daniel Alvarez
#Last update at 05/12/2019

require(shiny)





require(ggplot2) 

require(gmodels)

require(ggthemes)


require(plotly)
require(plotrix)



#require(Rcpp)
#require(dplyr)
#require(tidyr)
#require(gmodels)
#require(Hmisc)
#require(ggthemes)
#require(plotly)
#require(plotrix)




#leemos los datos volcados en un dataframe
#options(encoding = "latin1")
#options(encoding = "LATIN1")
diputados<-read.csv2("diputados.txt",encoding = "latin1")
#diputados<-read.csv2("diputados.txt",encoding = "latin1")
#UTF-8
#/srv/connect/apps/quien_nos_representa/app.R
#LATIN1

#test<-CrossTable(diputados$SEXO,diputados$G.PARLAMENTARIO,prop.chisq=FALSE)


Lugar<-c("Congreso","Congreso","España","España")#Dataframe para comparar el sexo de la poblacion española con el sexo del congreso
Sexo<-c("Hombres","Mujeres","Hombres","Mujeres")
Porcentaje<-c(60.57,39.43,49.03,50.97)
df<-data.frame(Lugar,Sexo,Porcentaje)

Rango<-c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79")
Lugar_edad<-rep(c("Congreso","España"),c(11,11))
porcentaje_edad<-c(0.57,4.57,8.28,10.85,17.14,17.42,18.86,14.57,5.71,1.14,0.86,5.02,5.62,7.2,8.27,8.1,7.84,7.1,6.07,5.42,4.81,3.49)
df_edad<-data.frame(Rango,Lugar_edad,porcentaje_edad)

Provincia<-c("A coruña","Álava","Albacete","Alicante","Alméria","Asturias","Ávila","Badajoz","Barcelona","Bizkaia","Burgos","Cáceres","Cádiz","Cantabria","Castellón","Ceuta","Ciudad Real","Córdoba","Cuenca","Gipuzkoa","Girona","Granada","Guadalajara","Huelva","Huesca","Islas Baleares","Jaén","La Rioja","Las Palmas","León","Lleida","Lugo","Madrid","Málaga","Melilla","Murcia","Navarra","Ourense","Palencia","Pontevedra","Salamanca","Santa Cruz de Tenerife","Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid","Zamora","Zaragoza","A coruña","Álava","Albacete","Alicante","Alméria","Asturias","Ávila","Badajoz","Barcelona","Bizkaia","Burgos","Cáceres","Cádiz","Cantabria","Castellón","Ceuta","Ciudad Real","Córdoba","Cuenca","Gipuzkoa","Girona","Granada","Guadalajara","Huelva","Huesca","Islas Baleares","Jaén","La Rioja","Las Palmas","León","Lleida","Lugo","Madrid","Málaga","Melilla","Murcia","Navarra","Ourense","Palencia","Pontevedra","Salamanca","Santa Cruz de Tenerife","Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid","Zamora","Zaragoza")
Lugar_origen<-rep(c("Congreso","España"),c(52,52))
porcentaje_origen<-c(2.29,1.14,1.14,3.43,1.71,2.29,0.58,1.71,8.86,2.29,1.14,1.14,2.57,1.43,1.43,0.29,1.43,1.71,0.86,1.71,1.71,2,0.86,1.43,0.86,2.29,1.43,1.14,2.29,1.14,1.14,1.14,10.29,3.14,0.29,2.86,1.43,1.14,0.86,2,1.14,2,0.86,3.43,0.57,1.71,0.86,1.71,4.57,1.43,0.86,2,2.39,0.70,0.83,3.94,1.52,2.2,0.34,1.45,12,2.46,0.76,0.85,2.65,1.24,1.24,0.18,1.07,1.68,0.42,1.54,1.63,1.95,0.54,1.11,0.47,2.42,1.37,0.68,2.37,0.99,0.93,0.71,14.07,3.51,0.19,3.16,1.39,0.66,0.35,2.02,0.71,2.18,0.33,4.15,0.19,1.7,0.29,1.47,5.45,1.11,0.37,2.04)
df_origen<-data.frame(Provincia,Lugar_origen,porcentaje_origen)

estudios<-c("Educación Superior","2ª Etapa E.Secundaria con Orientacion Profesional","Educación Superior","2ª Etapa E.Secundaria con Orientacion Profesional")
lugar_estudios<-c("Congreso","Congreso","España","España")
porcentaje_estudios<-c(92.6,1.7,29.7,8)
df_estudios<-data.frame(estudios,lugar_estudios,porcentaje_estudios)


###########################################################################################
# Define la interfaz de usuario para gestionar los datos de entrada y los datos de salida #
###########################################################################################
ui <- fluidPage(
  fluidRow(
    column(10,
    titlePanel("1.REPRESENTACIÓN POR SEXO")
    
    )
  ),
  fluidRow(
    
    column(9,
              wellPanel(
                checkboxGroupInput("partido","Seleccione uno o varios Partidos Políticos",c("Partido Popular"="PP","Partido Socialista"="PSOE","Unidos Podemos"="Podemos","Ciudadanos"="Ciudadanos","E.Republicana"="Esquerra Republicana","Bildu"="Bildu","Foro"="Foro(G.Mixto)","C-P-EUPV"="C-P-EUPV","Coalicion Canaria"="Coalicion Canaria","Nueva Canarias"="Nueva Canarias","PdeCat"="PdeCat","UPN-PP"="UPN-PP","EAJ-PNV"="EAJ-PNV"),inline = TRUE)
              )
     
  ),
  fluidRow(
    textOutput("etiqueta_1")
    #verbatimTextOutput("etiqueta_1")
  ),

  fluidRow(
   column(12,
    plotlyOutput("sexo.congreso")
    
   
    )
  )
 ),
 fluidRow(
   column(3,
          wellPanel(
            checkboxGroupInput("lugar","Seleccione el lugar respecto del cual desea ver el porcentaje ",c("Diputados del Congreso"="Congreso","Poblacion Española"="España"))
          )
     
   ),
   column(9,
          plotlyOutput("sexo.general")
     
   )
   
   
 ),
 fluidRow(

     titlePanel("2.REPRESENTACIÓN POR ESTADO CIVIL")
 ),
 fluidRow(
   column(12,
          wellPanel(
            checkboxGroupInput("partido_ecivil","Seleccione uno o varios Partidos Políticos",c("Partido Popular"="PP","Partido Socialista"="PSOE","Unidos Podemos"="Podemos","Ciudadanos"="Ciudadanos","E.Republicana"="Esquerra Republicana","Bildu"="Bildu","Foro"="Foro(G.Mixto)","C-P-EUPV"="C-P-EUPV","Coalicion Canaria"="Coalicion Canaria","Nueva Canarias"="Nueva Canarias","PdeCat"="PdeCat","UPN-PP"="UPN-PP","EAJ-PNV"="EAJ-PNV"),inline = TRUE)
          )
     
   )
 ),
 fluidRow(
   
   column(12,
          
          plotlyOutput("ecivil.congreso")
     
     
   )
 ),
 fluidRow(
   
   column(3,
          wellPanel(
            checkboxGroupInput("ecivil","Seleccione uno o varios estados civiles",c("Casado/a"="Casado/a","Divorciado/a"="Divorciado/a","No Consta"="No Consta","Soltero/a"="Soltero/a","Pareja de hecho"="Pareja de hecho","Viudo/a"="Viudo/a"))
          )
 
          
   ),
# ),
# fluidRow(
   column(8,
          plotlyOutput("ecivil.sexo")
          
   )
 ),
fluidRow(
  titlePanel("3.REPRESENTACIÓN POR EDAD")
  
),
fluidRow(
  column(12,
         wellPanel(
           checkboxGroupInput("partido_edad","Seleccione uno o varios Partidos Políticos",c("Partido Popular"="PP","Partido Socialista"="PSOE","Unidos Podemos"="Podemos","Ciudadanos"="Ciudadanos","E.Republicana"="Esquerra Republicana","Bildu"="Bildu","Foro"="Foro(G.Mixto)","C-P-EUPV"="C-P-EUPV","Coalicion Canaria"="Coalicion Canaria","Nueva Canarias"="Nueva Canarias","PdeCat"="PdeCat","UPN-PP"="UPN-PP","EAJ-PNV"="EAJ-PNV"),inline = TRUE)
         )
    
  )
  
),
fluidRow(
  
  plotlyOutput("edad.partidos")
  
  
),
fluidRow(
  column(3,
  wellPanel(
    selectInput("edad_opciones", label = "Elige una variable para representar frente a la edad.", 
                choices = list("Sexo" ="SEXO", "Estado Civil" = "ECIVIL", "Grupo Parlamentario" = "G.PARLAMENTARIO","Nivel Academico"="N.ACADEMICO"),
                selected = "G.PARLAMENTARIO")
    )
  ),
  column(8,
         
         plotlyOutput("edad.varios")
  )
),
fluidRow(
  column(3,
         verbatimTextOutput("etiqueta_2")
    
  ),
  column(8,
        plotlyOutput("edad.lugar")
  )
),
fluidRow(
  
  titlePanel("4.REPRESENTACIÓN POR ORIGEN")
  
),
fluidRow(
  column(3,
 
  wellPanel(
    selectInput("origen", label = "Elige una variable para representar frente a al origen.", 
                choices = list( "Ninguna"="Nada","Sexo" ="SEXO", "Estado Civil" = "ECIVIL", "Grupo Parlamentario" = "G.PARLAMENTARIO","Edad"="Edad","Nivel Academico"="N.ACADEMICO"),
                selected = "G.PARLAMENTARIO")
  )
 ),
 column(8,
        plotlyOutput("origen_varios",height = "800px")
 )
),
 fluidRow(
   column(3,
          verbatimTextOutput("etiqueta_3")
   ),
   column(8,
     plotlyOutput("origen.lugar",height = "800px")
   )
 ),
 fluidRow(
   titlePanel("5.REPRESENTACIÓN POR NIVEL ACADEMICO")
 ),
fluidRow(
  column(12,
         wellPanel(
           checkboxGroupInput("partido_nacademico","Seleccione uno o varios Partidos Políticos",c("Partido Popular"="PP","Partido Socialista"="PSOE","Unidos Podemos"="Podemos","Ciudadanos"="Ciudadanos","E.Republicana"="Esquerra Republicana","Bildu"="Bildu","Foro"="Foro(G.Mixto)","C-P-EUPV"="C-P-EUPV","Coalicion Canaria"="Coalicion Canaria","Nueva Canarias"="Nueva Canarias","PdeCat"="PdeCat","UPN-PP"="UPN-PP","EAJ-PNV"="EAJ-PNV"),inline = TRUE)
         )
  )  
),
  fluidRow(
  column(12,
         plotlyOutput("academico.partidos")
    
  )
),
 fluidRow(
   column(3,
          verbatimTextOutput("etiqueta_4")
   ),
  column(8,
         plotlyOutput("estudios.lugar")
    
  )
 ),
 fluidRow(
   
    verbatimTextOutput("event")
 ),
fluidRow(
  verbatimTextOutput("etiqueta_5")
)
  
)

# Define server logic 
server <- function(input, output) {
#########################
##Gestion etiquetas######
#########################
  
  output$etiqueta_1 <- renderPrint({
    "Puede acceder a mas información acercando el cursor sobre las barras(Nº de diputados, el porcentaje que supone del total de diputados, etc.) Pinchando en cada opción de la leyenda solo aparecerá en la grafica el resto de información.Si realiza un zoom puede volver al estado inicial con un doble click"
  })
  output$etiqueta_2 <- renderPrint({
    "Sobre la leyenda, si efectua un click en Congreso sus correspondientes barras se ocultarán.Si efectua un click sobre España sólo aparecerán los datos acerca del congreso"
     })
  output$etiqueta_3 <- renderPrint({
    "Sobre la leyenda, si efectua un click en Congreso sus correspondientes barras se ocultarán.Si efectua un click sobre España sólo aparecerán los datos acerca del congreso"
  })
  output$etiqueta_4 <- renderPrint({
    "Sobre la leyenda, si efectua un click en Congreso sus correspondientes barras se ocultarán.Si efectua un click sobre España sólo aparecerán los datos acerca del congreso"
  })
  output$etiqueta_5 <- renderPrint({
   "Datos referidos a 2018. ShinyApp creada a partir de R por Sara Becerro Alonso y Daniel Alvarez Hernandez como parte del proyecto XXX" 
     })
  
 ##################################
 #####Gestion de Sexo#############
  ################################
 

 
   
  
  #output$sexo.etiqueta<-renderText({
  # c(paste(paste(round(100*test$prop.col[1,ifelse(input$partido[length(input$partido)]=="PP",11,ifelse(input$partido[length(input$partido)]=="PSOE",12,ifelse(input$partido[length(input$partido)]=="Ciudadanos",3,ifelse(input$partido[length(input$partido)]=="Podemos",10,ifelse(input$partido[length(input$partido)]=="Bildu",1,ifelse(input$partido[length(input$partido)]=="C-P-EUPV",2,ifelse(input$partido[length(input$partido)]=="Coalicion Canaria",4,ifelse(input$partido[length(input$partido)]=="EAJ-PNV",5,ifelse(input$partido[length(input$partido)]=="Esquerra Republicana",6,ifelse(input$partido[length(input$partido)]=="Foro(G.Mixto)",7,ifelse(input$partido[length(input$partido)]=="Nueva Canarias",8,ifelse(input$partido[length(input$partido)]=="PdeCat",9,ifelse(input$partido[length(input$partido)]=="UPN-PP",13,0)))))))))))))],2),"% son hombres en"),input$partido[length(input$partido)]),paste(paste(round(100*test$prop.col[2,ifelse(input$partido[length(input$partido)]=="PP",11,ifelse(input$partido[length(input$partido)]=="PSOE",12,ifelse(input$partido[length(input$partido)]=="Ciudadanos",3,ifelse(input$partido[length(input$partido)]=="Podemos",10,ifelse(input$partido[length(input$partido)]=="Bildu",1,ifelse(input$partido[length(input$partido)]=="C-P-EUPV",2,ifelse(input$partido[length(input$partido)]=="Coalicion Canaria",4,ifelse(input$partido[length(input$partido)]=="EAJ-PNV",5,ifelse(input$partido[length(input$partido)]=="Esquerra Republicana",6,ifelse(input$partido[length(input$partido)]=="Foro(G.Mixto)",7,ifelse(input$partido[length(input$partido)]=="Nueva Canarias",8,ifelse(input$partido[length(input$partido)]=="PdeCat",9,ifelse(input$partido[length(input$partido)]=="UPN-PP",13,0)))))))))))))],2),"% son mujeres en"),input$partido[length(input$partido)]))
    
 # })

   output$sexo.congreso <- renderPlotly({
     ggplot(data=diputados, aes(x=G.PARLAMENTARIO,y=..count.. ,fill=SEXO,label=paste(100*..count../350,"%")))+ 
       geom_bar(position="dodge")+ylab("Nº Diputados")+xlab("Partido Politico")+scale_x_discrete(limit = input$partido)
   })
   output$event <- renderPrint({
     d <- event_data("plotly_hover")
     if (is.null(d)) "Hover on a point!" else d
   })
   
  output$sexo.general<-renderPlotly({
    ggplot(data=df, aes(x=Lugar, y=Porcentaje, fill=Sexo)) + 
      geom_bar(stat="identity", position="dodge")+scale_x_discrete(limit = input$lugar)
    
  })
  
  ##################################
  #####Gestion de Estado civil######
  ##################################
  output$ecivil.congreso <- renderPlotly({
    ggplot(data=diputados, aes(x=G.PARLAMENTARIO,y=..count.. ,fill=ECIVIL,label=paste(100*..count../350,"%"))) + 
      geom_bar(position="dodge")+ylab("Nº Diputados")+xlab("Partido Politico")+scale_x_discrete(limit = input$partido_ecivil)
  })
  output$ecivil.sexo<-renderPlotly({
    
    ggplot(data=diputados, aes(x=ECIVIL,y=..count.. , fill=SEXO,label=paste(100*..count../350,"%")  )) + 
      geom_bar(stat="count")+ylab("Nº de Diputados")+xlab("Estado Civil")+scale_x_discrete(limit = input$ecivil)
  })
  ##############################
  #####Gestion Edad#############
  ##############################
  output$edad.partidos<-renderPlotly({
  
      ggplot(data=diputados, aes(x=G.PARLAMENTARIO,y=..count.. ,fill=RANGO.EDAD,label=paste(100*..count../350,"%"))) + 
        geom_bar(position="dodge")+ylab("Nº Diputados")+xlab("Partido Politico")+scale_x_discrete(limit = input$partido_edad)
  })
  
  output$edad.varios<-renderPlotly({
    if(input$edad_opciones=="SEXO"){
      ggplot(data=diputados, aes(x=RANGO.EDAD,fill=SEXO,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Edad")
      
    }else if(input$edad_opciones=="ECIVIL"){
      ggplot(data=diputados, aes(x=RANGO.EDAD,fill=ECIVIL,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Edad")
      
    }else if(input$edad_opciones=="G.PARLAMENTARIO"){
      ggplot(data=diputados, aes(x=RANGO.EDAD,fill=G.PARLAMENTARIO,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Edad")+scale_fill_manual(values=c("green", "darkred","orange","yellow","darkgreen","#b27503","white","grey","blue","purple","#33d7ff","red","darkblue"))

    }else
    
    ggplot(data=diputados, aes(x=RANGO.EDAD,fill=NIVEL.ACADEMICO,y=100*..count../350   )) + 
      geom_bar(stat="count")+ylab("Porcentaje")+xlab("Edad")
    
  })
  output$edad.lugar<-renderPlotly({
    
    ggplot(data=df_edad, aes(x=Rango, y=porcentaje_edad, fill=Lugar_edad)) + 
      geom_bar(stat="identity", position="dodge")+ylab("Porcentaje")+xlab("Edad")+scale_fill_manual(values=c("black","gray"))
    
  })
  ##########################
  ##Gestion de Origen#######
  ##########################
  
  output$origen_varios<-renderPlotly({
    if(input$origen=="SEXO"){
      ggplot(data=diputados, aes(x=ORIGEN,fill=SEXO,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Origen")+coord_flip()
    }else if(input$origen=="ECIVIL"){
      ggplot(data=diputados, aes(x=ORIGEN,fill=ECIVIL,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Origen")+coord_flip()
     
    }else if(input$origen=="G.PARLAMENTARIO"){
      ggplot(data=diputados, aes(x=ORIGEN,fill=G.PARLAMENTARIO,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Origen")+coord_flip()+scale_fill_manual(values=c("green", "darkred","orange","yellow","darkgreen","#b27503","white","grey","blue","purple","#33d7ff","red","darkblue"))
      
    }else if(input$origen=="Edad"){
      ggplot(data=diputados, aes(x=ORIGEN,fill=RANGO.EDAD,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Origen")+coord_flip()
    }else if(input$origen=="N.ACADEMICO"){
      ggplot(data=diputados, aes(x=ORIGEN,fill=NIVEL.ACADEMICO,y=100*..count../350   )) + 
        geom_bar(stat="count")+ylab("Porcentaje")+xlab("Origen")+coord_flip()
    }else
    ggplot(data=diputados, aes(x=ORIGEN,fill=NULL,y=100*..count../350   )) + 
      geom_bar(stat="count")+ylab("Porcentaje")+xlab("Origen")+coord_flip()
    
  })
  output$origen.lugar<-renderPlotly({
    ggplot(data=df_origen, aes(x=Provincia,fill=Lugar_origen,y=porcentaje_origen)) + 
      geom_bar(stat="identity", position="dodge")+ylab("Porcentaje")+xlab("Origen")+coord_flip()+scale_fill_manual(values=c("black","gray"))
    
    
  })
#########################
##Gestion de N.Academico#
#########################
  output$academico.partidos<-renderPlotly({
    
    ggplot(data=diputados, aes(x=G.PARLAMENTARIO,y=..count.. ,fill=NIVEL.ACADEMICO,label=paste(100*..count../350,"%"))) + 
      geom_bar(position="dodge")+ylab("Nº de Diputados")+xlab("Partido Politico")+scale_x_discrete(limit = input$partido_nacademico)
    
    
  })
  
  output$estudios.lugar<-renderPlotly({
    ggplot(data=df_estudios, aes(x=estudios,fill=lugar_estudios,y=porcentaje_estudios)) + 
      geom_bar(stat="identity", position="dodge")+ylab("Porcentaje")+xlab("Nivel Academico")+scale_fill_manual(values=c("black","gray"))
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

