#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(readr)
library(fdth)
library(plotly)
library(ggcorrplot)
library(corrplot)
library(forecast)
library(dygraphs)
library(xts)
library(dplyr)
library(quantmod)
library(lubridate)
library(rsconnect)
library(leaflet)
library(jsonlite)
library(httr)
library(forecast)
library(MLmetrics)
library(astsa)




calcular_percentil <- function(iv_vector, iv_grupos){
    cortes<-seq(from=(1/iv_grupos),to=1-(1/iv_grupos),by=(1/iv_grupos))
    valores<-NULL   
    for(i in 1:length(cortes)){
        valores[i]<-quantile(iv_vector,cortes[i]) 
    }
    vec_percentil <- cbind(cortes, valores)
    return(vec_percentil)
}
 
rsconnect::setAccountInfo(name='shirleycr2018',
                          token='D2CEEC37EE2E9DC64B46857C888B445B',
                          secret='wnS90/ExFsEscWeWeLRAYofkOP6Mb+6KtbSqkuP2')

calcular_clasificaciones <- function(vec_percentil, iv_vector){
     
    # recodificando notas: asignacion de grupo segun cortes de quantile
    vector_recode_quantile<-rep(0,length(iv_vector))
    
    lv_grupos     <- length(vec_percentil[,2])
    lv_class      <- 1 
    lv_corte_low  <- 0
    
    for(i in 1:lv_grupos){
      
      print(vec_percentil[i,2])
      print(lv_class)
      print(lv_corte_low)
      vector_recode_quantile[iv_vector > lv_corte_low & iv_vector<=vec_percentil[i,2]]<- lv_class
      lv_corte_low <- vec_percentil[i,2]
      lv_class     <- lv_class + 1
      }
    vector_recode_quantile[iv_vector>lv_corte_low]<-lv_class
   
  # vector_recode_quantile[iv_vector>vec_percentil[1,2] & iv_vector<=vec_percentil[2,2]]<-2
  #  vector_recode_quantile[iv_vector>vec_percentil[2,2] & iv_vector<=vec_percentil[3,2]]<-3
  #  vector_recode_quantile[iv_vector>vec_percentil[3,2]]<-4
    
    df<-data.frame(variable=iv_vector,
                   clasificacion1=vector_recode_quantile)
    
   
    return(df)
}

ejecutar_tabfrec <- function(id_data, iv_variable, iv_breaks){
    
    # Hacer grafico de frecuencias     
    x<-fdt(sort(id_data[[iv_variable]] , decreasing = TRUE),k=iv_breaks)

    return(x)
}


get_tabfrec <- function(x){
    
    
    tabla<-x$table
    colnames(tabla)<-c("Clase",
                       "Freq_Abs",
                       "Freq_Rel",
                       "%",
                       "Acum_absdown",
                       "Acum_reldown") 
    r<-tabla$Freq_Abs
    r1<-rev(r)
    r1_acum<-cumsum(r1)
    tabla$Acum_absup <- as.numeric(rev(r1_acum))
    tabla$Acum_relup <- (tabla$Acum_absup / sum(tabla$Acum_absup))*100 
    return(tabla)
}
# Aqu estamos leyendo el archivo 
leer_archivo <- function(iv_filename){
    df_bitStamp <- data.frame(read_csv(iv_filename))
    df_bitStamp$Timestamp <- strptime(df_bitStamp$Timestamp, "%m/%d/%Y")  
    df_bitStamp$Timestamp <- as.Date(df_bitStamp$Timestamp)
    return (df_bitStamp)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    v_cprom   <<- "Prom"
    v_cmedian <<- "Median"
    v_cmoda   <<- "Moda"
    v_cstd    <<- "Std"
    v_var     <<- "Var"
    v_min     <<- "Min" 
    v_max     <<- "Max"
    
    
    df_bitStamp01 <<- leer_archivo("https://raw.githubusercontent.com/shirleycr2018/proyecto_dat04/main/BitStamp.csv")
 
    
    output$table <- DT::renderDataTable({
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_bitStamp 
        })
    
    # Generate a summary of the dataset ----
    
    output$estructura <- renderPrint({str
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                              df_bitStamp01$Timestamp >= v_from & 
                                  df_bitStamp01$Timestamp <= v_to)
        str(df_bitStamp)
    })
    
    output$summary <- renderPrint({str
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        summary(df_bitStamp)
    })
    
    output$plotcor <- renderPlot({ 

        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data    <- df_bitStamp
        
        matriz_cor<-cor(df_data[,c(2,3:8)])
        gg<-ggcorrplot(matriz_cor,
                       hc.order = TRUE, 
                       type = "lower", 
                       lab = TRUE, insig = "blank") +
            ggtitle("Correlograma del conjunto BitStamp")
        gg
        })
    
    output$plotcentro1_3d <- renderPlotly({str
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      df_bitStamp <- subset(df_bitStamp01,
                            df_bitStamp01$Timestamp >= v_from & 
                              df_bitStamp01$Timestamp <= v_to)
      
      df_data    <<- df_bitStamp
      
      fig <- plot_ly(data = df_data, 
                     z = ~Open, 
                     x = ~Timestamp, 
                     y = ~Close, 
                     color = ~Weighted_Price ,
                     colors = c('#0C4B8E' ,'#BF382A'),opacity = 0.5) %>%
                     add_markers( marker = list(size = 2))  %>%
                     layout( xaxis = list(~Timestamp))
      fig
    })
    
    output$plotcentro1 <- renderPlotly({str
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                              df_bitStamp01$Timestamp >= v_from & 
                                  df_bitStamp01$Timestamp <= v_to)
       
        df_data    <<- df_bitStamp

        fig5 <- plot_ly( data = df_data, 
                         x = ~Timestamp, 
                         y = ~Open , 
                        type = "scatter",
                        mode = "lines", 
                        name = "Open",
                        lines = list(color = ~Open)
                        )


        
        if(v_max %in% input$opciones){
            v_y_max <- max(df_data$Open)
            v_x_max <- subset(df_data, df_data$Open == v_y_max )$Timestamp
            
            
            fig5 <- fig5 %>% add_trace( x = v_x_max,
                                        y = v_y_max, 
                                        name ="Max",
                                        marker = list(color = v_x_max,
                                                      size  = 10,
                                                      mode  = 'markers'))
        }

        if(v_min %in% input$opciones){
        v_y_min <- min(df_data$Open)
        v_x_min <- subset(df_data, df_data$Open == v_y_min )$Timestamp
        
        fig5 <- fig5 %>% add_trace( x = v_x_min,
                                    y = v_y_min, 
                                    name ="Min",
                                    marker = list(color = v_x_min,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        
        if(v_cstd %in% input$opciones){
            v_cstd_Open <- sd(df_data$Open)
            fig5 <- fig5 %>% add_trace(  y = v_cstd_Open ,
                                        name ="Sd", 
                                        line= list(color=v_cstd_Open ,dash = 'dash'))
        }
        
        if(v_cprom %in% input$opciones){
        media_o_1   <<- mean(df_data$Open)

        fig5 <- fig5 %>% add_lines(y = media_o_1, 
                                   name = "Media Open", 
                                   line = list(shape = "hv", color="red",dash = 'dash'))
        }
        
        
        if(v_cmedian %in% input$opciones){
        mediana_o_1 <<- median(df_data$Open)
        
        fig5 <- fig5 %>% add_lines(y = mediana_o_1, 
                                   name = "Mediana Open", 
                                   line = list(shape = "hv", color="orange",dash = 'dash'))
        }
        
        if(v_cmoda %in% input$opciones){
        moda_o_1    <<- mfv(df_data$Open)
        fig5 <- fig5 %>% add_lines(y = moda_o_1, 
                                   name = "Moda Open", 
                                   line = list(shape = "hv", color="brown",dash = 'dash'))
        }
        
 
         
        
        fig5 
        
        })
    
    
    
    
    output$vprom_1 <- renderValueBox({
        
        valueBox(value=format(media_o_1, digits=6), subtitle = "Promedio")
      
    })
    
    
    
    output$plotcentro5 <- renderPlotly({str
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data    <<- df_bitStamp
        

        

        
        fig2 <- plot_ly(data = df_data, x = ~Timestamp , y = ~Close , 
                        type = "scatter",
                        mode = "lines", 
                        name = "Close",
                        lines = list(color = ~Close))

        if(v_max %in% input$opciones){
        v_y_max <- max(df_data$Close)
        v_x_max <- subset(df_data, df_data$Close == v_y_max )$Timestamp

        
        fig2 <- fig2 %>% add_trace( x = v_x_max,
                                    y = v_y_max,  
                                    name ="Max",
                                    marker = list(color = v_x_max,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        if(v_min %in% input$opciones){
        v_y_min <- min(df_data$Close)
        v_x_min <- subset(df_data, df_data$Close == v_y_min )$Timestamp
        fig2 <- fig2 %>% add_trace( x = v_x_min,
                                    y = v_y_min, 
                                    name ="Min",
                                    marker = list(color = v_x_min,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        
        if(v_cstd %in% input$opciones){
            v_cstd_Close <- sd(df_data$Close)
          
            fig2 <- fig2 %>% add_trace( y = v_cstd_Close,
                                        name ="Sd", 
                                        line= list(color=v_cstd_Close,dash = 'dash'))
        }
        
        if(v_cprom %in% input$opciones){
        media_c    <- mean(df_data$Close)
        fig2 <- fig2 %>% add_lines(y = media_c, 
                                   name = "Media Close", 
                                   line = list(shape = "hv", color="red",dash = 'dash'))
        }
        if(v_cmedian %in% input$opciones){
        mediana_c  <- median(df_data$Close)
        fig2 <- fig2 %>% add_lines(y = mediana_c, 
                                   name = "Mediana Close", 
                                   line = list(shape = "hv", color="orange",dash = 'dash'))
        }
        if(v_cmoda %in% input$opciones){
        moda_c     <- mfv(df_data$Close)
        fig2 <- fig2 %>% add_lines(y = moda_c, 
                                   name = "Moda Close", 
                                   line = list(shape = "hv", color="brown",dash = 'dash'))
        }
        fig2
    })
    
    output$plotcentro6 <- renderPlotly({str
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data   <<- df_bitStamp
        
    
        

        
        fig2 <- plot_ly( data = df_data, x = ~Timestamp      , y = ~Weighted_Price , type = "scatter",
                        mode = "lines", lines = list(color= ~Weighted_Price))
        
        if(v_max %in% input$opciones){
        v_y_max <- max(df_data$Weighted_Price)
        v_x_max <- subset(df_data, df_data$Weighted_Price == v_y_max )$Timestamp

        fig2 <- fig2 %>% add_trace( x = v_x_max,
                                    y = v_y_max, 
                                    name ="Max",
                                    marker = list(color = v_x_max,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        if(v_min %in% input$opciones){
        v_y_min <- min(df_data$Weighted_Price)
        v_x_min <- subset(df_data, df_data$Weighted_Price == v_y_min )$Timestamp
        fig2 <- fig2 %>% add_trace( x = v_x_min,
                                    y = v_y_min, 
                                    name ="Min",
                                    marker = list(color = v_x_min,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        
        if(v_cstd %in% input$opciones){
            v_cstd_Weighted_Price <- sd(df_data$Weighted_Price)
            fig2 <- fig2 %>% add_trace(  y = v_cstd_Weighted_Price,
                                        name ="Sd", 
                                        line= list(color=v_cstd_Weighted_Price,dash = 'dash'))
        }
        
        
        if(v_cprom %in% input$opciones){
        media_w   <- mean(df_data$Weighted_Price)
        fig2 <- fig2 %>% add_lines(y = media_w, name = "Media WP", line = list(shape = "hv", color="red",dash = 'dash'))
        }
        if(v_cmedian %in% input$opciones){
        mediana_w <- median(df_data$Weighted_Price)
        fig2 <- fig2 %>% add_lines(y = mediana_w, name = "Mediana WP", line = list(shape = "hv", color="orange",dash = 'dash'))
        }
        if(v_cmoda %in% input$opciones){
         moda_w    <- mfv(df_data$Weighted_Price)
        fig2 <- fig2 %>% add_lines(y = moda_w, name = "Moda WP", line = list(shape = "hv", color="brown",dash = 'dash'))
        }
        fig2
        
    })
    
    output$plotcentro7 <- renderPlotly({str  
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data    <<- df_bitStamp
        

        

        
        fig3 <- plot_ly(data=df_data, x = ~Timestamp      , y = ~High , type = "scatter",
                        mode = "lines", lines = list(color= ~High))
       
        if(v_max %in% input$opciones){
        v_y_max <- max(df_data$High)
        v_x_max <- subset(df_data, df_data$High == v_y_max )$Timestamp

        fig3 <- fig3 %>% add_trace( x = v_x_max,
                                    y = v_y_max, 
                                    name ="Max",
                                    marker = list(color = v_x_max,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        if(v_min %in% input$opciones){
        v_y_min <- min(df_data$High)
        v_x_min <- subset(df_data, df_data$High == v_y_min )$Timestamp
        fig3 <- fig3 %>% add_trace( x = v_x_min,
                                    y = v_y_min, 
                                    name ="Min",
                                    marker = list(color = v_x_min,
                                                  size  = 10,
                                                  mode  = 'markers')) 
        }
        
        if(v_cstd %in% input$opciones){
            v_cstd_High <- sd(df_data$High)
            fig3 <- fig3 %>% add_trace(  y = v_cstd_High,
                                        name ="Sd", 
                                        line= list(color=v_cstd_High,dash = 'dash'))
        }
        
        if(v_cprom %in% input$opciones){
        media_h    <- mean(df_data$High)
        fig3 <- fig3 %>% add_lines(y = media_h, name = "Media High", line = list(shape = "hv", color="red",dash = 'dash'))
        }
        if(v_cmedian %in% input$opciones){
        mediana_h  <- median(df_data$High)
        fig3 <- fig3 %>% add_lines(y = mediana_h, name = "Mediana High", line = list(shape = "hv", color="orange",dash = 'dash'))
        }
        if(v_cmoda %in% input$opciones){
        moda_h     <- mfv(df_data$High) 
        fig3 <- fig3 %>% add_lines(y = moda_h, name = "Moda High", line = list(shape = "hv", color="brown",dash = 'dash'))
        }
        fig3
        
        })
    
    output$plotcentro2 <- renderPlotly({str
        
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)

        df_data    <<- df_bitStamp
        
        
        fig4 <- plot_ly(data=df_data, x = ~Timestamp      , y = ~Low ,type = "scatter",
                        mode = "lines", lines = list(color= ~Low))
        if(v_max %in% input$opciones){
        v_y_max <- max(df_data$Low)
        v_x_max <- subset(df_data, df_data$Low == v_y_max )$Timestamp
        fig4 <- fig4 %>% add_trace( x = v_x_max,
                                    y = v_y_max, 
                                    name ="Max",
                                    marker = list(color = v_x_max,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        if(v_min %in% input$opciones){
        v_y_min <- min(df_data$Low)
        v_x_min <- subset(df_data, df_data$Low == v_y_min )$Timestamp
        fig4 <- fig4 %>% add_trace( x = v_x_min,
                                    y = v_y_min, 
                                    name ="Min",
                                    marker = list(color = v_x_min,
                                                  size  = 10,
                                                  mode  = 'markers')) 
        }
        
        if(v_cstd %in% input$opciones){
            v_cstd_Low <- sd(df_data$Low)
            
            fig4 <- fig4 %>% add_trace(  y = v_cstd_Low,
                                        name ="Sd", 
                                        line= list(color=v_cstd_Low,dash = 'dash'))
        }
        
       # fig4 <- fig4 %>% add_lines(name = "Low",line= list(color="purple"))
        if(v_cprom %in% input$opciones){
        media_l    <- mean(df_data$Low)
        fig4 <- fig4 %>% add_lines(y = media_l, name = "Media Low", line = list(shape = "hv", color="red",dash = 'dash'))
        }
        if(v_cmedian %in% input$opciones){
        mediana_l  <- median(df_data$Low)
        fig4 <- fig4 %>% add_lines(y = mediana_l, name = "Mediana Low", line = list(shape = "hv", color="orange",dash = 'dash'))
        }
        if(v_cmoda %in% input$opciones){
        moda_l     <- mfv(df_data$Low)
        fig4 <- fig4 %>% add_lines(y = moda_l, name = "Moda Low", line = list(shape = "hv", color="brown",dash = 'dash'))
        }
        fig4
        
    })
    
    output$plotcentro3 <- renderPlotly({
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data     <<- df_bitStamp
        
        fig3 <- plot_ly(data=df_data, x = ~Timestamp, y = ~Volume_Currency ,type = "scatter",
                        mode = "lines", lines = list(color= ~Volume_Currency))
        if(v_max %in% input$opciones){
        v_y_max <- max(df_data$Volume_Currency)
        v_x_max <- subset(df_data, df_data$Volume_Currency == v_y_max )$Timestamp
        fig3 <- fig3 %>% add_trace( x = v_x_max,
                                    y = v_y_max, 
                                    name ="Max",
                                    marker = list(color = v_x_max,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        if(v_min %in% input$opciones){
        v_y_min <- min(df_data$Volume_Currency)
        v_x_min <- subset(df_data, df_data$Volume_Currency == v_y_min )$Timestamp
        fig3 <- fig3 %>% add_trace( x = v_x_min,
                                    y = v_y_min, 
                                    name ="Min", 
                                    marker = list(color = v_x_min,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        if(v_cstd %in% input$opciones){
            v_cstd_Volume_Currency <- sd(df_data$Volume_Currency)
            fig3 <- fig3 %>% add_trace(  y = v_cstd_Volume_Currency,
                                        name ="Sd", 
                                        line= list(color=v_cstd_Volume_Currency,dash = 'dash'))
          }
        
        if(v_cprom %in% input$opciones){
        media_vc    <- mean(df_data$Volume_Currency)
        fig3 <- fig3 %>% add_lines(y= media_vc, name = "Media",line= list(shape = "hv",color="red",dash = 'dash'))
        }
        
        if(v_cmedian %in% input$opciones){
        mediana_vc  <- median(df_data$Volume_Currency)
        fig3 <- fig3 %>% add_lines(y= mediana_vc, name = "Mediana",line= list(shape = "hv",color="orange",dash = 'dash'))
        }
        
        if(v_cmoda %in% input$opciones){
        moda_vc     <- mfv(df_data$Volume_Currency)
        fig3 <- fig3 %>% add_lines(y= moda_vc, name = "Moda",line= list(shape = "hv",color="brown",dash = 'dash'))
        }
        fig3 
        
    })
    
    output$plotcentro4 <- renderPlotly({
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data    <<- df_bitStamp
        
        

        

        
        fig4 <- plot_ly(data=df_data, x = ~Timestamp      , y = ~Volume_BTC, type = "scatter",
                        mode = "lines", lines = list(color= ~Volume_BTC))
        
        if(v_max %in% input$opciones){
        v_y_max <- max(df_data$Volume_BTC)
        v_x_max <- subset(df_data, df_data$Volume_BTC == v_y_max )$Timestamp
        fig4 <- fig4 %>% add_trace( x = v_x_max,
                                    y = v_y_max, 
                                    name ="Max",
                                    marker = list(color = v_x_max,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        if(v_min %in% input$opciones){
        v_y_min <- min(df_data$Volume_BTC)
        v_x_min <- subset(df_data, df_data$Volume_BTC == v_y_min )$Timestamp
        fig4 <- fig4 %>% add_trace( x = v_x_min,
                                    y = v_y_min, 
                                    name ="Min",
                                    marker = list(color = v_x_min,
                                                  size  = 10,
                                                  mode  = 'markers'))
        }
        
        if(v_cstd %in% input$opciones){
            v_cstd_Volume_BTC <- sd(df_data$Volume_BTC)
            fig4 <- fig4 %>% add_trace(  y = v_cstd_Volume_BTC,
                                        name ="Sd", 
                                        line= list(color=v_cstd_Volume_BTC,dash = 'dash'))
        }
        

        
        if(v_cprom %in% input$opciones){
        media_vb    <- mean(df_data$Volume_BTC)
        fig4 <- fig4 %>% add_lines(y= media_vb, name = "Media",line= list(shape = "hv",color="red",dash = 'dash'))
        }
        
        if(v_cmedian %in% input$opciones){
        mediana_vb  <- median(df_data$Volume_BTC)
        fig4 <- fig4 %>% add_lines(y= mediana_vb, name = "Mediana",line= list(shape = "hv",color="orange",dash = 'dash'))
        }
        
        if(v_cmoda %in% input$opciones){
        moda_vb     <- mfv(df_data$Volume_BTC)
        fig4 <- fig4 %>% add_lines(y= moda_vb, name = "Moda",line= list(shape = "hv",color="brown",dash = 'dash'))
        }
        fig4 
        
    })
    
    
    output$plotCandles <- renderPlotly({
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data <- df_bitStamp
        df_data$direccion <- df_data$Close > df_data$Open
        
        fig1 <- df_data %>% plot_ly(x = ~Timestamp, type="candlestick",
                              open = ~Open, close = ~Close,
                              high = ~High, low = ~Low) 
        fig1 <- fig1 %>% layout(title = "Precios")
        
        
        
        fig2 <-  df_data %>% plot_ly(x=~Timestamp, y=~Volume_BTC, type='bar',color = ~direccion, colors = c('red','green'), name = "Volume BTC") 
        fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
        
        fig <- subplot(fig1, fig2, nrows=2,shareX = TRUE, titleY = TRUE)
        fig
    })

 
    output$table_frec <- DT::renderDataTable({
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data <<- df_bitStamp
        lv_variable <- input$cname1
        
        
        
        lv_breaks   <-  as.numeric(input$slider1)
        
        xx <- ejecutar_tabfrec(df_data, lv_variable, lv_breaks)  
        ls_breaks <- xx$breaks
        ls_breaks
        tabla     <- get_tabfrec(xx)
        tabla_1   <<- tabla
        tabla_1

    })
    
    
    output$cpercent = renderUI({
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data <- df_bitStamp
        
        #df_quantiles <- quantile(df_data[[lv_variable]], na.rm=TRUE, names = TRUE, type = 7)
        
        lv_quantil <- input$cuantiles1
        print(lv_quantil)
        
        vec_percentil <- calcular_percentil(sort(df_data[[lv_variable]]), lv_quantil)
        df_percentil  <- calcular_clasificaciones(vec_percentil, sort(df_data[[lv_variable]]))
        selectInput('cpercent', 'Cuantile', df_percentil$clasificacion1)
        
    })
    
    output$plotfrec1 <- renderPlotly({str   
        
        lv_breaks   <-  as.numeric(input$slider1)
        lv_variable <- input$cname1
        
        v_x <- req( 1: lv_breaks)

        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data <- df_bitStamp
        
        
        
        xx <- ejecutar_tabfrec(df_data, lv_variable, lv_breaks)  
        
        ls_breaks <- xx$breaks
        ls_breaks <<- ls_breaks
        lv_start  <- ls_breaks['start'] 
        lv_end    <- ls_breaks['end']
        lv_h      <- ls_breaks['h'] 
        tabla     <- get_tabfrec(xx)
        tabla_1   <<- tabla
        lv_xs     <- seq((lv_start+(lv_h/2)), lv_end, by=lv_h )
        
        p <- ggplot(tabla_1,aes(x=lv_xs, na.rm=TRUE))+
            scale_x_discrete(name="Class", limits=lv_xs)+
            ylab("Frecuencias Acumuladas")+
            xlab(lv_variable)+
            geom_line( aes(y= Acum_absdown, color = "Acum_absdown"))+
            geom_line( aes(y= Acum_absup, color = "Acum_absup"))+
            geom_point( aes(y= Acum_absdown, color = "Acum_absdown"))+
            geom_point( aes(y= Acum_absup , color = "Acum_absup"))               
        fig <- ggplotly(p)
        
        fig
    })
    
    
    output$percboxplot <- renderPlotly({
      
      lv_variable <- input$cname1
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      df_bitStamp <<- subset(df_bitStamp01,
                             df_bitStamp01$Timestamp >= v_from & 
                               df_bitStamp01$Timestamp <= v_to)
      
      df_data    <<- df_bitStamp
      lv_quantil <- input$cuantiles1
      print(lv_quantil)
      
      vec_percentil <- calcular_percentil(sort(df_data[[lv_variable]]), lv_quantil)
      df_percentil  <- calcular_clasificaciones(vec_percentil, sort(df_data[[lv_variable]]))
      
      lv_percentil <- input$percentilsel 
      
#      if (lv_percentil == "1"){
#        df_percentil <- subset(df_percentil, clasificacion1 == "4")
#      }
#      if(lv_percentil == "0.25"){
#        df_percentil <- subset(df_percentil, clasificacion1 == "1")
#      }
#      if(lv_percentil == "0.5"){
#        df_percentil <- subset(df_percentil, clasificacion1 == "2")
#      }
#      if(lv_percentil == "0.75"){
#        df_percentil <- subset(df_percentil, clasificacion1 == "3")
#      }
      
      fig <- plot_ly(df_percentil, y = ~variable, type = "box")
      
      fig
    })
    
    output$perchistogram <- renderPlotly({ 
        
        
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        
        df_data    <<- df_bitStamp
        
        
        lv_quantil <- input$cuantiles1
        print(lv_quantil)
       
        
        vec_percentil <- calcular_percentil(sort(df_data[[lv_variable]]), as.numeric(lv_quantil))
        df_percentil  <- calcular_clasificaciones(vec_percentil, sort(df_data[[lv_variable]]))
        
        lv_percentil <- input$percentilsel 
        
        if (lv_percentil == "1"){
          df_percentil <- subset(df_percentil, clasificacion1 == lv_quantil)
        }else{
          if(lv_percentil != "Todos"){
            lv_index <- match(  lv_percentil, vec_percentil)
            print(lv_index)
            df_percentil <- subset(df_percentil, clasificacion1 == lv_index) 
          }
            
        }
        
        x <- list(title = lv_variable)
        fig <- plot_ly(df_percentil, x = ~variable   ,  type = "histogram")
        fig <- fig %>% layout(xaxis = x)
        fig
        
        })
    
    
    
    output$plotfrec2 <- renderPlotly({
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        
        df_data    <<- df_bitStamp
        
        #vec_percentil <- calcular_percentil(sort(df_data[[lv_variable]]), 4)
        #df_percentil  <- calcular_clasificaciones(vec_percentil, sort(df_data[[lv_variable]]))
        
        fig <- plot_ly(data = df_data, 
                       y = df_data[[lv_variable]], 
                       type = "box", 
                       quartilemethod="linear", 
                       name="Linear Quartile")
      #  fig <- fig %>% add_trace(data = df_data,y = df_data[[lv_variable]], quartilemethod="inclusive", name="Inclusive Quartile")
      #  fig <- fig %>% add_trace(data = df_data,y = df_data[[lv_variable]], quartilemethod="exclusive", name="Exclusive Quartile")
        fig <- fig %>% layout(title = "Quartile")
        fig
        
    })
     
    output$multiregresion <- renderPlotly({
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        
        df_data    <<- df_bitStamp
        
        fig <- plot_ly(data = df_data, 
                       z = ~Close, 
                       x = ~Open, 
                       y = ~Low, 
                       color = ~High, 
                      # colors = c('#0C4B8E' ,'#BF382A'),
                       opacity = 0.5) %>%
            add_markers( marker = list(size = 2)) 
        fig
    })    
    

    
    output$percentilsel <- renderUI({
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        
        df_data    <<- df_bitStamp
        lv_quantil <- input$cuantiles1
        vec_percentil <- calcular_percentil(sort(df_data[[lv_variable]]), as.numeric(lv_quantil))
        df_vec <- data.frame(vec_percentil)
        selectInput("percentilsel", "Percentil", c(df_vec$cortes,"1","Todos"), selected="Todos") 
        
    })
    

    
    output$percentilvalue <- renderPrint({ 
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        
        df_data    <<- df_bitStamp
        lv_quantil <- input$cuantiles1
        vec_percentil <- calcular_percentil(sort(df_data[[lv_variable]]), as.numeric(lv_quantil))
        vec_percentil
        
    })
    
    output$cortabtext <- renderPrint({  
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data    <<- df_bitStamp
        df_cor <- cor(df_data[,-1])
        df_cor
        })
    
    output$modelotext <- renderPrint({ 
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        df_data    <<- df_bitStamp
        mod1 <- lm(formula=   High ~ Low + Open + Close +   Weighted_Price , data=df_data)
        summary(mod1)
        
        })
    
    output$confittext <- renderPrint({ 
      lv_variable <- input$cname1
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      df_bitStamp <<- subset(df_bitStamp01,
                             df_bitStamp01$Timestamp >= v_from & 
                               df_bitStamp01$Timestamp <= v_to)
      df_data    <<- df_bitStamp
      mod1 <- lm(formula=   High ~ Low + Open + Close +   Weighted_Price , data=df_data)
      confint(mod1) 
      })
    
    output$residualtext <- renderPrint({ 
      lv_variable <- input$cname1
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      df_bitStamp <<- subset(df_bitStamp01,
                             df_bitStamp01$Timestamp >= v_from & 
                               df_bitStamp01$Timestamp <= v_to)
      df_data    <<- df_bitStamp
      mod1 <- lm(formula=   High ~ Low + Open + Close +   Weighted_Price , data=df_data)
      anova(mod1) 
    })    
    
    output$percentiltab <- DT::renderDataTable({ 
        
        lv_variable <- input$cname1
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_bitStamp <<- subset(df_bitStamp01,
                               df_bitStamp01$Timestamp >= v_from & 
                                   df_bitStamp01$Timestamp <= v_to)
        
        df_data    <<- df_bitStamp
        
        vec_percentil <- calcular_percentil(sort(df_data[[lv_variable]]), 4)
        df_percentil  <- calcular_clasificaciones(vec_percentil, sort(df_data[[lv_variable]]))
        df_percentil
        })
    
    
    
    output$dygraphopenclose <- renderDygraph({  

        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        dateWindow <- c(v_from, v_to)
        

        df_data    <<- df_bitStamp01
        
        vp_OpenClose <- df_data %>% group_by(month=floor_date(Timestamp, "month")) %>% 
                                summarize(Open  = mean(Open),
                                          Close = mean(Close),
                                          Volume_BTC = mean(Volume_BTC))
        vp_OpenClose_ps <- ts(vp_OpenClose[,-1], frequency = 12,start = c(2013,1))
        vp_OpenClose_ps <- window(vp_OpenClose_ps, 
                                   start=c(year(v_from),month(v_from)), 
                                   end=c(year(v_to),  month(v_to)))
        
        
        vp_prices <- vp_OpenClose_ps
        
        
        dygraph(vp_prices, 
                main  = "Open vs Close",
                ylab  = "mean(prices)",
                group = "prices-gf") %>%
           dyRangeSelector(dateWindow = dateWindow)%>%
            dyHighlight(highlightCircleSize = 5, 
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE)
        
            })
    
    

    
    output$dygraphpredic <- renderDygraph({ 
      
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      dateWindow <- c(v_from, v_to)
      
      
      df_data    <<- df_bitStamp01
      lv_variable <- input$cname3
      

      
      if(lv_variable == "Open"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Open  = mean(Open))
      }
      if(lv_variable == "Close"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Close  = mean(Close))
      }
      if(lv_variable == "High"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(High  = mean(High))
      }
      if(lv_variable == "Low"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Low  = mean(Low))
      }
      
      if(lv_variable == "Volume_BTC"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_BTC  = mean(Volume_BTC))
      }
      
      if(lv_variable == "Volume_Currency"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_Currency  = mean(Volume_Currency))
      }
      if(lv_variable == "Weighted_Price"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Weighted_Price  = mean(Weighted_Price))
      }
      
      
      vp_variable_ps   <- ts(vp_variable[,-1], frequency = 12,start = c(year(v_from),month(v_from)), end=c(year(v_to),month(v_to)))
      
      hw_object     <<-HoltWinters(vp_variable_ps)
      lv_periodpred <- input$proyecthw
      hw_predict    <<- predict(hw_object,n.ahead =lv_periodpred, prediction.interval = TRUE)
      
      all <- cbind(vp_variable_ps, hw_predict)
      
     # lv_new_to <- as.Date.yearmon(time()[which.max(hw_predict)])
      
    #  dateWindow <- c(v_from, lv_new_to) 
      
      dygraph(all, lv_variable) %>%
        dyRangeSelector(dateWindow = dateWindow)%>%
        dySeries("vp_variable_ps", label = "Actual") %>%
        dySeries(c("hw_predict.lwr", "hw_predict.fit", "hw_predict.upr"), label = "Predicted")
      
#      plot(hw_object,hw_predict)
      #hw <- HoltWinters(vp_variable_ps, beta=FALSE, gamma=FALSE)
#     md   <- auto.arima(vp_variable_ps)
#     md_forecast <- forecast(md, lv_periodpred) 

      })
    
    
    output$dygraphpredicfore <- renderPlot({ 
      

      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      dateWindow <- c(v_from, v_to)
      
      
      df_data    <<- df_bitStamp01
      lv_variable <- input$cname3
      
      
      
      if(lv_variable == "Open"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Open  = mean(Open))
      }
      if(lv_variable == "Close"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Close  = mean(Close))
      }
      if(lv_variable == "High"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(High  = mean(High))
      }
      if(lv_variable == "Low"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Low  = mean(Low))
      }
      
      if(lv_variable == "Volume_BTC"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_BTC  = mean(Volume_BTC))
      }
      
      if(lv_variable == "Volume_Currency"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_Currency  = mean(Volume_Currency))
      }
      if(lv_variable == "Weighted_Price"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Weighted_Price  = mean(Weighted_Price))
      }
      
      
      vp_variable_ps   <- ts(vp_variable[,-1], frequency = 12,start = c(year(v_from),month(v_from), end=c(year(v_to),month(v_to))))
      
      hw_object     <<-HoltWinters(vp_variable_ps)
      lv_periodpred <- input$proyecthw
      hw_forecast   <- forecast(hw_object,lv_periodpred, level=c(95))
      autoplot(hw_forecast)
       
    })
    
    
    

    

    
    
    output$dygraphpredic2 <- renderDygraph({ 
      
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      dateWindow <- c(v_from, v_to)
 
      
      df_data    <<- df_bitStamp01
      lv_variable <- input$cname2
      
      
      
      if(lv_variable == "Open"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Open  = mean(Open))
      }
      if(lv_variable == "Close"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Close  = mean(Close))
      }
      if(lv_variable == "High"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(High  = mean(High))
      }
      if(lv_variable == "Low"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Low  = mean(Low))
      }
      
      if(lv_variable == "Volume_BTC"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_BTC  = mean(Volume_BTC))
      }
      
      if(lv_variable == "Volume_Currency"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_Currency  = mean(Volume_Currency))
      }
      if(lv_variable == "Weighted_Price"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Weighted_Price  = mean(Weighted_Price))
      }
      
      
      vp_variable_ps   <- ts(vp_variable[,-1], frequency = 12,start = c(year(v_from),month(v_from), end=c(year(v_to),month(v_to))))
      
      lv_periods <- input$perproyeccion
      
      md <<- auto.arima(vp_variable_ps)
      
      df_predict <- predict(md, n.ahead= lv_periods, level=c(95))
      
      actual    <- vp_variable_ps 
    
      all <- cbind(actual,df_predict$se, df_predict$pred)
      
    #  lv_new_to <- as.Date.yearmon(time(df_predict$pred)[which.max(df_predict$pred)])
     # dateWindow <- c(v_from, lv_new_to) 
      
      
      dygraph(all, lv_variable) %>%
        dyRangeSelector(dateWindow = dateWindow)%>%
        dySeries("actual", label = "Actual") %>%
        dySeries("df_predict$se", label = "Se") %>%
        dySeries("df_predict$pred", label = "Predicted")
      
    })
    
    output$arimaforecast <- renderPlot({
      
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      dateWindow <- c(v_from, v_to)
      
      
      df_data    <<- df_bitStamp01
      lv_variable <- input$cname2
      
      
      
      if(lv_variable == "Open"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Open  = mean(Open))
      }
      if(lv_variable == "Close"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Close  = mean(Close))
      }
      if(lv_variable == "High"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(High  = mean(High))
      }
      if(lv_variable == "Low"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Low  = mean(Low))
      }
      
      if(lv_variable == "Volume_BTC"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_BTC  = mean(Volume_BTC))
      }
      
      if(lv_variable == "Volume_Currency"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Volume_Currency  = mean(Volume_Currency))
      }
      if(lv_variable == "Weighted_Price"){
        vp_variable <- df_data %>% 
          group_by(month=floor_date(Timestamp, "month")) %>% 
          summarize(Weighted_Price  = mean(Weighted_Price))
      }
      
      
      vp_variable_ps   <- ts(vp_variable[,-1], frequency = 12,start = c(year(v_from),month(v_from), end=c(year(v_to),month(v_to))))
      
      lv_periods <- input$perproyeccion
      
      md <<- auto.arima(vp_variable_ps)
      
      df_forecast1 <- forecast(md, h= lv_periods,level=c(95))
      fig <- autoplot(df_forecast1)
      fig
      
    })
    

    
      output$hw_md <- renderPrint({ 
         
         
         hw_object
         
         
        })
      
      output$arima_md <- renderPrint({ 
        
        
        md 
        
      })
    
    
    output$dygraphlowhigh <- renderDygraph({  
        

        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        
        dateWindow <- c(v_from, v_to)
        df_data    <<- df_bitStamp01
        
        vp_LowHigh <- df_data %>% group_by(month=floor_date(Timestamp, "month")) %>% 
                                  summarize(Low  = mean(Low),
                                            High = mean(High),
                                            Volume_BTC = mean(Volume_BTC))
        
        vp_LowHigh_ps <- ts(vp_LowHigh[,-1], frequency = 12,start = c(2013,1))
        vp_LowHigh_ps <- window(vp_LowHigh_ps, 
                                   start=c(year(v_from),month(v_from)), 
                                   end=c(year(v_to),  month(v_to)))
        
        vp_prices <- vp_LowHigh_ps
        
        dygraph(vp_prices,  
                main = "Low vs High", 
                ylab = "mean(Prices)",
                group = "prices-gf") %>%
            dyRangeSelector(dateWindow = dateWindow)%>%
            dyHighlight(highlightCircleSize = 5, 
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE)
        
        })
    
    
    output$dygraphdias <- renderDygraph({ 
      
      v_from <<- as.Date(input$daterange01[1])
      v_to   <<- as.Date(input$daterange01[2])
      dateWindow <- c(v_from, v_to)
      df_data    <<- df_bitStamp01
      
      vp_LowHigh <-  df_data[c("Timestamp","Low","High","Open","Close","Weighted_Price")] 
      vp_LowHigh$Timestamp <- factor(format(vp_LowHigh$Timestamp , format="%Y-%m-%d %H:%M"))
      vp_LowHigh.xts <- xts(vp_LowHigh[,-1], as.Date(vp_LowHigh$Timestamp))
      
        dygraph(vp_LowHigh.xts,  
                main = "Prices", 
                ylab = "mean(Prices)")%>%
          dyRangeSelector(dateWindow = dateWindow) 
   
      
    })
    

    
    output$dygraphcandle <- renderDygraph({ 
        
        v_from <<- as.Date(input$daterange01[1])
        v_to   <<- as.Date(input$daterange01[2])
        df_data    <<- df_bitStamp01
        
        
#        vp_allprices <- df_data %>% group_by(month=floor_date(Timestamp, "month")) %>% 
#            summarize(
#                       Open = mean(Open),
#                      High = mean(High),
#                      Low  = mean(Low),
#                       Close = mean(Close),
#                       Volume_BTC = mean(Volume_BTC))
        
        vp_allprices <-  df_data[c("Timestamp","Low","High","Open","Close","Weighted_Price")] 
        vp_allprices$Timestamp <- factor(format(vp_allprices$Timestamp , format="%Y-%m-%d %H:%M"))
        vp_allprices.xts <- xts(vp_allprices[,-1], as.Date(vp_allprices$Timestamp))
        
        
        dateWindow <- c(v_from, v_to)
        
#        vp_allprices_ps <- ts(vp_allprices[,-1], frequency = 12,start = c(2013,1))
#        vp_allprices_ps <- window( vp_allprices_ps, 
#                                   start=c(year(v_from),month(v_from)), 
#                                   end=c(year(v_to),  month(v_to)))
        
        dygraph(vp_allprices.xts,  
                main = "Prices", 
                ylab = "mean(Prices)") %>%
            dyRangeSelector(dateWindow = dateWindow)%>%
            dyCandlestick(compress = TRUE)        
        })
    
    output$bbmap <- renderLeaflet({
       
      res_venues <- GET("https://raw.githubusercontent.com/shirleycr2018/proyecto_dat04/main/Venues.json")
      
      df_venues  <- data.frame(fromJSON(rawToChar(res_venues$content))) 
      
    pal <- colorFactor(  topo.colors(26) , levels(as.factor(df_venues$venues.category)))
  
    map <-    leaflet(df_venues)%>%
              addCircles(lng= ~venues.lon, lat = ~venues.lat)%>%
              addTiles()%>%
              addCircleMarkers( data = df_venues,
                                lat = ~venues.lat,
                                lng= ~venues.lon,
                                radius = 3,
                               color = ~pal(venues.category),
                               popup= ~paste(
                                 "<b>", venues.category, "</b><br/>",
                                 "Nombre: ", as.character(venues.name)),
                                stroke = FALSE,
                                fillOpacity = 0.8 )
    map <- map%>% leaflet::addLegend("topright", pal = pal, values = ~venues.category,
                                                             title = "Categorias",
                                                             opacity = 1)
    map
        
                        
                        
   
      
    })
    
    sliderValues <- reactive({
        
        
    })
        
})
