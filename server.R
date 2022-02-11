library(tidyverse)
library(shiny)
library(shinythemes)
library(rsconnect)

pc <- read.csv("pc.csv")
int <- read.csv("int.csv")
tic <- read.csv("tic.csv")

server <- function(input, output) {
  
  output$pc_bars <-renderPlot({
    
    sub_pc <- pc %>%
      filter(Region == input$pc_region & sector_esc == input$pc_gestion & nivel_en_curso == input$pc_nivel)
    
    ggplot(data=sub_pc) +
      geom_bar(mapping = aes(x = acceso_comp, y = porcentaje, fill=porcentaje), 
               stat = "identity")+
      labs(x = "Acceso a computadora", 
           y = "Porcentaje")+
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(aes(x = acceso_comp, y = porcentaje, label = porcentaje), 
                colour = "black",
                position=position_dodge(width=0.9), 
                vjust=-0.25)
  })
  
  output$int_bars <-renderPlot({
    
    sub_int <- int %>%
      filter(Region == input$int_region & sector_esc == input$int_gestion & nivel_en_curso == input$int_nivel)
    
    ggplot(data=sub_int) +
      geom_bar(mapping = aes(x = tipo_int, y = porcentaje, fill=porcentaje), 
               stat = "identity")+
      labs(x = "Acceso a internet", 
           y = "Porcentaje",
           caption = "Referencias: 
                      1: Acceso fijo con buena calidad de señal.
                      2: Acceso fijo con problemas de señal.
                      3: Acceso sólo con datos de celular.
                      4: Sin acceso a Internet.")+
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(aes(x = tipo_int, y = porcentaje, label = porcentaje), 
                colour = "black",
                position=position_dodge(width=0.9), 
                vjust=-0.25)
  }) 
  
  output$tic_bars <-renderPlot({
    
    sub_tic <- tic %>%
      filter(Region == input$tic_region & sector_esc == input$tic_gestion & nivel_en_curso == input$tic_nivel)
    
    ggplot(data=sub_tic) +
      geom_bar(mapping = aes(x = acceso_TIC, y = porcentaje, fill=porcentaje), 
               stat = "identity")+
      labs(x = "Acceso a computadora e internet", 
           y = "Porcentaje",
           caption = "Referencias: 
                      1: Con PC y acceso fijo a internet.
                      2: Con PC y acceso a internet con datos móviles.
                      3: Sin PC, con acceso fijo a internet.
                      4: Sin PC, con acceso a internet con datos móviles o sin acceso internet.")+
      theme_minimal() +
      theme(legend.position = "none") +
      geom_text(aes(x = acceso_TIC, y = porcentaje, label = porcentaje), 
                colour = "black",
                position=position_dodge(width=0.9), 
                vjust=-0.25)
  })
  
  
  
  
}
