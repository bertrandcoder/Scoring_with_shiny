
library(tidyverse)
library(corrplot)
library(GGally)
library(ggeffects)
library(MASS)
library(ROCR)
library(effects)
library(shiny)
library(DT)

ui <- fluidPage(
  navlistPanel(
    id="nav","Credit Scoring",
    tabPanel(
      "Modélisation",
      tabsetPanel(
        tabPanel("Data",
                 dataTableOutput(outputId = "table")),
        tabPanel("Modèle"),
        tabPanel("Performances")
      )
    ),
    tabPanel(
      "Prédiction","")
  )
)


server <- function(input, output,session) {
  
  
  cs.training <- read.csv("C:/Users/LENOVO/Desktop/Orleans 2022-2023/R 
                          shiny/DossierRshinyBertrand_KOUOMEGNE/DossierRshinyBertrand_Kouomegne/
                          cs-training.csv", header=T)
  base<-cs.training[,-1]%>%
    mutate(default<-as.factor(SeriousDlqin2yrs))
  names(base)<-c("SeriousDlqin2yrs","soldeCard","age","nPayLate30_59","DebtRatio",
                 "MonthlyIncome","OpenCredit","nPayLate90","Loans","nPayLate60_89",
                 "NumberOfDependents","y")
  base<- na.omit(base[,-1])
  
  
  output$table<-renderDataTable({


  })
}

# Run the application 
shinyApp(ui = ui, server = server)
