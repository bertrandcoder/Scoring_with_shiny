library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(ggeffects)
library(MASS)
library(ROCR)
library(effects)
library(DT)
library(kableExtra)
library(oddsratio)
library(pROC)

choices<-c("age","nPayLate30_59",
  "MonthlyIncome","OpenCredit","nPayLate90","Loans","nPayLate60_89",
  "NumberOfDependents")



ui <- dashboardPage(
  dashboardHeader(title = "Credit scoring"),
  dashboardSidebar(
    hr(),
    sidebarMenu(
      id="tabs",
      menuItem("Prévision",tabName = "prevision",icon = icon("line-chart")),
      menuItem("Modélisation",tabName = "modelisation",icon = icon("table"),
               menuSubItem("Data",tabName = "data",icon = icon("angle-right")),
               menuSubItem("Modèle",tabName = "modele",icon = icon("angle-right")),
               menuSubItem("Performances",tabName = "perf",icon = icon("angle-right")))
    ),
    hr(),
    conditionalPanel("input.tabs=='data'",
                     fluidRow(
                       column(1),
                       column(10,
                              h4("histograms"),
                              hr(),
                              selectInput(inputId = 'var',label = "Choix de la variable",choices = choices ),
                              sliderInput(inputId = 'binwidth',label = "Choix de la taille",value = 1,min = 0,max = 5))
                     )
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "prevision",
              fluidRow(
                valueBoxOutput("classe"),
                
                valueBoxOutput("progressBox"),
                
                valueBoxOutput("prob")
              ),
              fluidRow(
                
                column(4, 
                  textInput(inputId = "prenom",label = "Entrez le prénom du client:",value = "Défaut"),
                  
                  sliderInput(inputId = "age",label = "Age en années:",
                               value = 30,min = 30,max = 150),
                  sliderInput(inputId = "nPayLate30_59",label = "Retard de payement de 30 à 59 jours:",
                               value = 1,min = 0,max = 100),
                  numericInput(inputId = "DebtRatio",label = "Dépenses mensuelles (en pourcentage):",
                               value = 0.5,min = 0,max = 1),
                ),
                
                column(4,
                  numericInput(inputId = "MonthlyIncome",label = "Revenues mensuelles (en euros):",
                               value = 2000,min = 0,max = 40000000),
                  sliderInput(inputId = "OpenCredit",label = "Nombre de prêt en cours:",
                               value = 0,min = 0,max = 60),
                  sliderInput(inputId = "NpayLate90",label = "Retard de payement de plus de 90 jours:",
                               value = 4,min = 0,max = 100)
                  
                ),
                column(4,
                  sliderInput(inputId = "Loans",label = "Nombre de prêts hypothécaires et immobiliers:",
                               value = 7,min = 0,max = 100),
                  sliderInput(inputId = "NpayLate60_89",label = "Retard de payement de 60 à 89 jours:",
                               value = 2,min = 0,max = 100),
                  sliderInput(inputId = "NumberOfDependents",label = "Nombre de personnes à charge dans la famille:",
                               value = 1,min = 0,max = 20)
                )
                
              ),
              fluidPage(
                column(4,
                  actionButton("count", "Simuler",icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                column(8,
                  h5(box(textOutput("resultat")))
                )
                
              )),
      tabItem(tabName = "data",
              fluidPage(
                dataTableOutput(outputId = "table")
              ),
              fluidPage(
                h3("Graphiques"),
                box(
                  title = "Histogram",status = "primary",solidHeader = T,collapsible = T,
                  plotOutput(outputId = "histo")),
                box( title = "boxplot",status = "primary",solidHeader = T,collapsible = T,
                  plotOutput(outputId = "boxplot"))
                       
              ),
              fluidPage(
                h3("Résumé de la base de donnée"),
                column(3,
                       verbatimTextOutput("str")
                ),
                column(3,
                       verbatimTextOutput("summary")
                ),
                column(6,
                  plotOutput(outputId = "corr"))
                
              )
              ),
      tabItem(
        tabName = "modele",
        fluidPage(
          h3("Coefficients estimés"),
          verbatimTextOutput(outputId = "coef")
        ),
        fluidPage(
          h3("ODD Ratio"),
          verbatimTextOutput(outputId = "odd_ratio")
        )
      ),
      tabItem(
        tabName = "perf",
        fluidRow(
            valueBoxOutput("auc"),
            valueBoxOutput("performances"),
            valueBoxOutput("erreur")
        ),
        fluidRow(
          column(3,),
          column(6,
              plotOutput(outputId = "roc")
          ),
          column(3,)
        )
        
      )
    )
    

  )
)

server <- function(input, output) {
  
  cs.training <- read.csv("cs-training.csv", header=T)
  
  base<-cs.training[,-1]%>%
    mutate(default<-as.factor(SeriousDlqin2yrs))
  
  names(base)<-c("SeriousDlqin2yrs","soldeCard","age","nPayLate30_59","DebtRatio",
                 "MonthlyIncome","OpenCredit","nPayLate90","Loans","nPayLate60_89",
                 "NumberOfDependents","y")
  base<- na.omit(base[,-1])
  
  
  output$table<-renderDataTable({
    attach(base)
    
    datatable(head(base),colnames = c("Solde Total des cartes de credit","Age en années",
                                      "Retard de payement de 30 à 59 jours","Dépense mensuelles",
                                      "Revenues mensuelles","Nombres de prêts en cours","Retard de payement de plus de 90 jours",
                                      "Nombre de prêts hypothécaires et immobiliers","Retard de payement de 60 à 89 jours",
                                      "Nombre de personnes à charge dans la famille","Défaut")) 
  })
  
  output$str<-renderPrint({
    str(base)
  })
  
  output$summary<-renderPrint({
    summary(base)
  })
  
  output$histo<-renderPlot({
    
    ggplot(base, aes_string(x = input$var)) +
      geom_histogram(aes(y=..density..),binwidth = input$binwidth,color="black" ,fill = "white") +
      geom_density(alpha=.2, fill="#FF6666")+
      labs(title = paste("Histogramme de la variable", input$var),
           x = input$var, y = "Fréquence")
    
  })
  
  output$boxplot<-renderPlot({
    ggplot(base,aes_string(x=y,y=input$var))+
      geom_violin(aes(fill = y), linewidth = 1, alpha = .5)+
      geom_boxplot(outlier.alpha = 0, coef = 0,
                   color = "steelblue", width = .2)+
      labs(title = paste("boxplot de la variable", input$var),
           x = "Défaut", y = input$var)
      
  })
  
  output$corr<-renderPlot({
    corrplot(cor(base[,-11]), method = "circle", type = "upper", tl.col = "black", tl.srt = 45, 
             tl.cex = 0.8, addCoef.col = "black", addCoef.cex = 0.6)
  })
  
  set.seed(111)
  d = sort(sample(nrow(base), nrow(base) * 0.75))
  # Echantillon d'apprentissage
  train <- base[d,]
  # Echantillon de test
  test <- base[-d,]
  
  log_reg<-glm(y~age+nPayLate30_59+nPayLate60_89+nPayLate90+
                 MonthlyIncome+NumberOfDependents+Loans+DebtRatio+OpenCredit,
               family = binomial(link = logit) ,data = train)
  
  summary(log_reg)
  
  
  
  output$coef<-renderPrint({
    table_reg<-summary(log_reg)$coefficients
    table_reg
  })
  
  output$odd_ratio<-renderPrint({
    exp(coef(log_reg))
  })
  
  
  prob_test<-predict(log_reg,newdata = test,type = "response")
  
  y.predic = rep(1,nrow(test))
  y.predic[prob_test<0.5]<-0
  
  tx_error<-mean(y.predic!=test$y)
  
  
  
  output$roc<-renderPlot({
    
    auc <- round(auc(test$y,prob_test),4)
    roc_curve<-roc(test$y,prob_test, plot = TRUE)
    
    ggroc(roc_curve,colour = 'steelblue', size = 2)+
      geom_abline(intercept = 1, slope = 1, color="black", linetype="dashed", size=1.5)+
      ggtitle(paste0('Courbe de ROC', '(AUC = ', auc, ')'))
    
  })
  
  output$auc<-renderValueBox({
    
    auc <- round(auc(test$y,prob_test),4)
    
    valueBox(
      paste0(auc), "AUC du modèle", icon = icon("thumbs-up"),color = "green"
    )
  })
  
  output$erreur<-renderValueBox({
    valueBox(
      paste0(round( tx_error* 100, 2), "%"), "Taux d'érreur du modèle", icon = icon("info-circle"),color = "green"
    )
  })
  
  output$prob <- renderValueBox({
    
    data_input<-eventReactive(input$count,{
      data.frame(
        age=input$age,
        nPayLate30_59=input$nPayLate30_59,
        DebtRatio= input$DebtRatio,
        MonthlyIncome=input$MonthlyIncome,
        OpenCredit=input$OpenCredit,
        nPayLate90=input$NpayLate90, 
        Loans=input$Loans,
        nPayLate60_89=input$NpayLate60_89,
        NumberOfDependents=input$NumberOfDependents
      )
    },ignoreNULL = F)
    
    
    prob<-predict(log_reg,newdata =data_input(),type = "response")
    
    icon_choice<-ifelse(prob<0.5,"thumbs-up","thumbs-down")
    color_choice<-ifelse(prob<0.5,"green","red")
    
    valueBox(
      paste0(round(prob * 100, 2), "%"), "Probabilité", icon = icon(icon_choice, lib = "glyphicon"),
      color = color_choice
    )
  })
  
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(0 + input$count), "Nombre de Simulations", icon = icon("list"),
      color = "orange"
    )
  })
  
  output$classe <- renderValueBox({
    
    data_input<-eventReactive(input$count,{
      data.frame(
        age=input$age,
        nPayLate30_59=input$nPayLate30_59,
        DebtRatio= input$DebtRatio,
        MonthlyIncome=input$MonthlyIncome,
        OpenCredit=input$OpenCredit,
        nPayLate90=input$NpayLate90, 
        Loans=input$Loans,
        nPayLate60_89=input$NpayLate60_89,
        NumberOfDependents=input$NumberOfDependents
      )
    },ignoreNULL = F)
    
    prob<-predict(log_reg,newdata =data_input(),type = "response")
    
    icon_choice<-ifelse(prob<0.5,"thumbs-up","thumbs-down")
    color_choice<-ifelse(prob<0.5,"green","red")
    
    classe<-ifelse(prob<0.1,"A+++",ifelse(prob<0.3,"A++",ifelse(prob<0.5,"A+",ifelse(prob<0.7,"A-","A--"))))
    
    valueBox(
      paste0(classe), "Classe", icon = icon("info-circle"),color = color_choice
    )
  })
  
  
  
  output$resultat<-renderText({
    
    data_input<-eventReactive(input$count,{
      data.frame(
        age=input$age,
        nPayLate30_59=input$nPayLate30_59,
        DebtRatio= input$DebtRatio,
        MonthlyIncome=input$MonthlyIncome,
        OpenCredit=input$OpenCredit,
        nPayLate90=input$NpayLate90, 
        Loans=input$Loans,
        nPayLate60_89=input$NpayLate60_89,
        NumberOfDependents=input$NumberOfDependents
      )
    },ignoreNULL = F)
    
    prob<-predict(log_reg,newdata =data_input(),type = "response")
    
    score<-eventReactive(input$count,{
      -1.480386-input$age*0.02452173+input$nPayLate30_59*0.5016522+ input$DebtRatio*0.0001849362 - input$MonthlyIncome*0.00004438383 - input$OpenCredit*0.006176554 +input$NpayLate90*0.4404249 + input$Loans*0.08881291 -input$NpayLate60_89*0.9028279 + input$NumberOfDependents*0.09789222
      },ignoreNULL = F)
    
    
    icon_choice<-ifelse(prob<0.5,"thumbs-up","thumbs-down")
    color_choice<-ifelse(prob<0.5,"green","red")
    
    
    ifelse(prob<0.5,paste0("Le score de ",input$prenom ," est de ",round(score(),2)," Vous pouvez lui faire confiance c'est un bon client voire très bon"),paste0("Le score de ",input$prenom ," est de ",round(score(),2)," OUPPP! Attention d'après le modèle ce n'est pas un bon cient"))
    
  })
  
}

shinyApp(ui, server)