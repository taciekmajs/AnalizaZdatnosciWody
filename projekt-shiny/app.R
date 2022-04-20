library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(skimr)
library(forcats)
library(corrplot)
library(caret)
library(randomForest)
library(DT)

df <- read.csv('water_potability.csv')

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  theme = "cerulean",
                  h4("Analiza wody pod wzgledem zdatnosci do picia"),
                  tabPanel(h4("Wstep"),
                           
                           mainPanel(
                             h1("Ogolne przedstawienie problemu oraz zestawu danych"),
                             h4("Jak wiadomo, woda jest niesamowicie wazna w naszym zyciu. Bez niej niemozliwe jest
                                powstanie zycia i to ona je podtrzymuje. Niestety, na swiecie panuje obecnie kryzys
                                wodny, ktory w przyszlosci bedzie sie tylko poglebial. Kraje dotkniete tym kryzysem
                                czesto zmuszone sa do pozyskiwania wody w absurdalnie kosztowne sposoby, takie
                                jak transport jej tankowcami."),
                             h2("Informacje o zbiorze danych"),
                             h4("Zbior danych sklada sie z 10 zmiennych, 9 jest numerycznych a jedna
                                kategoryczna (0-woda niezdatna do picia, lub 1-woda zdatna do picia) 
                                Wziete pod uwage zostalo 3267 pomiarow wody"),
                             
                             h3("Zmienne:"),
                             h4("1: ph Value: Wazny parametr w ocenie rownowagi kwasowo-zasadowej wody"),
                             h4("2: Hardness: Twardosc jest powodowana glownie przez sole wapnia i magnezu"),
                             h4("3: Solids (Total dissolved solids - TDS): (Ciala stale) Woda ma zdolnosc rozpuszczania 
                                szerokiej gamy nieorganicznych i niektorych organicznych mineralow lub soli, 
                                takich jak potas, wapn, sod, wodoroweglany, chlorki, magnez, siarczany itp."),
                             h4("4: Chloramines: Chlor i chloramina to glowne srodki 
                                dezynfekujace stosowane w publicznych systemach wodociagowych."),
                             h4("Sulfate: Siarczany to naturalnie wystepujace substancje, 
                                ktore znajduja sie w mineralach, glebie i skalach."),
                             h4("6: Conductivity: (Przewodnosc) Czysta woda nie jest dobrym przewodnikiem pradu elektrycznego, 
                                a raczej dobrym izolatorem. Wzrost stezenia jonow poprawia przewodnictwo elektryczne wody."),
                             h4("7: Organic_carbon: Calkowity wegiel organiczny (TOC) w wodach zrodlowych pochodzi z 
                                rozkladajacej sie naturalnej materii organicznej (NOM), a takze ze zrodel syntetycznych."),
                             h4("8: Trihalomethanes: THM to substancje chemiczne, ktore mozna znalezc w wodzie uzdatnionej chlorem."),
                             h4("9: Turbidity: (Metnosc) Zmetnienie wody zalezy od ilosci cial stalych obecnych w stanie zawieszonym."),
                             h4("10: Potability: (Zdatnosc do picia) Wskazuje, czy woda jest bezpieczna do spozycia przez ludzi, 
                                gdzie 1 oznacza zdatna do picia, a 0 oznacza niezdatna do picia.")
                             
                             
                             
                             
                             
                           ),
                           
                           
                           sidebarPanel(
                             tags$img(src="szklanka.jpg",width="320px",height="272px")
                             ) 
                          
                           
                           
                           
                  ), 
                  tabPanel(h4("Dane"), 
                           
                           
                           h2("Dane po wypelnienu wartosci brakujacych"),
                           mainPanel(DT::dataTableOutput("dane_surowe"))
                           
                           
                           
                           
                           
                           
                           ), #Dane
                  
                  tabPanel(h4("Analiza"),
                           
                           
                           
                           column(
                                    
                             h2("Wielowymiarowy wykres gestosci"),
                             plotOutput("gestosci"),
                             br(),
                             br(),
                             br(),
                             br(),
                             h2("Wielowymiarowy wykres pudelkowy"),
                             plotOutput("pudelkowy"),
                             br(),
                             br(),
                             br(),
                             br(),
                             width = 5
                             ),
                           column(h2("Wykres korelacji"),
                                        plotOutput("korelacji"),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  h2("Wykres dla wybranej zmiennej"),
                                  
                                  
                                    selectInput(
                                      "zmienna", label = "Zmienna",
                                      choices = tolower(colnames(df)), multiple = F
                                    ),
                                    plotOutput("dlazmiennej"),
                                 
                                  
                                  width =4)
                          
                           
                           
                           
                           ), #Analiza
                  
                  tabPanel(h4("Prognozowanie"),
                           
                           sidebarPanel(
                             h2("Prognozowanie"),
                             h4("Prognozowanie rozpoczniemy od podzielenia naszych danych na czesc
                                uczaca i testowa w stosunku 4:1"),
                             h4("Oto wybrane informacje dotyczace zmiennych czesci uczacej:"),
                             br(),br(),br(),br(),br(),br(),
                             h4("Nastepnie stworzymy modele 5 roznymi metodami oraz sprawdzimy, 
                                ktora z nich jest najlepsza:"),
                             br(),br(),br(),br(),
                             h4("Poniewaz srednia dokladnosc jest wysoka, wybierzemy model Random Forest,
                                dokonamy przewidywania, oraz wyznaczymy ktore cechy wody sa najwazniejsze"),
                            
                             
                             
                           ), #Prognozowanie opisy
                           
                           
                           mainPanel(
                             verbatimTextOutput("summary1"),
                             verbatimTextOutput("summary2"),
                             verbatimTextOutput("summary3"),
                             plotOutput("summary4"),
                             
                             
                             
                             
                           ) #Prognozowanie wyswietlanie
                           
                           
                           
                           ) #Prognozowanie
                  
                  
                  
                ) # navbarPage
) # fluidPage


server <- function(input, output) {
  
  df <- df %>% mutate(Potability = as.factor(Potability))   #wczytywanie danych
  colnames(df) <- tolower(colnames(df))
  
  for(i in 1:9){
    df[is.na(df[,i]),i] <- mean(df[, i], na.rm = T) #usuwanie brakujacych wartosci
  }
  
  
  output$dane_surowe <- DT::renderDataTable(       #tabela Dane
    DT::datatable({
      df
    },
    options = list(lengthMenu=list(c(10,15,20),c('10','15','20')),pageLength=20,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'color': 'ff0000'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = "multiple",
    style = "bootstrap",
    class = "cell-border stripe",
    rownames = FALSE,
    colnames = c("ph","hardness","solids","chloramines","sulfate","conductivity","organic_carbon","trihalomethanes","turbidity","potality")
    ))
   
   scales <- list(x=list(relation="free"), y=list(relation="free"))
   output$gestosci = renderPlot({featurePlot(x=df[,1:9],y=df[,10], plot = "density", scales = scales)}, height = 500, width = 500)   
   
   scales <- list(x=list(relation="free"), y=list(relation="free"))
   output$pudelkowy = renderPlot({featurePlot(x=df[,1:9],y=df[,10], plot = "box", scales = scales)}, height = 500, width = 500)

   output$korelacji <- renderPlot({corrplot(
     cor(df[, -10]),
     type = "lower",
     method = "circle",
     number.cex = .9,
     order = "alphabet",
     tl.col = "#00796B",
     tl.srt = 25,
     title = "\nKorelacja pomiedzy zmiennymi"
   )}, height = 500, width = 500)    
   
   output$dlazmiennej <- renderPlot({
     ggplot(df, aes(x = df[,input$zmienna], fill = potability)) +
       geom_density(alpha=0.6)
   }, height = 400, width = 400)
   
   train_index <- createDataPartition(df$potability, p = 0.8, list = F)
   test_set <- df[-train_index,]
   training_set <- df[train_index,]
   output$summary1 <- renderPrint({summary(training_set)})
   
   
   control <- trainControl(method = "cv", number = 10)
   metric = "Accuracy"
   
   #Linear Discriminant Analysis (LDA)
   set.seed(7)
   fit.lda <- train(potability ~.,data=training_set,method="lda",metric=metric,trControl=control)
   #Classification and Regression Trees (CART).
   set.seed(7)
   fit.cart <- train(potability ~.,data=training_set,method="rpart",metric=metric,trControl=control)
   #k-Nearest Neighbors (kNN).
   set.seed(7)
   fit.knn <- train(potability ~.,data=training_set,method="knn",metric=metric,trControl=control)
   #Support Vector Machines (SVM) with a linear kernel.
   set.seed(7)
   fit.svm <- train(potability ~.,data=training_set,method="svmRadial",metric=metric,trControl=control)
   #Random Forest (RF)
   set.seed(7)
   fit.rf <- train(potability ~.,data=training_set,method="rf",metric=metric,trControl=control)
   
   results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
   
   output$summary2 <- renderPrint({summary(results)})
   
   predictions <- predict(fit.rf, test_set)
   confusionMatrix(predictions, test_set$potability)
   output$summary3 <- renderPrint({confusionMatrix(predictions, test_set$potability)})
   
   
   vars.imp <- varImp(fit.rf, scale = F)
   output$summary4 <- renderPlot({ggplot(vars.imp) +
     geom_col(fill = "dark green", size = 1) +
     labs(title = "Variables importance")}, width = 500, height = 500)
   
} 

shinyApp(ui = ui, server = server)