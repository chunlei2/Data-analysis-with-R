## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "STAT 425 Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Statistics", tabName = "stat", icon = icon("table")),
      menuItem("Analysis", tabName = "ana", icon = icon("bar-chart-o")),
      menuItem("Maps", tabName = "maps", icon = icon("map-o")),
      menuItem("Prediction", tabName = "pred", icon = icon("sitemap"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "data",
              #tabItem("rawdata",
                      #numericInput("maxrows", "Rows to show", 25),
                      #tableOutput("rawtable"),
              box(title = "Ames Housing Price Data", status = "primary",
                  dataTableOutput('rawtable'),
                  downloadButton("downloadCsv", "Download as CSV"),
                  width = 23)       
              
              #)
      ),
      
      # Second tab content
      tabItem(tabName = "stat",
      fluidRow(
      tabBox(
        title = "Neighborhood",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "600px",
        tabPanel('By Average Price',
                 br(),
                 infoBoxOutput("box1",width = 4),
                 infoBoxOutput("box2",width = 4),
                 infoBoxOutput("box3",width = 4),
                 plotOutput("plottab1")
                 ),
        tabPanel('By Overall Condition',
                 br(),
                 infoBoxOutput("box6",width = 4),
                 infoBoxOutput("box7",width = 4),
                 infoBoxOutput("box8",width = 4),
                 plotOutput("plottab2")
                 ),
        tabPanel('By Deal',
                 br(),
                 infoBoxOutput("box11",width = 4),
                 infoBoxOutput("box12",width = 4),
                 infoBoxOutput("box13",width = 4),
                 plotOutput("plottab3")
                 ),
        width = 16
      ),
      
      dataTableOutput('table')
      )
      ),
      # Third tab content
      tabItem(tabName = "ana",
              fluidRow(
                box(selectInput("input1", "Neighborhood1", 
                                choices = c("Blmngtn","Blueste","BrDale","BrkSide","ClearCr",
                                            "CollgCr","Crawfor","Edwards","Gilbert","IDOTRR",
                                            "MeadowV","Mitchel","NAmes","NoRidge","NPkVill",
                                            "NridgHt","NWAmes","OldTown","Sawyer","SawyerW",
                                            "Somerst","StoneBr","SWISU","Timber","Veenker"),selected = 'NoRidge' ),
                    selectInput("input2", "Neighborhood2", 
                                choices = c("Blmngtn","Blueste","BrDale","BrkSide","ClearCr",
                                            "CollgCr","Crawfor","Edwards","Gilbert","IDOTRR",
                                            "MeadowV","Mitchel","NAmes","NoRidge","NPkVill",
                                            "NridgHt","NWAmes","OldTown","Sawyer","SawyerW",
                                            "Somerst","StoneBr","SWISU","Timber","Veenker"),selected = 'Somerst'),width = 3),
                box(plotOutput("plot1", height = 250),width = 9)
              ),
                fluidRow(
                  box(selectInput("input3", "X axis", 
                                  choices = c('MSZoning','Neighborhood','OverallCond','TotalBsmtSF','YearBuilt',
                                              'GrLivArea', 'GarageCars','OverallQual','SalePrice'),selected = 'GrLivArea' ),
                      selectInput("input4", "Y axis", 
                                  choices = c('MSZoning','Neighborhood','OverallCond','TotalBsmtSF','YearBuilt',
                                              'GrLivArea', 'GarageCars','OverallQual','SalePrice'),selected = 'OverallCond'),
                      selectInput("input5", "Level", 
                                  choices = c('MSZoning','Neighborhood','OverallCond',
                                               'GarageCars','OverallQual'),selected = 'GarageCars'),width = 3), 
                  box(plotOutput("plot2", height = 250),width = 9)
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "maps",
              
              sliderInput(inputId = 'range',label = 'sale price range',min=1,max=800000,value=c(2000,700000)),
              box(title = 'Ames Map',status = "primary",
                  plotOutput(outputId = 'map'))
      ),
      
      # Fifth tab content
      tabItem(tabName = "pred",
              box(title = "Inputs-1", status = "primary",
                "Please select the eight following attributes in both parts before pushing the button. The result will be shown once the prediction finished", 
                selectInput("MSZoning", "1. MS Zoning",
                            choices = c(
                              "A (agr)",
                              "C (all)",
                              "FV",
                              "I (all)",
                              "RH",
                              "RL",
                              "RM"
                            ),
                            selected = "A(agr)"
                ),
                selectInput("Neighborhood", "2. Neighborhood",
                            choices = c(
                              "Blmngtn",
                              "Blueste",
                              "BrDale",
                              "BrkSide",
                              "ClearCr",
                              "CollgCr",
                              "Crawfor",
                              "Edwards",
                              "Gilbert",
                              "Greens",
                              "GrnHill",
                              "IDOTRR",
                              "Landmrk",
                              "MeadowV",
                              "Mitchel",
                              "NAmes",
                              "NoRidge",
                              "NPkVill",
                              "NridgHt",
                              "NWAmes",
                              "OldTown",
                              "Sawyer",
                              "SawyerW",
                              "Somerst",
                              "StoneBr",
                              "SWISU",
                              "Timber",
                              "Veenker"
                            ),
                            selected = "Blmngtn"
                ),
                sliderInput("OverallCond", "3. Overall Cond:", 1, 9, 5),
                sliderInput("OverallQual", "4. Overall Qual:", 1, 10, 5)
                
               
                
              ),
      
              box(
                title = "Inputs-2", status = "primary",
                "Notice: Please press the button below when you finish all the inputs.",
                numericInput("TotalBsmtSF", "5. Total Bsmt SF: (Please input an integer)", 1050),#1050 is the average of this col
                sliderInput("YearBuilt", "6. Year Built:", 1872, 2010, 1970),
                sliderInput("GrLivArea", "7. Gr Liv Area:", 334, 5642, 1500),
                selectInput("GarageCars", "8. Garage Cars:",
                            choices = c(
                              "0" = 0, 
                              "1" = 1,
                              "2" = 2,
                              "3" = 3,
                              "4" = 4,
                              "5" = 5
                            ),
                            selected = "0"
                ),
                actionButton("Button", "Get Prediction" )
              ),
              
                
                infoBoxOutput("predict_value")

                
              
        )
    )
  )
)

server <- function(input, output) {
  

  library(caret)
  library(randomForest)
  library(AmesHousing)
  library(rpart)
  
  variable_list = c('MS Zoning',
                    'Neighborhood',
                    'Overall Cond',
                    'Total Bsmt SF',
                    'Year Built',
                    'Gr Liv Area',
                    'Garage Cars',
                    'Overall Qual',
                    'SalePrice')
  variable = ames_raw[,variable_list]
  name_list = c('MSZoning',
                'Neighborhood',
                'OverallCond',
                'TotalBsmtSF',
                'YearBuilt',
                'GrLivArea',
                'GarageCars',
                'OverallQual',
                'SalePrice')
  colnames(variable) = name_list
  
  
  #check the na values
  #apply(variable_df,2,function(x) {sum(is.na(x))})
  
  #Since there are only two na samples, delete all Na
  variable_df = na.omit(variable)
  variable_df['Average_price'] = variable_df$SalePrice/variable_df$GrLivArea
  #variable_df = read.csv("variable_df.csv")
  variable_df$MSZoning = as.factor(variable_df$MSZoning)
  variable_df$Neighborhood = as.factor(variable_df$Neighborhood)
  variable_df$GarageCars = as.factor(variable_df$GarageCars)
  variable_df$TotalBsmtSF = as.numeric(variable_df$TotalBsmtSF)
  variable_df['Average_price'] = variable_df$SalePrice/variable_df$GrLivArea
  library(dplyr)
  group = group_by(variable_df, variable_df$Neighborhood)
  group_mat = summarise(group,  
                     count = n(),                #个数  
                     max_yr = max(`YearBuilt`),       #最大值  
                     min_yr = min(`YearBuilt`),       #最小值
                     avg_price = round(mean(SalePrice /`GrLivArea`),2),
                     avg_cond = mean(`OverallCond`))

  group_mat_price = arrange(group_mat,desc(avg_price))
  colnames(group_mat_price) = c('Neighborhood', 'Amount of Sold', 'The Newest Built Year', 'The Oldest Built Year',
                          'Sales Price Per SF', 'Average Overall Condition')
  group_mat_cond = arrange(group_mat,desc(avg_cond))
  colnames(group_mat_cond) = c('Neighborhood', 'Amount of Sold', 'The Newest Built Year', 'The Oldest Built Year',
                                'Sales Price Per SF', 'Average Overall Condition')
  group_mat_vol = arrange(group_mat,desc(count))
  colnames(group_mat_vol) = c('Neighborhood', 'Amount of Sold', 'The Newest Built Year', 'The Oldest Built Year',
                                'Sales Price Per SF', 'Average Overall Condition')
  
  lang = function(i,data){
    paste1 = paste("Average Price: ", data[i,5],sep = " ")
    paste2 = paste("Average Overall Condition: ", data[i,6],sep = " ")
    paste3 = paste("Amount of Sold: ", data[i,2],sep = " ")
    paste4 = paste("The Newest House built in: ", data[i,3], sep = " ")
    paste5 = paste("The Oldest House built in: ", data[i,4], sep = " ")
    cat(paste1,"\n",paste2,"\n",paste3,"\n",paste4,"\n",paste5)
  }
  # paste1 = paste("Average Price: ", group_mat_price[1,5],sep = " ")
  # paste2 = paste("Average Overall Condition: ", group_mat_price[1,6],sep = " ")
  # paste3 = paste("Amount of Sold: ", group_mat_price[1,2],sep = " ")
  # paste4 = paste("The Newest House built in: ", group_mat_price[1,3], sep = " ")
  # paste5 = paste("The Oldest House built in: ", group_mat_price[1,4], sep = " ")
  # aoao = cat(paste1,"\n",paste2,"\n",paste3,"\n",paste4,"\n",paste5)
  # output$predict_value <- renderValueBox({
  #   valueBox(
  #     value = input$maxrows,
  #     subtitle = "Total downloads"
  #   )
  #   
  #   })
  # output$box = renderInfoBox({
  #   infoBox("TRIAL", icon = icon("list"),
  #   color = "purple")
  # })
  output$box1 <- renderInfoBox({
    infoBox("Top 1",group_mat_price[1,1])
  })
  # output$text1 <- renderInfo({
  # 
  #   value = 
  #   
  # })
  output$box2 <- renderValueBox({
    infoBox("Top 2", group_mat_price[2,1])
  })
  output$box3 <- renderInfoBox({
    infoBox("Top 3", group_mat_price[3,1])
  })
  output$plottab1 <- renderPlot({
    Sale_MSSubClass = ggplot(variable_df, aes(x = Neighborhood, 
                                           y = SalePrice)) +
      geom_boxplot(fill = '#A8CD1B', colour = '#585858') + theme_bw() +  
      scale_x_discrete(name = "Neighborhood") +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(name = "Sales Price Per SF", breaks = seq(0, 900000, 50000))
    ggtitle("Boxplot of SalePrice by Neighborhood") 
    Sale_MSSubClass
  })
  # output$box4 <- renderInfoBox({
  #   infoBox("Top 4", group_mat_price[4,1])
  # })
  # output$box5 <- renderInfoBox({
  #   infoBox("Top 5", group_mat_price[5,1])
  # })
  output$box6 <- renderInfoBox({
    infoBox("Top 1", group_mat_cond[1,1])
  })
  output$box7 <- renderInfoBox({
    infoBox("Top 2", group_mat_cond[2,1])
  })
  output$box8 <- renderInfoBox({
    infoBox("Top 3", group_mat_cond[3,1])
  })
  # output$box9 <- renderInfoBox({
  #   infoBox("Top 4", group_mat_cond[4,1])
  # })
  # output$box10 <- renderInfoBox({
  #   infoBox("Top 5", group_mat_cond[5,1])
  # # })
  output$plottab2 <- renderPlot({
    # Sale_MSSubClass = ggplot(variable_df, aes(x = Neighborhood,
    #                                           y = OverallCond)) +
    #   geom_boxplot(fill = '#A8CD1B', colour = '#585858') + theme_bw() +
    #   scale_x_discrete(name = "Neighborhood") +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #   scale_y_continuous(name = "Overall Condition", breaks = seq(0, 900000, 50000))
    # ggtitle("Boxplot of Overall Condition by Neighborhood")
    # Sale_MSSubClass
    
    OverallCond_Neiborhood = ggplot(variable_df, aes(x = Neighborhood, y = OverallCond)) +
      geom_jitter(fill = '#A8CD1B', aes(colour = Neighborhood),show.legend=F, size=0.4) +
      theme_bw() +  
      scale_y_continuous(name = "Overall Condition", breaks = seq(0, 11, 1)) + 
      ggtitle("Boxplot of Overall Condition by Neighborhood")+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
    OverallCond_Neiborhood
  })
  output$box11 <- renderInfoBox({
    infoBox("Top 1", group_mat_vol[1,1])
  })
  output$box12 <- renderInfoBox({
    infoBox("Top 2", group_mat_vol[2,1])
  })
  output$box13 <- renderInfoBox({
    infoBox("Top 3", group_mat_vol[3,1])
  })
  # output$box14 <- renderInfoBox({
  #   infoBox("Top 4", group_mat_vol[4,1])
  # })
  # output$box15 <- renderInfoBox({
  #   infoBox("Top 5", group_mat_vol[5,1])
  # })
  output$plottab3 <- renderPlot({
    Sale_MSSubClass = ggplot(variable_df, aes(Neighborhood)) + geom_bar() +
      theme(axis.text.x = element_text(angle = 45))+
      theme_bw()
    Sale_MSSubClass
    
  })
  output$table = renderDataTable({
    if(input$tabset1 == 'By Average Price'){
    as.data.frame(head(group_mat_price,15))}
    else if(input$tabset1 == 'By Overall Condition'){
      as.data.frame(head(group_mat_cond,25))
    }
    else{
      as.data.frame(head(group_mat_vol,25))
    }
  })
  output$predict_value <- renderValueBox({
    if(input$Button == 0){
    infoBox(
      "Prediction", icon = icon("list"),
      color = "purple",
      '0'
    )}
    else{

      #Random Forest Model
      #variable_df = read.csv("variable_df.csv")
      set.seed(1)
      rf_mod = randomForest(formula = SalePrice ~ . ,
                            #method = "anova",
                            data = variable_df,
                            ntree = 500,
                            mtry = 3,
                            importance = T)
      data_tst_temp = variable_df[1,1:8]
      data_tst_temp[1,] = c(input$MSZoning,input$Neighborhood, as.character(input$OverallCond), input$TotalBsmtSF,
                            input$YearBuilt, input$GrLivArea, input$GarageCars, as.character(input$OverallQual))
      
      pred = predict(rf_mod, data = data_tst_temp[1,])
      infoBox(
        "Prediction", icon = icon("list"),
        color = "purple",
        value = round(pred,1)
      )}
    }
  )

  output$plot1 <- renderPlot({

    library(ggplot2)
    library(AmesHousing)
    ames_raw_subset = subset(ames_raw, Neighborhood %in% c(input$input1, input$input2) ,
                             select = c('Yr Sold', 'SalePrice', 'Neighborhood'))
    ggplot(ames_raw_subset, aes(x=`Yr Sold`, y=SalePrice, colour =Neighborhood )) + geom_point() + 
      stat_summary(fun.y = mean, geom = "line", size=1, aes(colour=Neighborhood))+
      theme_bw() +
      scale_y_continuous(name = "Sale Price", breaks = seq(0, 900000, 50000)) +
      xlab("Year") + 
      scale_fill_brewer(palette="Set2")
  })
    
  output$plot2 <- renderPlot({
     ggplot(aes_string(x = input$input3, y=input$input4), data=variable_df) + geom_point() + 
       facet_wrap(reformulate(input$input5))

    
  })
  #tab 3 map
  output$map<-renderPlot({
    library(ggmap)
    library(AmesHousing)
    ames_total<-merge(ames_geo,ames_raw,by='PID')
    ames_total_subset<-subset(ames_total,input$range[1] < 
                                ames_total$SalePrice&ames_total$SalePrice<input$range[2])
    ames.ggmap<-get_map(location='Ames',source='google',zoom=13,maptype='roadmap')
    g<-ggmap(ames.ggmap)
    g <- g + geom_point(data=ames_total_subset, aes(x=Longitude, y=Latitude), inherit.aes=FALSE,
                        color='dodgerblue', alpha=0.2)
    g + ggtitle('House Price in Ames')
  })
  #tab 1 csv download

  
  #tab 1 raw data table
  # output$rawtable <- renderTable({
  #   library("xtable")
  #   ames_subset = ames_raw[1:input$maxrows,1:10]
  #   xtable(ames_subset)
  # }, digits = 1)

  output$rawtable <- renderDataTable({
    library(AmesHousing)
    #column <- c('MS Zoning', 'Neighborhood', 'Overall Cond', 'Total Bsmt SF',
    #           'Overall Qual', 'Year Built', 'Gr Liv Area', 'Garage Cars','SalePrice')
    #ames_subset = ames_raw[,column]
    as.data.frame(variable_df[,1:9])
              })
  
  output$downloadCsv <- downloadHandler(
    filename = "ames_raw.csv",
    content = function(file) {
      write.csv(ames_subset, file)
    },
    contentType = "text/csv"
  )

  
  
}

shinyApp(ui, server)
