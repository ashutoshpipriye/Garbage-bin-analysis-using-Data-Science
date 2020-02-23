Data <- read.csv("C:/Users/Ashu/Documents/Newdata.csv")
Waste <- read.csv("C:/Users/Ashu/Documents/Waste_Management.csv")
library(shiny)
library(datasets)
library(ggplot2)
library(reshape)

shinyServer(function(input, output) {
  
  # Return the requesteed dataset
  datasetInput <- reactive({
    switch (input$dataset,
            'Bin_Data' = Data,
            'Waste_Management' = Waste
    )
  })
  
  xcol <- reactive({
    df <- datasetInput()
    y <- input$resp
    df <- df[!is.na(df[,y]),]
    y <- df[,input$resp]
    
    if(input$pred_type == F){
      x <- df[,!(names(df) %in% input$resp)]
      p <- ncol(x)
      pvalues <- numeric(p)
      for(i in seq_len(p)) {
        fit <- lm(y ~ x[, i])
        summ <- summary(fit)
        pvalues[i] <- summ$coefficients[2, 4]
      }
      ord <- order(pvalues)
      ord <- head(ord,1)
      xf <- as.data.frame(x[,ord])
      names(xf) <- names(x[ord])
    } else {
      xcol <- input$predx
      xf <- as.data.frame(df[,xcol])
      names(xf) <- xcol
    }
    xf
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })    
  
  # Column names of the selected dataset
  # output$colnames <- renderPrint({
  #     dataset <- datasetInput()
  #     colnames <- names(dataset)
  #     colnames
  # })
  # 
  output$response <- renderUI({
    resp <- names(datasetInput())[!sapply(diamonds, is.factor)]
    selectInput("resp", "Choose Response Varaible", resp)
  })
  
  output$predictors <- renderUI({
    predx <- setdiff(names(datasetInput()), input$resp)
    checkboxGroupInput("predx", "Choose Predictor Varaible(s)", predx)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$model <- renderPrint({
    df <- datasetInput()
    i <- input$resp
    y <- df[!is.na(df[,i]),i]
    cat('Linear regression is performed on the formula:',input$resp,'~',
        paste(names(xcol()),collapse='+') )
    fit <- lm(y~., data=xcol())
    summary(fit)
    
  })
  
  output$plt <- renderPlot({
    df <- datasetInput()
    i <- input$resp
    y <- df[!is.na(df[,i]),i]
    x <- cbind(xcol(), y=y)
    x.m <- melt(x, 'y')
    tit <- paste('Simple Linear Regression:',input$resp , ' vs other predictors')
    g <- ggplot(x.m, aes(x=y, y=value, color=variable)) + geom_point()
    g <- g + geom_smooth(method = 'lm') 
    g <- g + facet_wrap(~variable, scales = 'free_y')
    g <- g + ylab('') + ggtitle(tit)
    g
    
  })
  
  
})