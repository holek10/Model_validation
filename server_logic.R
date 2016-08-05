
# -----------------------------------------------------  
# -- load a dataset and perform checks (if necessary columns are present)
# ----------------------------------------------------- 
  user_dataset_load <- reactive( {   
    
    userData <-NULL
    
    if (!is.null(input$file)) {                     # User has not uploaded a file yet
      userData <- readRDS(input$file$datapath)
      #return(NULL)
    }
    if( input$wd_file_load != 0) {
      
      input$wd_file_load
      isolate(
        if (input$dir_file != "none") {
          dataset_path <- isolate(input$dir_file)
          userData <- readRDS(dataset_path) } else {userData<-NULL}
      )
    }
  #  if (input$dataset_load_choice == "1" & !is.null(input$file)) {                     # User has not uploaded a file yet
  #    userData <- readRDS(input$file$datapath)    
  #  } else { 
    #  if(input$dataset_load_choice == "2")
    #    input$wd_file_load
    #    isolate(
    #      if (input$dir_file != "none") {
    #        dataset_path <- isolate(input$dir_file)
    #        userData <- readRDS(dataset_path) } else {userData<-NULL}
    #    )
      
    #}
    #userData <- readRDS(paste(getwd(),"/validation_sample_data.rds",sep=""))          # temp file load
    return(userData)
  })
  

  # check if correct dataset was uploaded (return NULL otherwise) 
  user_dataset <- reactive( {         
    userData <- NULL 
    columns <- c("id", "default", "record_date", "score", "rating", "validation", "pd")    # necessary columns
    if (FALSE %in% (columns %in% colnames( user_dataset_load() ) ) ) {
      userData <- NULL
    } else { 
      userData <- user_dataset_load()
    }
    return(userData)
  })
  
  
  # information whether variable is of type numeric 
  output$numeric_variable <- renderUI({
    if( !is.null(user_dataset())  ) {
      if (  !input$variable %in% names(user_dataset()) ) {
        HTML("<input id='is_numeric' value='0'  type='text' style='display:none'> ") 
      } else { 
        if ( class(user_dataset()[,input$variable]) %in% c("integer", "numeric") ) {
          HTML("<input id='is_numeric' value='1'  type='text' style='display:none'> ") 
          
        } else { 
          HTML("<input id='is_numeric' value='0'  type='text' style='display:none'> ") 
        }
      }       
    } else { HTML("<input id='is_numeric' value='0'  type='text' style='display:none'> ")   }
  })
  outputOptions(output, 'numeric_variable', suspendWhenHidden=FALSE)
  
  # graphic feedback for user whether proper dataset was loaded or not
  output$user_dataset <- renderPrint( {   
    columns <- c("id", "default", "record_date", "score", "rating", "validation", "pd")   # necessary columns 
    # colnames(user_dataset() )
    if (!is.null(user_dataset_load())) 
    {
      if (FALSE %in% (columns %in% colnames(user_dataset_load()) ) )  
      {    
        error <- tags$div(class="alert alert-error",  
                          HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                          p(strong('Oh no!'), "Non-compliant dataset.
                            Please make sure data has required pre-defined structure and try again."))                    
        cat(as.character(error))   
      } else {
        success <- tags$div(class="alert alert-success",  
                            HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                            p(strong("Well done!"), "You have successfully loaded the dataset.
                              Go to  'Basic characteristics' tab to see basic datasets properties. Continue with next tabs."))   
        cat(as.character(success))            
      }
    } else {
      info <- tags$div(class="alert alert-info",  
                       HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                       p(strong("Let's start!"), "Upload a dataset using one of the methods below.
                         Once data is uploaded go to 'Basic characteristics' tab to see basic properties for development and validation sample. 
                         'Time dimension' shows evloution in time, 'Statistical tests' provide selected quantitative tests, 
                         'Individual variables' show basic information for each model variable, 
                         and 'Data explorer' shows source data in interactive mode."))   
      cat(as.character(info))   
    }  
    
  })

  # show left-hand side menu if dataset loaded properly
  output$data_loaded <- renderUI( {          
    
    if(is.null(user_dataset())) {  
      #paste("false")
      HTML("<input id='dataset_loaded' value='0'  type='text' style='display:none'> ") 
    } else { 
      #paste("true")
      HTML("<input id='dataset_loaded' value='1'  type='text' style='display:none'> ")
    }
    
  })

# ----------------------------------------------------- 
# --- auxilary functions to see output on screen
# -----------------------------------------------------
  output$test  <-  renderPrint( {
    
  #  str(PSI()$psi_table)
   # validation_sample()$rating
    
  })
  
  output$test2  <-  renderPrint( {
    
  #  as.matrix(PSI()$psi_table)
    #validation_sample()$rating
    rating_val <- validation_sample()$rating
    current <- as.numeric(table(rating_val)/length(rating_val)) 
    H_index <- sum(current^2)
    H_norm <- (H_index - 1/length(current)) / (1 - 1/length(current))
    list(H_index = H_index , H_norm = H_norm)
    
  })
  

  
# -----------------------------------------------------
# -- Parameters (left panel) & data
# -----------------------------------------------------

  output$variable_choice <- renderUI({
   # if( !is.null(user_dataset() ) ) {
      dta <- user_dataset()
      exclude <- c("id", "default", "record_date", "score", "rating", "validation", "pd")
      include <- names(dta)[!names(dta) %in% exclude] 
      selectInput(inputId = "variable", label="Select a variable:", choices=c("no variable chosen", include), selected = "no variable chosen", multiple = FALSE,  selectize=FALSE)
  #  } 
  })
  outputOptions(output, 'variable_choice', suspendWhenHidden=FALSE)
  
  output$choose_default <- renderUI({
    if( !is.null(user_dataset() ) ) {
      dta <- user_dataset()
      default <- sort(names(dta)[grep("def", names(dta))])
      selectInput(inputId = "default_variable", label="", choices=default, selected = NULL, multiple = FALSE,  selectize=FALSE) 
    }
  })
  
  
  development_sample <- reactive({    
    if( !is.null(user_dataset() ) ) {
      dta <- user_dataset()
      dta <- dta[dta$validation==0,]
      score <- dta$score
      default <- dta[,input$default_variable]
      rating <- dta$rating   
      output <- list(dataset = dta, score = score, default = default, rating = rating)
      return(output)   
    }
  })
  
  validation_sample <- reactive({
    if( !is.null(user_dataset() ) ) {
      dta <- user_dataset()
      dta <- dta[dta$validation==1,]
      score <- dta$score
      default <- dta[,input$default_variable]
      rating <- dta$rating   
      output <- list(dataset = dta, score = score, default = default, rating = rating)
      return(output)   
    }
  })

  tableColor <- function(data, bold.last = F, font.col ="white", header.col=EYcolors[3], row.col=EYcolors[6], font="sans-serif", ...) {
    if (is.null(data))
      return("")
    add.to.row= NULL
    if( nrow(data) > 1 ) {
      temp = as.list(c(seq(1,nrow(data)-1,by=2), nrow(data)-1)) 
      add.to.row=list(temp, c(rep("XXX", length(temp)-1), "LAST"))
    } 
    temp = renderTable(data, add.to.row=add.to.row, ...)
    #temp = renderTable(data, add.to.row=add.to.row)
    temp = temp()  
    temp = gsub("align=\"right\"", "", temp)    # remove aligment which doesn't work with HTML 5 
    if( bold.last ) 
      temp = gsub(" LAST<TR>", paste("<TR style=\"font-weight:bold\">")  , temp)  
    temp = gsub("LAST", "", temp)
    if(!is.na(row.col) && !is.null(row.col))
      temp = gsub("XXX<TR", paste("<TR bgcolor=", row.col, ""), temp)
    temp = gsub("XXX", "", temp)
    temp = gsub("LAST", "", temp)
    if(!is.na(header.col) && !is.null(header.col)) 
      temp = gsub("<TR>\\s*<TH>\\s*</TH>",paste("<TR style=\"color:", font.col, "\" bgcolor=", header.col, "><TH></TH>"), temp)
    temp = gsub("<TH>", paste("<TH style=\"text-align:center\"> "), temp)
    if(!is.na(font) && !is.null(font))
      temp = gsub("<TD", paste("<TD style=\"font-family:",font,"; text-align:center\"", sep=""), temp)
    return(temp)
  }
# -----------------------------------------------------
# -- Basic characteristics
# -----------------------------------------------------
    
  calculate_ROC_dev <- reactive({   
    source("functions_Models.r")
    score <- development_sample()$score
    default <- development_sample()$default
    rr <- ROC.basic(score[default==1 ], score[default==0 ], F)
    aucc <- auc(score[default==0], score[default==1], F)
    gini <- aucc*2-1
    output <- list( rr = rr, aucc = aucc, gini = gini) 
    return(output)    
  })
    
  calculate_ROC_val <- reactive({ 
    source("functions_Models.r")
    score <- validation_sample()$score
    default <- validation_sample()$default
    rr <- ROC.basic(score[default==1 ], score[default==0 ], F)
    aucc <- auc(score[default==0], score[default==1], F)
    gini <- aucc*2-1
    output <- list( rr = rr, aucc = aucc, gini = gini)
    return(output)   
  })
  
  output$ROC_plot <- renderPlot({    
  #  if( !is.null(user_dataset() ) & input$plotROC == T ) {
    if( !is.null(user_dataset() ) ) {
      rr <- calculate_ROC_dev()$rr
      aucc <- calculate_ROC_dev()$aucc
      gini <- calculate_ROC_dev()$gini
      main_title <- paste("ROC curve, ", 'AUC = ', as.character(round(aucc,2)), ' AR = ', as.character(round(gini,2)), "\n development sample")
      plot(1-rr$Spec, rr$Sens,  main=main_title, xlab='Specificity', ylab='Sensitivity',pty="s", panel.first = grid(), asp=1)  
      abline(0, 1, lwd=2)
    } else { return(plot.new()) }  
  })
  
  output$ROC_plot_v <- renderPlot({   
    # if( !is.null(user_dataset() ) & input$plotROC == T ) {
    if( !is.null(user_dataset() ) ) {
      rr <- calculate_ROC_val()$rr
      aucc <- calculate_ROC_val()$aucc
      gini <- calculate_ROC_val()$gini
      main_title <- paste("ROC curve, ", 'AUC = ', as.character(round(aucc,2)), ' AR = ', as.character(round(gini,2)), "\n validation sample")
      plot(1-rr$Spec, rr$Sens,  main=main_title, xlab='Specificity', ylab='Sensitivity',pty="s", panel.first = grid(), asp=1)  
      abline(0, 1, lwd=1)
    } else { return(plot.new()) }  
  })
  
  output$Score_distribution <- renderPlot({
    if( !is.null(user_dataset() ) ) {
      score <- development_sample()$score
      default <- development_sample()$default
      main_title <- "Score distribution \n development sample"
      plot(c(0,100),c(0,1) ,main=main_title, type="l", col="#0000ff00",lwd=3,
           xlab='Score',ylab='Density', panel.first = grid()) #  transparent score values ( 0-100 , to initiate plot)
      lines(density(score[default==0])$x,density(score[default==0])$y*10,col=EYcolors[7],lwd=3)            #      density of non_defaulted clients
      lines(density(score[default==1])$x,density(score[default==1])$y*10,col=EYcolors[5],lwd=3)             #     density of defaulted clients
      legend('topleft',legend=c('non-defaulted ','defaulted'),lwd=c(2,2),col=c(EYcolors[7],EYcolors[5]),bg='white',cex=1)        
    } else { return(plot.new()) }   
  })
  
  output$Score_distribution_v <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      score <- validation_sample()$score
      default <- validation_sample()$default
      main_title <- "Score distribution \n validation sample"
      plot(c(0,100),c(0,1) ,main=main_title , type="l", col="#0000ff00",lwd=3,
           xlab='Score',ylab='Density', panel.first = grid()) #  transparent score values ( 0-100 , to initiate plot)
      lines(density(score[default==0])$x,density(score[default==0])$y*10,col=EYcolors[7],lwd=3)            #      density of non_defaulted clients
      lines(density(score[default==1])$x,density(score[default==1])$y*10,col=EYcolors[5],lwd=3)             #     density of defaulted clients
      legend('topleft',legend=c('non-defaulted ','defaulted'),lwd=c(2,2),col=c(EYcolors[7],EYcolors[5]),bg='white',cex=1)   
    } else { return(plot.new()) }
  })
  
  output$Rating_distribution <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      dta <- development_sample()$dataset
      score <- development_sample()$score
      default <- development_sample()$default
      #default <- dta$default_1Y
      rating <- development_sample()$rating
      rating_grades <- levels(rating)
      thresholds <-  round(tapply(score , rating , max))
      pd <-  as.numeric(round(table(rating[default==1])/table(rating),4)[rating_grades])    
      bs <- c(0, as.numeric(thresholds[(rating_grades)]) )
      rat <- rating_grades      
      defs <- sort(unique(dta$pd))
      main_title <- "Rating distribution \n development sample"
      plot(c(0,100),c(0,1) ,main=main_title, type="l", col="#0000ff00",lwd=1,
           xlab='Score',ylab='Probability of default (%)', xlim=c(0,100),ylim=c(0,0.5)) #   
      for (i in 2:length(bs)) {
        polygon(bs[c(i-1,i,i,i-1)],c(0,0,.5,.5),col=c(rgb(0.95,0.95,0.95),'lightgray')[i%%2+1],border=c(rgb(0.95,0.95,0.95),'lightgray')[i%%2+1])
        polygon(bs[c(i-1,i,i,i-1)],c(0,0,pd[i-1],pd[i-1]),col=rgb(1,210/255,0),border=c(rgb(0.95,0.95,0.95),'lightgray')[i%%2+1])
        lines(c(bs[i-1],bs[i]),c(defs[i-1],defs[i-1]),lwd=2,col="red")
        text((bs[i-1]+bs[i])/2,0.25,rat[i-1])
        #text((bs[i-1]+bs[i])/2,0.20,default_count[i-1])
        #text((bs[i-1]+bs[i])/2,0.15,score_count[i-1])
        #abline(v=bs,lwd=2,col=rgb(0.5,0.5,0.5))
      }
      #lines(score_o[score_o<=100 & score_o>=0],fit_values_o[score_o<=100 & score_o>=0],type="l",lwd=2)
      legend('topleft',legend=c("A priori (prescribed) PD","Frequency (observed) PD"),lwd=c(2,2),col=c("red",rgb(255,210,0,max=255)),bg='white',cex=1)     
    } else { return(plot.new()) } 
  })
  
  output$Rating_distribution_v <- renderPlot({
    if( !is.null(user_dataset() ) ) {
      dta <- validation_sample()$dataset
      score <- validation_sample()$score
      default <- validation_sample()$default
      #default <- dta$default_1Y
      rating <- validation_sample()$rating
      rating_grades <- levels(rating)
      thresholds <-  round(tapply(score , rating , max))
      pd <-  as.numeric(round(table(rating[default==1])/table(rating),4)[rating_grades])    
      bs <- c(0, as.numeric(thresholds[(rating_grades)]) )
      rat <- rating_grades      
      defs <- sort(unique(dta$pd))
      main_title <- "Rating distribution \n validation sample"
      plot(c(0,100),c(0,1) ,main=main_title, type="l", col="#0000ff00",lwd=1,
           xlab='Score',ylab='Probability of default (%)', xlim=c(0,100),ylim=c(0,0.5)) #   
      for (i in 2:length(bs)) {
        polygon(bs[c(i-1,i,i,i-1)],c(0,0,.5,.5),col=c(rgb(0.95,0.95,0.95),'lightgray')[i%%2+1],border=c(rgb(0.95,0.95,0.95),'lightgray')[i%%2+1])
        polygon(bs[c(i-1,i,i,i-1)],c(0,0,pd[i-1],pd[i-1]),col=rgb(1,210/255,0),border=c(rgb(0.95,0.95,0.95),'lightgray')[i%%2+1])
        lines(c(bs[i-1],bs[i]),c(defs[i-1],defs[i-1]),lwd=2,col="red")
        text((bs[i-1]+bs[i])/2,0.25,rat[i-1])
        #text((bs[i-1]+bs[i])/2,0.20,default_count[i-1])
        #text((bs[i-1]+bs[i])/2,0.15,score_count[i-1])
        #abline(v=bs,lwd=2,col=rgb(0.5,0.5,0.5))
      }
      #lines(score_o[score_o<=100 & score_o>=0],fit_values_o[score_o<=100 & score_o>=0],type="l",lwd=2)
      legend('topleft',legend=c("A priori (prescribed) PD","Frequency (observed) PD"),lwd=c(2,2),col=c("red",rgb(255,210,0,max=255)),bg='white',cex=1)     
    } else { return(plot.new()) } 
  })
  
  output$Count_rating <- renderPlot({
    if( !is.null(user_dataset() ) ) {   
      default <- development_sample()$default   
      rating <- development_sample()$rating    
      main_title <- "Rating class distribution (relative) \n development sample"
      layout(rbind(1,2), heights=c(7,1))
      barplot(table(rating)/length(rating), main=main_title, xlab="Rating", ylab="Distribution (%)", col=EYcolors[5] )
      par(new=T)
      barplot(table(rating[default==1])/length(rating), add=T, col=EYcolors[3], axes=F, axisnames=F )
      par(mar=c(0, 0, 0, 0))
      plot.new()
      legend('center', legend=c("total","defaulted"), pch=c(15,15), col=c(EYcolors[5],EYcolors[3]),bg='white',cex=1, ncol=2,bty ="n")  
      
    } else { return(plot.new()) }    
  })
  
  output$Count_rating_v <- renderPlot({
    if( !is.null(user_dataset() ) ) {   
      default <- validation_sample()$default   
      rating <- validation_sample()$rating    
      main_title <- "Rating class distribution (relative) \n validation sample"
      layout(rbind(1,2), heights=c(7,1))
      barplot(table(rating)/length(rating), main=main_title, xlab="Rating", ylab="Distribution (%)", col=EYcolors[5] )
      par(new=T)
      barplot(table(rating[default==1])/length(rating), add=T, col=EYcolors[3], axes=F, axisnames=F )
      par(mar=c(0, 0, 0, 0))
      plot.new()
      legend('center', legend=c("total","defaulted"), pch=c(15,15), col=c(EYcolors[5],EYcolors[3]),bg='white',cex=1, ncol=2,bty ="n")  
      
    } else { return(plot.new()) }   
  })
  
  
# -----------------------------------------------------
# -- Time dimension
# -----------------------------------------------------

  output$date_range_dev <- renderUI({
    dta <- development_sample()$dataset
    dateRangeInput(inputId="dateRange_dev", label="", start = min(format(dta$record_date,"%Y-%m")), end = max(dta$record_date),
                   min = min(dta$record_date), max = max(dta$record_date), format = "yyyy-mm",
                   startview = "year", weekstart = 1, language = "en",separator = " to ")
  })

  output$date_range_val <- renderUI({
    dta <- validation_sample()$dataset
    dateRangeInput(inputId="dateRange_val", label="", start = min(format(dta$record_date,"%Y-%m")), end = max(dta$record_date),
                   min = min(dta$record_date), max = max(dta$record_date), format = "yyyy-mm",
                   startview = "year", weekstart = 1, language = "en",separator = " to ")
  })

  output$Default_in_time <- renderPlot({
    if( !is.null(user_dataset() ) ) {
      dta <- development_sample()$dataset
      default <- development_sample()$default
      min_date <- as.numeric(format(input$dateRange_dev[1],"%Y%m" ))
      max_date <- as.numeric(format(input$dateRange_dev[2],"%Y%m" ))
      cond <- as.numeric(format(dta$record_date,"%Y%m")) >= min_date & as.numeric(format(dta$record_date,"%Y%m"))  <= max_date
      count<- tapply(default[cond]  , format(dta$record_date,"%Y-%m")[cond] , length)
      #count <- table(as.factor(format(dta$record_date,"%Y-%m"))[as.numeric(format(dta$record_date,"%Y%m")) >= min_date & as.numeric(format(dta$record_date,"%Y%m")) <= max_date ])
      #defs  <- table(as.factor(format(dta$record_date,"%Y-%m"))[dta$default==1 & as.numeric(format(dta$record_date,"%Y%m")) >= min_date & as.numeric(format(dta$record_date,"%Y%m")) <= max_date ])
      defs <- tapply(default[cond]  , format(dta$record_date,"%Y-%m")[cond]  , sum)
      def_rate <- defs/count
      main_title <- "Default rate in time \n development sample"
      plot(def_rate, type="l", lwd=2, main=main_title, xlab="", ylab="Default rate (%)", xaxt="n",col=EYcolors[7], panel.first = grid())
      axis(1, at=seq(def_rate) , labels = names(def_rate))
    } else { return(plot.new()) }
  })

  output$Default_in_time_v <- renderPlot({
    if( !is.null(user_dataset() ) ) {
      dta <- validation_sample()$dataset
      default <- validation_sample()$default
      min_date <- as.numeric(format(input$dateRange_val[1],"%Y%m" ))
      max_date <- as.numeric(format(input$dateRange_val[2],"%Y%m" ))
      cond <- as.numeric(format(dta$record_date,"%Y%m")) >= min_date & as.numeric(format(dta$record_date,"%Y%m"))  <= max_date
      count<- tapply(default[cond]  , format(dta$record_date,"%Y-%m")[cond] , length)
      #count <- table(as.factor(format(dta$record_date[cond],"%Y-%m"))
      defs <- tapply(default[cond]  , format(dta$record_date,"%Y-%m")[cond]  , sum)
      #defs <- table(as.factor(format(dta$record_date[cond],"%Y-%m"))[dta$default_temp==1])
      #defs <-   tapply(dta$default[cond]  , format(dta$record_date,"%Y-%m")[cond]  , sum)
      def_rate <- defs/count
      main_title <- "Default rate in time \n validation sample"
      plot(def_rate, type="l", lwd=2, main=main_title, xlab="", ylab="Default rate (%)", xaxt="n", col=EYcolors[7], panel.first = grid())
      axis(1, at=seq(def_rate) , labels = names(def_rate))
    } else { return(plot.new()) }
  })


# --- default rate per rating in time, using Google Motion Chart - experimental only!
  output$default_by_rating_in_time <- renderGvis({
    #dta <- validation_sample()$dataset
    default <- validation_sample()$default
    rating <- validation_sample()$rating
    record_date <- validation_sample()$dataset$record_date
    temp <- table(rating[default==1], as.factor(format(record_date, "%Y-%m"))[default==1] ) / table(rating, format(record_date, "%Y-%m"))
    #   matrix(table(format(dta$record_date, "%Y-%m")), nrow=length(unique(dta$rating)),ncol=length(unique(format(dta$record_date, "%Y-%m"))) , byrow=T)
    def_rate <- temp
    def_rate <- as.data.frame(def_rate, stringsAsFactors=F)
    names(def_rate) <- c("rating", "record_date", "value")
    def_rate$record_date <- as.Date(paste(def_rate$record_date, "-01",sep=""), tz="", format="%Y-%m-%d")
    gvisMotionChart(def_rate, idvar="rating", timevar="record_date", date.format="%YW%W",
                    options=list(height=400, width=800) )
  })
# -------------------------------------------


# --- relative rating count in time using Google Motion Chart - experimental only!
  output$rating_in_time <- renderGvis({
    dta <- validation_sample()$dataset
    temp <- table(dta$rating, format(dta$record_date, "%Y-%m"))/
      matrix(table(format(dta$record_date, "%Y-%m")), nrow=length(unique(dta$rating)),ncol=length(unique(format(dta$record_date, "%Y-%m"))) , byrow=T)
    rel_count <- temp
    rel_count <- as.data.frame(rel_count, stringsAsFactors=F)
    names(rel_count) <- c("rating", "record_date", "value")
    rel_count$record_date <- as.Date(paste(rel_count$record_date, "-01",sep=""), tz="", format="%Y-%m-%d")
    gvisMotionChart(rel_count, idvar="rating", timevar="record_date", date.format="%YW%W",
                      options=list(height=400, width=800) )
  })
# -------------------------------------------



  Gini_per_period <- reactive({
    dta <- validation_sample()$dataset
    score <- validation_sample()$score
    default <- validation_sample()$default
    record_date <- dta$record_date
    periods <- names(table(format(dta$record_date,"%Y-%m")))
    start_period <- periods[1]
    gini_dev <- calculate_ROC_dev()$gini
    #gini <- NA
    gini <- sapply(seq(length(periods)),  function(i) {
           auc(score[format(record_date,"%Y-%m") >= start_period &  format(record_date,"%Y-%m") <= periods[i] & default==0],
               score[format(record_date,"%Y-%m") >= start_period &  format(record_date,"%Y-%m") <= periods[i] & default==1], F)*2-1
                  })
    output <- list( gini = gini, periods = periods, gini_dev = gini_dev)
    return(output)
  })

  output$Gini_in_time <- renderPlot({
    if( !is.null(user_dataset() ) ) {
    gini <- Gini_per_period()$gini
    gini_dev <- Gini_per_period()$gini_dev
    periods <- Gini_per_period()$periods

    layout(rbind(1,2), heights=c(7,1))

    plot(gini , type="l", lwd=2, main="Cumulative Gini index across periods", xlab="", ylab="value (%)", col=EYcolors[7], xaxt="n", panel.first=grid())
    abline(gini_dev,0, lwd=2, col=EYcolors[2])
    axis(1, at=1:length(gini), labels = periods)
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend('center',
           legend=c("Gini (development sample)","Cumulative Gini (validation sample)"),
           lwd=c(2,2),col=c(EYcolors[2],EYcolors[7]),bg='white',cex=1, ncol=2,bty ="n")

    } else { return(plot.new()) }
  })



# -----------------------------------------------------
# -- Statistical tests
# -----------------------------------------------------

  KS_test_values <- reactive({
    if( !is.null(user_dataset() )  ) {
      score <- development_sample()$score
      default <- development_sample()$default
      KS_critical_value <- 1.36*sqrt(length(score)/( as.numeric(table(default))[1] * as.numeric(table(default))[2] )   )
      KS_statistic <- as.numeric(ks.test(score[default==0],score[default==1])$statistic)
      output <- list( KS_critical_value = KS_critical_value, KS_statistic = KS_statistic)
    } else { return(NULL) }
  })

  KS_test_values_v <- reactive({
    if( !is.null(user_dataset() )   ) {
      score <- validation_sample()$score
      default <- validation_sample()$default
      KS_critical_value <- 1.36*sqrt(length(score)/( as.numeric(table(default))[1] * as.numeric(table(default))[2] )   )
      KS_statistic <- as.numeric(ks.test(score[default==0],score[default==1])$statistic)
      output <- list( KS_critical_value = KS_critical_value, KS_statistic = KS_statistic)
    } else { return(NULL) }
  })


  output$KS_test <- renderPlot({
    if( !is.null(user_dataset() )  ) {
        score <- development_sample()$score
        default <- development_sample()$default
        score_scale <- paste(0:100)
        ks_stat <- ks.test(score[default==0],score[default==1])
        good <- cumsum(table(ceiling(score[default==0])))/sum(table(ceiling(score[default==0])))
        bad <- cumsum(table(ceiling(score[default==1])))/sum(table(ceiling(score[default==1])))
        ks <- data.frame("score" = score_scale, "good"=good[match(score_scale,names(good))], "bad"=bad[match(score_scale,names(bad))])
        if(is.na(ks$bad[1])) ks$bad[1] <- 0
        ks$bad <- approx(ks$bad, method="constant", xout=seq_along(ks$bad), rule=2)$y    # move last known value forward
        ks$difference <- ks$good - ks$bad
        main_title <- paste("K-S test ","D =",round(ks_stat$statistic,4), ", p-value =",round(ks_stat$p.value,4), "\n development sample")
        plot(ks$good, type="l",lwd=3, col=EYcolors[7], main=main_title, xlab="Score", ylab="Distribution function", panel.first = grid())
        lines(ks$bad, type="l", lwd=3 , col=EYcolors[5])
        lines(ks$difference,  type="l", lwd=3, col=EYcolors[2])
        legend('topleft',legend=c('non-defaulted','defaulted', 'difference'),lwd=c(2,2,2),col=c(EYcolors[7],EYcolors[5],EYcolors[2]),bg='white',cex=1)
      } else { return(plot.new()) }
    })

  output$KS_test_v <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      score <- validation_sample()$score
      default <- validation_sample()$default
      score_scale <- paste(0:100)
      ks_stat <- ks.test(score[default==0],score[default==1])
      good <- cumsum(table(ceiling(score[default==0])))/sum(table(ceiling(score[default==0])))
      bad <- cumsum(table(ceiling(score[default==1])))/sum(table(ceiling(score[default==1])))
      ks <- data.frame("score" = score_scale, "good"=good[match(score_scale,names(good))], "bad"=bad[match(score_scale,names(bad))])
      if(is.na(ks$bad[1])) ks$bad[1] <- 0
      ks$bad <- approx(ks$bad, method="constant", xout=seq_along(ks$bad), rule=2)$y    # move last known value forward
      ks$difference <- ks$good - ks$bad
      main_title <- paste("K-S test ","D =",round(ks_stat$statistic,4), ", p-value =",round(ks_stat$p.value,4), "\n validation sample")
      plot(ks$good, type="l",lwd=3, col=EYcolors[7], main=main_title, xlab="Score", ylab="Distribution function", panel.first = grid())
      lines(ks$bad, type="l", lwd=3 , col=EYcolors[5])
      lines(ks$difference,  type="l", lwd=3, col=EYcolors[2])
      legend('topleft',legend=c('non-defaulted','defaulted', 'difference'),lwd=c(2,2,2),col=c(EYcolors[7],EYcolors[5],EYcolors[2]),bg='white',cex=1)
    } else { return(plot.new()) }
  })

  output$KS_flag <- renderUI( {
    if( !is.null(user_dataset() )  ) {
      KS_critical_value <- KS_test_values()$KS_critical_value
      KS_statistic <- KS_test_values()$KS_statistic
      if ( KS_statistic > KS_critical_value  )  {
        HTML("<span class='label label-success'>reject equivalence of distributions</span>")
      } else {
        HTML("<span class='label label-danger'>do not reject equivalence of distributions</span>")
      }
    } else { return(NULL) }
  })

  output$KS_flag_v <- renderUI( {
    if( !is.null(user_dataset() )  ) {
      KS_critical_value <- KS_test_values_v()$KS_critical_value
      KS_statistic <- KS_test_values_v()$KS_statistic
      if ( KS_statistic > KS_critical_value  )  {
        HTML("<span class='label label-success'>reject equivalence of distributions</span>")
      } else {
        HTML("<span class='label label-danger'>do not reject equivalence of distributions</span>")
      }
    } else { return(NULL) }
  })



  PSI <- reactive({
    if( !is.null(user_dataset() )  ) {
      rating_dev <- development_sample()$rating
      rating_val <- validation_sample()$rating
      baseline <- as.numeric(table(rating_dev)/length(rating_dev))
      current <- as.numeric(table(rating_val)/length(rating_val))
#       temp <- data.frame("rating" = sort(unique(dta$rating)), "development" = round(baseline,4), "validation" = round(current,4),
#                          "change" = round(current-baseline,4), "ratio" = round(current/baseline,4), "WoE" = round(log(current/baseline),4),
#                          "contribution" = round(log(current/baseline)*(current-baseline),5) , stringsAsFactors = F)
      temp <- cbind(baseline, current, current-baseline, current/baseline, log(current/baseline),
                    log(current/baseline)*(current-baseline) )
      temp <- rbind(temp, colSums(temp, na.rm=T))
      temp <- data.frame( c(as.character(sort(unique(rating_dev))), "Total"), round(temp,4) )
      psi <- temp[dim(temp)[1], dim(temp)[2]]
      names(temp) <- c("rating", "development (share)", "validation (share)",
                       "change (val-dev)", "ratio (val/dev)", "WoE (log of ratio) ", "contribution (WoE*change)")
#       temp$rating <- as.character(temp$rating)
#       psi <- temp[dim(temp)[1], dim(temp)[2]]
#       temp <- data.frame(rbind(temp, c("Total",apply(temp[-1],2,sum, na.rm=T) )))
#       temp$rating <- c(colnames(def_tab)[-dim(def_tab)[2]],"NULL", "Total")
#
#       names(temp) <- c("rating", "development (share)", "validation (share)",
#                        "change (val-dev)", "ratio (val/dev)", "WoE (log of ratio) ", "contribution (WoE*change)")
      output <- list(psi_table = temp , psi= psi)
      return(output)
    } else { return(NULL) }
  })

  output$PSI_table <- reactive({
    if( !is.null(user_dataset() )  ) {
      PSI_table <- PSI()$psi_table
      tableColor(as.matrix(PSI_table), bold.last=T)
    } else { return(NULL) }
  })
  
  output$PSI_table <- DT::renderDataTable(
    if( !is.null(user_dataset() )  ) {
      PSI_table <- PSI()$psi_table
      PSI_table
    #  tableColor(as.matrix(PSI_table), bold.last=T)
    } else { return(NULL) }
    , options = list(dom = 't', pageLength = 12)
  )
  
  output$PSI_value <- renderText({
    if( !is.null(user_dataset() )  ) {
      PSI_value <- PSI()$psi
      PSI_value
    } else { return(NULL) }
  })

  output$PSI_flag <- renderUI( {
    if( !is.null(user_dataset() )  ) {
      PSI_value <- PSI()$psi
      if ( PSI_value < 0.1  )  {
        HTML("<span class='label label-success'>stable</span>")
      } else {
        if (PSI_value >= 0.1 & PSI_value <= 0.25 ) {
          HTML("<span class='label label-warning'>signs of instability</span>")
        } else {
          HTML("<span class='label label-danger'>unstable</span>")
        }
      }
    } else { return(NULL) }
  })

  output$Bionomial_test <- DT::renderDataTable(
    if( !is.null(user_dataset() )  ) {
      dta <- development_sample()$dataset
      rating <- development_sample()$rating
      default <- development_sample()$default
      pd <- sort(unique(dta$pd))
      count <- as.numeric(table(rating))
      def_rate <- as.numeric(table(rating[default==1])/table(rating))
      pom_table <- data.frame("rating" = sort(unique(rating)), "count" = count, "PD" = pd, "DR" = round(def_rate,4) )
      pom_table$ci_left <- round(pom_table$DR - qnorm(0.975)*sqrt((pom_table$DR)*(1-pom_table$DR)/(pom_table$count)),4)
      pom_table$ci_right <- round(pom_table$DR + qnorm(0.975)*sqrt((pom_table$DR)*(1-pom_table$DR)/(pom_table$count)),4)
      names(pom_table) <- c("rating","count", "PD", "default rate", "lower CI", "upper CI")
     # tableColor(as.matrix(pom_table))
      pom_table
    } else { return(NULL) }
  ,  options = list(dom = 't', pageLength = 12) )

  output$Bionomial_test_v <- DT::renderDataTable(
    if( !is.null(user_dataset() )   ) {
      dta <- validation_sample()$dataset
      rating <- validation_sample()$rating
      default <- validation_sample()$default
      pd <- sort(unique(dta$pd))
      count <- as.numeric(table(rating))
      def_rate <- as.numeric(table(rating[default==1])/table(rating))
      #def_rate <- table(dta$rating[dta$default==1])/table(dta$rating)
      pom_table <- data.frame("rating" = sort(unique(rating)), "count" = count, "PD" = pd, "DR" = round(def_rate,4) )
      pom_table$ci_left <- round(pom_table$DR - qnorm(0.975)*sqrt((pom_table$DR)*(1-pom_table$DR)/(pom_table$count)),4)
      pom_table$ci_right <- round(pom_table$DR + qnorm(0.975)*sqrt((pom_table$DR)*(1-pom_table$DR)/(pom_table$count)),4)
      names(pom_table) <- c("rating","count", "PD", "default rate", "lower CI", "upper CI")
   #   tableColor(as.matrix(pom_table))
      pom_table
    } else { return(NULL) }
    ,  options = list(dom = 't', pageLength = 12) )

  Herfindahl_index <- reactive({
    if( !is.null(user_dataset() )  ) {
      rating_val <- validation_sample()$rating
      current <- as.numeric(table(rating_val)/length(rating_val))
      H_index <- sum(current^2)
      H_norm <- (H_index - 1/length(current)) / (1 - 1/length(current))
      output <- list(H_index = H_index , H_norm = H_norm)
      return(output)
    } else { return(NULL) }
  })

  output$H_index <- renderText({
    if( !is.null(user_dataset() )   ) {
      Herfindahl_index()$H_index
    } else { return(NULL) }
  })

  output$H_norm  <- renderText({
    if( !is.null(user_dataset() ) ) {
      Herfindahl_index()$H_norm
    } else { return(NULL) }
  })

  output$H_test_flag <- renderUI( {
    if( !is.null(user_dataset() )  ) {
      H_norm <-  Herfindahl_index()$H_norm
        if ( H_norm < 0.1  )  {
          HTML("<span class='label label-success'>no concentration</span>")
        } else {
            if (H_norm >= 0.1 & H_norm <= 0.2 ) {
              HTML("<span class='label label-warning'>moderate concentration</span>")
            } else {
              HTML("<span class='label label-danger'>high concentration</span>")
            }
        }
    } else { return(NULL) }
    })


  Chi_square <- reactive({
    if( !is.null(user_dataset() )  ) {
      rating <- validation_sample()$rating
      default <- validation_sample()$default
      pd <- sort(unique(validation_sample()$dataset$pd))
      temp <- data.frame(table(rating), as.numeric(table(rating[default==1])), pd )
      colnames(temp) <- c("rating","count","default", "PD")
      temp$DR <- round(temp$default/temp$count,3)
      temp$exp_def <- round(temp$PD*temp$count,1)
      temp$chi_sq_stat <- round((temp$exp_def - temp$default)^2/temp$exp_def,2)
      Chi_sq_stat <- sum(temp$chi_sq_stat)
      colnames(temp) <- c("rating", "# of observations", "# of defaults" ,"expected \n PD", "default \n rate", "expcted # of defaults", "Chi-square statistic")
      Chi_critical_90 <- qchisq(0.90, length(unique(rating))-1)
      Chi_critical_95 <- qchisq(0.95, length(unique(rating))-1)
      Chi_critical_99 <- qchisq(0.99, length(unique(rating))-1)
      output <- list(final_table = temp, Chi_stat = Chi_sq_stat, Chi_critical_90 = Chi_critical_90,
                     Chi_critical_95=Chi_critical_95, Chi_critical_99=Chi_critical_99 )
      return(output)
    } else { return(NULL) }
  })

  output$Chi_square_table <- DT::renderDataTable(
    if( !is.null(user_dataset() )  ) {
      final_table <- Chi_square()$final_table
     # tableColor(as.matrix(final_table))
      final_table
    } else { return(NULL) }
  , options = list(dom = 't', pageLength = 11) )

  output$Chi_square_test_flag_90 <- renderUI( {
    if( !is.null(user_dataset() )   ) {
      Chi_stat <- Chi_square()$Chi_stat
      Chi_critical <- Chi_square()$Chi_critical_90
      if ( Chi_stat < Chi_critical  )  {
        HTML("<span class='label label-success'>do not reject reasonability of model calibration</span>")
      } else {
        HTML("<span class='label label-danger'>reject reasonability of model calibration</span>")
        }
    } else { return(NULL) }
  })

  output$Chi_square_test_flag_95 <- renderUI( {
    if( !is.null(user_dataset() )  ) {
      Chi_stat <- Chi_square()$Chi_stat
      Chi_critical <- Chi_square()$Chi_critical_95
      if ( Chi_stat < Chi_critical  )  {
        HTML("<span class='label label-success'>do not reject reasonability of model calibration</span>")
      } else {
        HTML("<span class='label label-danger'>reject reasonability of model calibration</span>")
      }
    } else { return(NULL) }
  })

  output$Chi_square_test_flag_99 <- renderUI( {
    if( !is.null(user_dataset() )   ) {
      Chi_stat <- Chi_square()$Chi_stat
      Chi_critical <- Chi_square()$Chi_critical_99
      if ( Chi_stat < Chi_critical  )  {
        HTML("<span class='label label-success'>do not reject reasonability of model calibration</span>")
      } else {
        HTML("<span class='label label-danger'>reject reasonability of model calibration</span>")
      }
    } else { return(NULL) }
  })


# -----------------------------------------------------
# -- Individual variables
# -----------------------------------------------------

  calculate_ROC_ind_dev <- reactive({
    if( !is.null(user_dataset() )  ) {
      source("functions_Models.r")
      dta <- development_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,variable_name]
      default <- development_sample()$default
      rr <- ROC.basic(variable[default==1 & !is.na(variable)], variable[default==0 & !is.na(variable)], F)
      aucc <- auc(variable[default==0 & !is.na(variable)], variable[default==1 & !is.na(variable)], F)
      gini <- aucc*2-1
      output <- list( rr = rr, aucc = aucc, gini = gini, variable_name = variable_name)
      return(output)
    } else { return(NULL) }
  })

  calculate_ROC_ind_val <- reactive({
    if( !is.null(user_dataset() ) ) {
      source("functions_Models.r")
      dta <- validation_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,variable_name]
      default <- validation_sample()$default
      rr <- ROC.basic(variable[default==1 & !is.na(variable)], variable[default==0 & !is.na(variable)], F)
      aucc <- auc(variable[default==0 & !is.na(variable)], variable[default==1 & !is.na(variable)], F)
      gini <- aucc*2-1
      output <- list( rr = rr, aucc = aucc, gini = gini, variable_name = variable_name)
      return(output)
    } else { return(NULL) }
  })

  output$ROC_plot_ind <- renderPlot({
    if( !is.null(user_dataset() ) ) {
      rr <- calculate_ROC_ind_dev()$rr
      aucc <- calculate_ROC_ind_dev()$aucc
      gini <- calculate_ROC_ind_dev()$gini
      variable_name = calculate_ROC_ind_dev()$variable_name
      main_title <- paste("ROC curve, ",variable_name, ' AUC = ', as.character(round(aucc,2)), ' AR = ', as.character(round(gini,2)), "\n development sample")
      plot(1-rr$Spec, rr$Sens,  main=main_title, xlab='Specificity', ylab='Sensitivity',pty="s", panel.first = grid())
    } else { return(plot.new()) }
  })

  output$ROC_plot_ind_v <- renderPlot({
    if( !is.null(user_dataset() ) ) {
      rr <- calculate_ROC_ind_val()$rr
      aucc <- calculate_ROC_ind_val()$aucc
      gini <- calculate_ROC_ind_val()$gini
      variable_name = calculate_ROC_ind_val()$variable_name
      main_title <- paste("ROC curve, " ,variable_name, ' AUC = ', as.character(round(aucc,2)), ' AR = ', as.character(round(gini,2)), "\n validation sample")
      plot(1-rr$Spec, rr$Sens,  main=main_title, xlab='Specificity', ylab='Sensitivity',pty="s", panel.first = grid())
    } else { return(plot.new()) }
  })

  summary_variable <- function( variable, default ){
    if ( class(variable) %in% c("numeric", "integer" ) ) {
      sum_table <- round(as.data.frame(t(as.matrix(summary(variable)))),2)[1:6]
      sum_table_2 <- round(data.frame(length(variable[default==1]),
      100*length(variable[default==1])/length(variable), 100*length(variable[is.na(variable)])/length(variable)),2)
      names(sum_table_2) <- c("# def","% DR","% NA's")
      final_table <- as.data.frame(cbind(sum_table,sum_table_2))
    } else {
      def_tab <- table(default, variable, exclude=NULL)
      p_non_def <- def_tab[1,]/sum(def_tab[1,])           # information value
      p_def <- def_tab[2,]/sum(def_tab[2,])               # information value
      inf_value <- (p_non_def-p_def)*log(p_non_def/p_def) # information value
      inf_value[inf_value %in% c("Inf","-Inf")] <- 0
      temp <- cbind("count" = as.numeric(apply(def_tab,2,sum)), "defaults" = as.numeric(def_tab[2,]),
                         "distribution" = as.numeric(apply(def_tab,2,sum)/sum(def_tab)), "DR" = as.numeric(def_tab[2,]/apply(def_tab,2,sum)),
                         "Inf.Value" = as.numeric(inf_value))
      temp <- data.frame(rbind(temp, apply(temp,2,sum, na.rm=T) ))
      temp$value <- c(colnames(def_tab)[-dim(def_tab)[2]],"NULL", "Total")
      temp <- temp[,c("value", "count", "defaults", "distribution", "DR","Inf.Value")]
      temp$DR[nrow(temp)] <- temp$defaults[nrow(temp)] / temp$count[nrow(temp)]
      temp[4:6] <- round(temp[4:6],4)
      final_table <- temp
    }
  return(final_table)
  }


  output$variable_summary <- DT::renderDataTable(
    if( !is.null(user_dataset() )   ) {
      dta <- development_sample()$dataset
      variable <- dta[,input$variable]
      default <- development_sample()$default
   #   tableColor(as.matrix(summary_variable( variable, default )), bold.last=T )
      summary_variable(variable,default)
    } else { return(NULL) }
  , options = list(dom = 't', pageLength = 12) )

  output$variable_summary_v <- DT::renderDataTable(
    if( !is.null(user_dataset() )   ) {
      dta <- validation_sample()$dataset
      variable <- dta[,input$variable]
      default <- validation_sample()$default
   #   tableColor(as.matrix(summary_variable( variable, default )) , bold.last=T  )
      summary_variable(variable,default)
    } else { return(NULL) }
    , options = list(dom = 't', pageLength = 12) )


  output$summary_numeric <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      dta <- development_sample()$dataset
      variable <- dta[,input$variable]
      if ( class(variable) %in% c("numeric","integer")) {
        main_title <- paste("Box-whisker plot for variable: ",input$variable, "\n development sample")
        boxplot(variable, horizontal=T, xlab="Value", main=main_title )
        grid(NULL,NA)
    } else { cat(as.character(NULL)) }
    } else { cat(as.character(NULL)) }
  })


  output$summary_numeric_v <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      dta <- validation_sample()$dataset
      variable <- dta[,input$variable]
      if ( class(variable) %in% c("numeric","integer")) {
        main_title <- paste("Box-whisker plot for variable: ",input$variable, "\n validation sample")
        boxplot(variable, horizontal=T, xlab="Value", main=main_title )
        grid(NULL,NA)
      } else { cat(as.character(NULL)) }
    } else { cat(as.character(NULL)) }
  })


  plot_default_rate <- function( variable_name, variable, default, sample , breaks_no ) {

    if ( class(variable) %in% c("numeric","integer")) {
      breakpoints <- quantile(variable,seq(0,1,length=breaks_no), na.rm=T)
      count <- hist(variable,breaks=as.numeric(breakpoints),plot = FALSE)
      def <- hist(variable[default==1],breaks=as.numeric(breakpoints),plot = FALSE)
      point <- seq(1,(length(breakpoints)-1),by=1)
      values <- round(breakpoints[2:(length(breakpoints))],3)
      def_rate <- round(def$counts/count$counts,4)
      main_title <- paste("Default rate for variable: ",variable_name, sample, sep="")
      plot(def_rate, ylim=c(0,max(def_rate)), type="l",lwd=2,xaxt="n",xlab='Upper bound of variable',ylab='Default rate (%)', main=main_title  )
      axis(1,at=point, labels=values)
      grid()
    } else {
      par(mar=c(5.1,4.1,4.1,4.1))
      mp <- barplot(table(variable), col=EYcolors[5], ylim=c(0,max(pretty(table(variable)))), axisnames=F )
      grid(NA,NULL)
      par(new=T)
      main_title <- paste("Default rate for variable: ",variable_name, sample, sep="")
      barplot(table(variable),main=main_title, col=EYcolors[5], axes=F, ylim=c(0,max(pretty(table(variable)))), ylab="Count" ,names.arg=F )
      barplot(table(variable[default==1]), add=T, col=EYcolors[3], axes=F, names.arg=F)
      text(mp,  par('usr')[3],labels = names(table(variable)),  srt=45,  adj = c(1.1,1.1), xpd=T, cex=.9)
      par(new=T)
      def_rate <- table(variable[default==1])/table(variable)
      plot(def_rate, axes=F,bty = "n", type="l", lwd=2, main="", xlab="", ylab="", col=EYcolors[7])
      axis(4, at=pretty(c(0,range(def_rate ))))
      mtext("Default rate (%)",side=4, line=3 )
    }

  }

  output$default_rate <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      dta <- development_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,input$variable]
      default <- development_sample()$default
      sample <- "\n development sample"
      plot_default_rate( variable_name, variable, default, sample , breaks_no = input$slider_dev)
      #round(summary_variable( variable, default ), 2)
    } else { return(plot.new()) }
  })

  output$default_rate_v <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      dta <- validation_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,input$variable]
      default <- validation_sample()$default
      sample <- "\n validation sample"
      plot_default_rate( variable_name, variable, default, sample, breaks_no = input$slider_val )
      #round(summary_variable( variable, default ), 2)
      } else { return(plot.new()) }
    })

  output$quantile_slider_dev <- renderUI({
    if( !is.null(user_dataset() )  ) {
    if( class(development_sample()$dataset[,input$variable]) %in% c("numeric","integer")   ) {
    slider <- tags$div(HTML("<table border=0 align=center width=90%><tr><td width=40% align=center valign=bottom>
                            <p>Choose quantiles: </p></td><td valign=top align=center>"),
                    sliderInput(inputId= "slider_dev", label="", min=3, max=11, value=5, step = 1 ),
      HTML("</td></tr></table>"))
      return(slider)
    } else { return(NULL)}
  #  } else { return(NULL) }
    }
  })

  output$quantile_slider_val <- renderUI({
    if( !is.null(user_dataset() )  ) {
    if( class(validation_sample()$dataset[,input$variable]) %in% c("numeric","integer")   ) {
      slider <- tags$div(HTML("<table border=0 align=center width=90%><tr><td width=40% align=center valign=bottom>
                              <p>Choose quantiles: </p></td><td valign=top align=center>"),
                         sliderInput(inputId= "slider_val", label="", min=3, max=11, value=5, step = 1),
                         HTML("</td></tr></table>"))
      return(slider)
    } else { return(NULL)}
    }
  })


  plot_score_split <- function( variable_name, variable, score, main_title , breaks_no ){
    if ( class(variable) %in% c("numeric","integer")) {
      breakpoints <- quantile(variable,seq(0,1,length=breaks_no), na.rm=T)
      boxplot(score~cut(variable, breakpoints), main=main_title,ylab="Score", xlab="Interval", range=0, col=EYcolors[5]  )
      grid(NA,NULL)
    } else {
      boxplot(score~variable,  xaxt="n", main=main_title ,ylab="Score", xlab="", range=0, col=EYcolors[5] )
      grid(NA,NULL)
      text(seq(names(table(variable))),  par('usr')[3],labels = names(table(variable)),  srt=45,  adj = c(1.1,1.1), xpd=T, cex=.9)
    }
  }

  output$score_q_slider_dev <- renderUI({
    if( !is.null(user_dataset() )   ) {
    if( class(development_sample()$dataset[,input$variable]) %in% c("numeric","integer")   ) {
      slider <- tags$div(HTML("<table border=0 align=center width=90%><tr><td width=40% align=center valign=bottom>
                              <p>Choose quantiles: </p></td><td valign=top align=center>"),
                         sliderInput(inputId= "score_slider_dev", label="", min=3, max=11, value=5, step = 1 ),
                         HTML("</td></tr></table>"))
      return(slider)
    } else { return(NULL)}
    }
  })

  output$score_q_slider_val <- renderUI({
    if( !is.null(user_dataset() )   ) {
    if( class(validation_sample()$dataset[,input$variable]) %in% c("numeric","integer")   ) {
     # tryCatch({
        slider <- tags$div(HTML("<table border=0 align=center width=90%><tr><td width=40% align=center valign=bottom>
                              <p>Choose quantiles: </p></td><td valign=top align=center>"),
                         sliderInput(inputId= "score_slider_val", label="", min=3, max=11, value=5, step = 1),
                         HTML("</td></tr></table>"))
        return(slider)
   #   }, error = function(e) { stop(paste('Choose variable')) })
    } else { return(NULL)}
    }
  })


  output$score_split <- renderPlot({
    if( !is.null(user_dataset() )  ) {
   #   tryCatch({
      dta <- development_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,input$variable]
      score <- development_sample()$score
      default <- development_sample()$default
      main_title <- paste("Score breakdown (non-default) for variable: ",variable_name, "\n development sample",sep="")
      plot_score_split( variable_name, variable[default==0], score[default==0], main_title=main_title , breaks_no = input$score_slider_dev)
      #round(summary_variable( variable, default ), 2)
   #   }, error = function(e) { stop(paste('Don\'t panic! \n Choose variable or decrease number of quantiles.')) })
    } else { return(plot.new()) }
  })

  output$score_split_v <- renderPlot({
    if( !is.null(user_dataset() ) ) {
      dta <- validation_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,input$variable]
      score <- validation_sample()$score
      default <- validation_sample()$default
      main_title <- paste("Score breakdown (non-default) for variable: ",variable_name, "\n validation sample",sep="")
      plot_score_split( variable_name, variable[default==0], score[default==0], main_title=main_title , breaks_no = input$score_slider_val)
      #round(summary_variable( variable, default ), 2)
    } else { return(plot.new()) }
  })


  output$score_split_default <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      #   tryCatch({
      dta <- development_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,input$variable]
      score <- development_sample()$score
      default <- development_sample()$default
      main_title <- paste("Score breakdown (default) for variable: ",variable_name, "\n development sample",sep="")
      plot_score_split( variable_name, variable[default==1], score[default==1], main_title=main_title , breaks_no = input$score_slider_dev)
      #round(summary_variable( variable, default ), 2)
      #   }, error = function(e) { stop(paste('Don\'t panic! \n Choose variable or decrease number of quantiles.')) })
    } else { return(plot.new()) }
  })

  output$score_split_default_v <- renderPlot({
    if( !is.null(user_dataset() )  ) {
      dta <- validation_sample()$dataset
      variable_name <- input$variable
      variable <- dta[,input$variable]
      score <- validation_sample()$score
      default <- validation_sample()$default
      main_title <- paste("Score breakdown (default) for variable: ",variable_name, "\n validation sample",sep="")
      plot_score_split( variable_name, variable[default==1], score[default==1], main_title=main_title , breaks_no = input$score_slider_val)
      #round(summary_variable( variable, default ), 2)
    } else { return(plot.new()) }
  })

  output$DataTable = renderDataTable({

    if (!is.null(user_dataset())) {
      user_dataset()
    } else { return(NULL) }

  }, options = list(orderClasses = TRUE , pageLength = 5, scrollX = "100%",  scrollCollapse =TRUE))




