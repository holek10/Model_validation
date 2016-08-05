library(shiny)
library(shinydashboard)
library(DT)


loadingBar <- tags$div(class="progress progress-striped active", tags$div(class="bar", style="width: 100%;"))

# Code for loading message
loadingMsg <- tags$div(class="modal", style="position:fixed; top:40% !important; left:50%;", tabindex="-1", role="dialog", "aria-labelledby"="myModalLabel", "aria-hidden"="true",  
                       tags$div(class="modal-header",tags$h3(id="myModalHeader", "In progress...")),
                       tags$div(class="modal-footer",loadingBar)
) 



ui <- dashboardPage(
                   
  dashboardHeader(title = "Model validation tool", titleWidth = 300
  ),
  dashboardSidebar(
    tags$head(
      tags$script(
        HTML(
          "
          $(document).ready(function(){
          // Bind classes to menu items, easiet to fill in manually
          var ids = ['subItemOne','subItemTwo','subItemThree','subItemFour'];
          for(i=0; i<ids.length; i++){
          $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
          }
          
          // Register click handeler
          $('.my_subitem_class').on('click',function(){
          // Unactive menuSubItems
          $('.my_subitem_class').parent().removeClass('active');
          })
          })
          "
        )
      )
      ),
    width = 300,
    sidebarMenu(
      menuItem(text = "Data upload", tabName = "dataUpload", icon = icon("database"), selected = TRUE
      ),
      menuItem(text = "Basic characteristics", tabName = "basicTab", icon = icon("info-circle"), 
        menuSubItem(text = "ROC curve", tabName = "plotROC"),
        menuSubItem(text = "Score distribution", tabName = "plotScoreDist"),
        menuSubItem(text = "Rating distribution", tabName = "plotRatingDist"),
        menuSubItem(text = "Rating class distribution", tabName = "plotCountRating")
      ),
      menuItem(text = "Time dimension", tabName = "timeDimension", icon = icon("clock-o"),
        menuSubItem(text = "Default rate over time", tabName = "plotDefaultInTime"),
        menuSubItem(text = "Default rate per rating over time", tabName = "plotDefaultByRatingInTime"),
        menuSubItem(text = "Rating class distribution over time", tabName = "plotRatingInTime"),
        menuSubItem(text = "Gini index over time", tabName = "plotGini")
      ),
      menuItem(text = "Statistical tests", tabName = "statTests", icon = icon("bar-chart"), 
        menuSubItem(text = "Kolmogorov-Smirnov test", tabName = "plotKS"),
        menuSubItem(text = "Population Stability Index (PSI)", tabName = "showPSI"),
        menuSubItem(text = "Binomial test (confidence interval)", tabName = "showBinTest"),
        menuSubItem(text = "Herfindahl Index", tabName = "showHerfindahl"),
        menuSubItem(text = "Chi-square test", tabName = "showChiSquare")
      ),
      menuItem(text = "Individual variables", tabName = "variables", icon = icon("search-plus"), 
        # br(),p("Select a variable from the list below"),
        htmlOutput("variable_choice"),
        htmlOutput("numeric_variable"),
        hr(),
        menuSubItem(text = "Variable summary", tabName = "showVarSummary"),
        menuSubItem(text = "ROC curve", tabName = "plotROC_ind"),
        menuSubItem(text = "Default rate", tabName = "plotDefRate"),
        menuSubItem(text = "Score distribution", tabName = "plotScoreSplit")           
      ),
      # conditionalPanel(condition = "input.is_numeric == 1",
      #                  p("Numeric variable is split using quantiles (quartiles by default). Use slider to see more granular split.")),
      menuItem(text = "Data explorer", tabName = "dataExplorer", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dataUpload", 
        fluidRow(
          column(8, offset = 2, height = 150,
            htmlOutput("user_dataset"),
            # htmlOutput("dataset_check"), 
            htmlOutput("data_loaded"),
            conditionalPanel("$('html').hasClass('shiny-busy')",loadingMsg)
            #       htmlOutput("dataset_message"), 
            #       htmlOutput("dataset_loaded") , 
            #       htmlOutput("dataset_type")
          )
        ),
        fluidRow(
          box(
            title = "Select RDS file", status = "primary", solidHeader = TRUE, collapsible = T, collapsed = F, footer = "Wait until progress bar reaches the end", 
            "RDS is a special R format which contains single object.",br(),
            "Please refer to user manual for creating pre-defined input data structure.",br(),br(),
            fileInput(inputId = "file", label = "Upload a file", accept=c('.rds'), width = "100%")
          ),
          box(
            title = "Select from working directory", status = "primary", solidHeader = T, collapsible = F, footer = "Make sure there are some valid files in working directory",
            p("Choose appropriate file from your working directory (demo only) and press the button."),
            selectInput(inputId = "dir_file", label="", choices=c("none", list.files(pattern="*.rds")), selected = "none", multiple = FALSE),
            actionButton(inputId = "wd_file_load",label = "Load a file" )
          )
        ),
        fluidRow(
          conditionalPanel(condition = " input.dataset_loaded == 1",
            column(width = 6, offset = 3,
              box(width=12, p("Select your target variable:") ,  htmlOutput("choose_default")   
              )
            )
          )
        )
      ),
      tabItem(tabName = "basicTab",
              h3("basic overview")
             
      ),
  # Basic characteristics tab panel
      tabItem(tabName = "plotROC",
        box(
          title = "ROC curve for development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("ROC_plot", width ="400px", height = "400px"))
        ),
        box(
          title = "ROC curve for validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("ROC_plot_v", width ="400px", height = "400px"))
        )
      ),
      tabItem(tabName = "plotScoreDist",
        box(
          title = "Score distribution for development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("Score_distribution"))
        ),
        box(
          title = "Score distribution for validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("Score_distribution_v"))
        )
      ),
      tabItem(tabName = "plotRatingDist",
        box(
          title = "Rating distribution for development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("Rating_distribution"))
        ),
        box(
          title = "Rating distribution for validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("Rating_distribution_v"))
        )
      ),
      tabItem(tabName = "plotCountRating",
        box(
          title = "Rating class distribution for development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("Count_rating"))
        ),
        box(
          title = "Rating class distribution for validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("Count_rating_v"))
        )
      ),
  # Time dimension tab panel
      tabItem(tabName = "plotDefaultInTime",
        box(
          title = "Default rate over time for development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("Default_in_time")), 
          div(p("Specify date range :    "), htmlOutput("date_range_dev"))
        ),
        box(
          title = "Default rate over time for validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center",plotOutput("Default_in_time_v")), 
          div(p("Specify date range : "), htmlOutput("date_range_val"))
        )
      ),
      tabItem(tabName = "plotDefaultByRatingInTime",
        box(width = 12, 
          title = "Default rate by rating over time ", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", htmlOutput("default_by_rating_in_time"))
        )
      ),
      tabItem(tabName = "plotRatingInTime",
        box(width = 12, 
            title = "Rating over time", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
            div(align = "center", htmlOutput("rating_in_time"))
        )
      ),
      tabItem(tabName = "plotGini",
        box(width = 12, 
            title = "Gini coefficient over time", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
            div(align = "center", htmlOutput("Gini_in_time"))
        )
      ),
  # Statistical tests tab panel 
      tabItem(tabName = "plotKS",
        box(
          title = "K-S test for development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("KS_test"), uiOutput("KS_flag"))
        ),
        box(
          title = "K-S test for validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("KS_test_v"), uiOutput("KS_flag_v"))
        )
      ),
      tabItem(tabName = "showPSI",
        box(width = 12, 
          title = "Population Stability Index (PSI)", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          column(9, align = "center", DT::dataTableOutput("PSI_table")),
          column(3, align = "center",p("Population Stability Index value: "), strong(uiOutput("PSI_value")), uiOutput("PSI_flag"))
        )
      ),
      tabItem(tabName = "showBinTest",
        box(
          title = "Binomial test for development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", DT::dataTableOutput("Bionomial_test"))
        ),
        box(
          title = "Binomial test for validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", DT::dataTableOutput("Bionomial_test_v"))
        )
      ),
      tabItem(tabName = "showHerfindahl",
        box( 
            title = "Herfindahl Index", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
            column(6, align = "center", p("Herfindahl Index: "), strong(uiOutput("H_index")) ),
            column(6, align = "center",p("Normalized Herfindahl Index: "), strong(uiOutput("H_norm")), uiOutput("H_test_flag"))
        )
      ),  
      tabItem(tabName = "showChiSquare",
        box(width = 12, 
            title = "Population Stability Index (PSI)", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
            column(9, align = "center", DT::dataTableOutput("Chi_square_table")),
            column(3, align = "center", br(), 
                   p("Confidence level of 90% "), uiOutput("Chi_square_test_flag_90"), hr(),
                   p("Confidence level of 95% "), uiOutput("Chi_square_test_flag_95"), hr(),
                   p("Confidence level of 99% "), uiOutput("Chi_square_test_flag_99"))
        )
      ), 
  #Individual variables tab panel
      tabItem(tabName = "showVarSummary",
        box(
          title = "Summary for a variable in development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", DT::dataTableOutput("variable_summary")),
          conditionalPanel("input.is_numeric == 1", plotOutput("summary_numeric", height="300px") )
        ),
        box(
          title = "Summary for a variable in validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", DT::dataTableOutput("variable_summary_v")),
          conditionalPanel("input.is_numeric == 1", plotOutput("summary_numeric_v", height="300px") )
        )
      ),
      tabItem(tabName = "plotROC_ind",
        box(
          title = "ROC curve for a variable in development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("ROC_plot_ind"))
        ),
        box(
          title = "ROC curve for a variable in validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("ROC_plot_ind_v"))
        )
      ),
      tabItem(tabName = "plotDefRate",
        box(
          title = "Default rate for a variable in development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("default_rate"), htmlOutput("quantile_slider_dev"))
        ),
        box(
          title = "Default rate for a variable in validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", plotOutput("default_rate_v"), htmlOutput("quantile_slider_val"))
        )
      ),
      tabItem(tabName = "plotScoreSplit",
        box(
          title = "Score breakdown for a variable in development sample", status = "success", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", htmlOutput("score_q_slider_dev"), plotOutput("score_split"),plotOutput("score_split_default") )
        ),
        box(
          title = "Score breakdown for a variable in validation sample", status = "warning", solidHeader = T, footer = "Allow some time for calculation and plotting",
          div(align = "center", htmlOutput("score_q_slider_val"), plotOutput("score_split_v"), plotOutput("score_split_default_v") )
        )     
      ),
  # Data Explorer tab panel
      tabItem(tabName = "dataExplorer", 
        dataTableOutput('DataTable')  
      )
    )
  )
)


server <- function(input, output) {
 
source("server_logic.R", local=T)

  
}



shinyApp(ui, server)
