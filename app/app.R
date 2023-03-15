library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(data.table)
library(Rnumerai)
library(DT)


# ==============================================================================
# Leaderboard
# ==============================================================================

# Download latest leaderboard from Numerai and get a list of all models
d_lb <- get_leaderboard()
ls_model <- sort(d_lb$username)


# ==============================================================================
# Helper Functions
# ==============================================================================

# Download raw data
download_raw_data <- function(model_name) {
  
  # Download data from Numerai
  d_raw <- round_model_performances(model_name)
  
  # Remove rows without CORR
  d_raw <- d_raw[!is.na(d_raw$corr), ]
  
  # Add the model name
  d_raw$model <- model_name
  
  # Return
  return(d_raw)
  
}

# Reformat
reformat_data <- function(d_raw) {
  
  # Keep some columns only
  col_keep <- c("model", "corr", "corrPercentile", "corrWMetamodel", 
                "fncV3", "fncV3Percentile", "payout", "roundPayoutFactor",
                "roundNumber", "roundResolved", "selectedStakeValue",
                "tc", "tcPercentile")
  d_munged <- as.data.table(d_raw[, col_keep])
  
  # Reformat percentile
  d_munged[, corrPercentile := round(corrPercentile * 100, 6)]
  d_munged[, fncV3Percentile := round(fncV3Percentile * 100, 6)]
  d_munged[, tcPercentile := round(tcPercentile * 100, 6)]
  
  # Reorder columns
  setcolorder(d_munged, c("model", "roundNumber", "roundResolved",
                          "selectedStakeValue",
                          "corr", "corrPercentile", 
                          "fncV3", "fncV3Percentile",
                          "tc", "tcPercentile",
                          "corrWMetamodel",
                          "roundPayoutFactor", "payout"))
  
  # Rename columns
  colnames(d_munged) <- c("model", "round", "resolved", 
                          "stake",
                          "corr", "corr_pct",
                          "fncv3", "fncv3_pct",
                          "tc", "tc_pct", 
                          "corr_meta",
                          "pay_ftr", "payout")
  
  # Return
  return(d_munged)
  
}


# ==============================================================================
# UI
# ==============================================================================

ui <- shinydashboardPlus::dashboardPage(
  
  title = "Shiny Numerati",
  
  skin = "black-light",
  
  options = list(sidebarExpandOnHover = TRUE),
  
  header = shinydashboardPlus::dashboardHeader(
    title = "✨ Shiny Numerati",
    userOutput("user")
  ),
  
  sidebar = shinydashboardPlus::dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      menuItem(text = "Start Here", tabName = "start", icon = icon("play")),
      menuItem(text = "Performance", tabName = "performance", icon = icon("line-chart")),
      menuItem(text = "Payout", tabName = "payout", icon = icon("credit-card")),
      menuItem(text = "About", tabName = "about", icon = icon("question-circle"))
    ), 
    minified = TRUE,
    collapsed = FALSE
  ),
  
  body = dashboardBody(
    
    tabItems(
      
      # ========================================================================
      
      tabItem(tabName = "start", 
              
              fluidPage(
                
                markdown("# **Shiny Numerati**"),
                markdown("### Community Dashboard for the Numerai Classic Tournament"),
                
                br(),
                
                fluidRow(
                  
                  column(6, 
                         
                         markdown("## **Step 1 - Select Your Models**"),
                         
                         markdown("### First, click this ⬇"),
                         
                         pickerInput(inputId = "model",
                                     label = " ",
                                     choices = ls_model,
                                     multiple = TRUE,
                                     width = "100%",
                                     options = list(
                                       `title` = "---------->>> HERE <<<----------",
                                       `header` = "Notes: 1) Use the search box below to find and select your models. 2) Use 'Select All' for quick selection.",
                                       size = 20,
                                       `actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `live-search-placeholder` = "For example, try  lgbm_v4  or  integration_test",
                                       `virtual-scroll` = TRUE,
                                       `multiple-separator` = ", ",
                                       `selected-text-format`= "count > 3",
                                       `count-selected-text` = "{0} models selected (out of {1})",
                                       `deselect-all-text` = "Deselect All",
                                       `select-all-text` = "Select All"
                                     )
                         )
                  ),
                  
                  column(6,
                         
                         markdown("## **Step 2 - Download Data**"),
                         
                         markdown("### Next, click this ⬇ (it may take a while)"),
                         
                         br(),
                         
                         actionBttn(inputId = "button_download", 
                                    label = "Download Data from Numerai",
                                    color = "primary",
                                    icon = icon("cloud-download"),
                                    style = "gradient",
                                    block = TRUE
                         )
                  )
                ),
                
                br(),
                
                h3(strong(textOutput(outputId = "text_download"))),
                verbatimTextOutput(outputId = "print_download"),
                
                br(),
                
                h3(strong(textOutput(outputId = "text_preview"))),
                shinycssloaders::withSpinner(DTOutput("dt_model")),
                
                br(),
                
                h3(strong(textOutput(outputId = "text_next")))
                
              )
      ),
      
      # ========================================================================
      
      tabItem(tabName = "performance", 
              fluidPage(
                markdown("![image](https://media.giphy.com/media/cftSzNoCTfSyAWctcl/giphy.gif)")
              )
      ),
      
      tabItem(tabName = "payout", 
              fluidPage(
                markdown("![image](https://media.giphy.com/media/cftSzNoCTfSyAWctcl/giphy.gif)")
              )
      ),
      
      # ========================================================================
      
      tabItem(tabName = "about", 
              markdown("## **About this App**"),
              markdown('#### Yet another Numerai community dashboard by <b><a href="https://linktr.ee/jofaichow" target="_blank">Jo-fai Chow</a></b>.'),
              
              br(),
              markdown("## **Acknowledgement**"),
              markdown("#### This hobby project was inspired by Rajiv's <a href='https://huggingface.co/spaces/rajistics/shiny-kmeans' target='_blank'>shiny-kmeans</a> on 🤗 Spaces."),
              
              br(),
              markdown("## **Changelog**"),
              markdown(
                "
                - #### **0.1.0** — First prototype with an interactive table output
                "),
              br(),
              markdown("## **Session Info**"),
              verbatimTextOutput(outputId = "session_info")
      )
      
      # ========================================================================
      
    ) # end of tabItems
    
  ),
  
  footer = shinydashboardPlus::dashboardFooter(
    left = "Powered by ❤️, ☕, Shiny, and 🤗 Spaces",
    right = paste0("Version 0.1.0"))
  
)



# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output) {
  
  # About Joe
  output$user <- renderUser({
    dashboardUser(
      name = "JC",
      image = "https://numerai-public-images.s3.amazonaws.com/profile_images/aijoe_v5_compressed-iJWEo1WeHkpH.jpg",
      subtitle = "@matlabulous",
      footer = p('"THE NMR LIFE CHOSE ME."', class = 'text-center')
    )
  })
  
  
  # ============================================================================
  # Reactive --> Download Model Data
  # ============================================================================
  
  react_download <- eventReactive(input$button_download, {sort(input$model)})
  
  output$print_download <- renderPrint({react_download()})
  
  output$text_download <- renderText({
    if (length(react_download()) >= 1) "Your Selection:" else " "
  })
  
  output$text_preview <- renderText({
    if (length(react_download()) >= 1) "Data Preview:" else " "
  })
  
  output$text_next <- renderText({
    if (length(react_download()) >= 1) "⬅ [Coming Soon] Performance and Payout Charts 📈📊🔥" else " "
  })
  
  react_d_model <- eventReactive(
    input$button_download,
    {
      
      # Download dataframes one by one (may parallelise this in the future)
      d_raw <- c()
      for (item in input$model) d_raw <- rbind(d_raw, download_raw_data(item))
      
      # Data munging
      d_munged <- reformat_data(d_raw)
      
      # Return final result
      d_munged
      
    }
  )
  
  output$dt_model <- DT::renderDT({
    
    DT::datatable(
      
      # Data
      react_d_model(),
      
      # Other Options
      rownames = FALSE,
      extensions = "Buttons",
      options =
        list(
          dom = 'Bflrtip', # https://datatables.net/reference/option/dom
          buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          order = list(list(0, 'asc'), list(1, 'asc')),
          pageLength = 5,
          lengthMenu = c(5, 10, 20, 100, 500, 1000, 50000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("corr", "tc", "fncv3", "corr_meta", "pay_ftr"), digits = 4) |>
      formatRound(columns = c("corr_pct", "tc_pct", "fncv3_pct"), digits = 1) |>
      formatRound(columns = c("stake", "payout"), digits = 2) |>
      
      formatStyle(columns = c("model"),
                  fontWeight = "bold") |>
      
      formatStyle(columns = c("stake"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "#2196F3"))) |>
      
      formatStyle(columns = c("corr", "fncv3"),
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "black"))) |>
      
      formatStyle(columns = c("tc"),
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "#A278DC"))) |>
      
      formatStyle(columns = c("corr_pct", "tc_pct", "fncv3_pct"),
                  color = styleInterval(cuts = c(1, 5, 15, 85, 95, 99), 
                                        values = c("#692020", "#9A2F2F", "#D24141", 
                                                            "#D1D1D1", # light grey
                                                            "#00A800", "#007000", "#003700"))) |>
                                                              
      formatStyle(columns = c("payout"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = c(-1e-15, 1e-15), 
                                        values = c("#D24141", "#D1D1D1", "#00A800")))
    
  })
  
  
  # ============================================================================
  # Session Info
  # ============================================================================
  
  output$session_info <- renderPrint({
    sessionInfo()
  })
  
}


# ==============================================================================
# App
# ==============================================================================

shinyApp(ui, server)

