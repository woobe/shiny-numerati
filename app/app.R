library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)

library(DT)
library(plotly)
library(scico)
library(ggthemes)
library(scales)
library(stringr)
library(wesanderson)

library(data.table)
library(dtplyr)

library(parallel)

# devtools::install_github("woobe/Rnumerai")
library(Rnumerai)


# ==============================================================================
# Helper Functions
# ==============================================================================

# Download raw data
download_raw_data <- function(model_name) {
  
  # Download data from Numerai
  d_raw <- round_model_performances(model_name)
  
  # Remove rows without CORR
  d_raw <- d_raw[!is.na(d_raw$corrWMetamodel), ]
  
  
  # Add the model name
  d_raw$model <- model_name
  
  # Return
  return(as.data.table(d_raw))
  
}

# Reformat
reformat_data <- function(d_raw) {
  
  # Keep some columns only
  col_keep <- c("model", "roundNumber", 
                "roundOpenTime", "roundResolveTime",
                "roundResolved", "selectedStakeValue",
                "corr20V2", "corr20V2Percentile",
                "fncV3", "fncV3Percentile",
                "tc", "tcPercentile",
                "corrWMetamodel",
                "apcwnm", "mcwnm",
                "roundPayoutFactor", "payout")
  d_munged <- d_raw[, col_keep, with = FALSE]
  
  # Date
  d_munged[, roundOpenTime := as.Date(roundOpenTime)]
  d_munged[, roundResolveTime := as.Date(roundResolveTime)]
  
  # Reformat percentile
  d_munged[, corr20V2Percentile := round(corr20V2Percentile * 100, 6)]
  d_munged[, fncV3Percentile := round(fncV3Percentile * 100, 6)]
  d_munged[, tcPercentile := round(tcPercentile * 100, 6)]
  
  # Rename columns
  colnames(d_munged) <- c("model", "round", 
                          "date_open", "date_resolved",
                          "resolved", "stake",
                          "corrV2", "corrV2_pct",
                          "fncV3", "fncV3_pct",
                          "tc", "tc_pct", 
                          "corr_meta",
                          "apcwnm", "mcwnm",
                          "pay_ftr", "payout")
  
  # Return
  return(d_munged)
  
}

# Generate Colour Palette
gen_custom_palette <- function(ls_model) {
  
  # Extract info
  n_limit <- 5
  n_coluor <- length(unique(ls_model))
  n_pal_rep <- ceiling(n_coluor / n_limit)
  wes_pal_themes <- rep(c("Cavalcanti1", "Darjeeling1"), n_pal_rep)
  
  # Generate
  custom_palette <- c()
  for (n_pal in 1:n_pal_rep) {
    tmp_pal_name <- wes_pal_themes[n_pal]
    tmp_pal <- wesanderson::wes_palette(name = tmp_pal_name, n = n_limit, type = "continuous")
    custom_palette <- c(custom_palette, tmp_pal)
  }
  
  # Trim and return
  return(custom_palette[1:n_coluor])
  
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
  
  
  # ============================================================================
  # Sidebar
  # ============================================================================
  
  sidebar = shinydashboardPlus::dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      menuItem(text = "Start Here", tabName = "start", icon = icon("play")),
      menuItem(text = "Performance Summary", tabName = "performance", icon = icon("line-chart")), # icon("credit-card")
      menuItem(text = "Raw Data", tabName = "raw_data", icon = icon("download")),
      menuItem(text = "Community Events", tabName = "community", icon = icon("users")),
      menuItem(text = "About", tabName = "about", icon = icon("question-circle"))
    ), 
    minified = TRUE,
    collapsed = FALSE
  ),
  
  
  # ============================================================================
  # Main Body
  # ============================================================================
  
  body = dashboardBody(
    
    tabItems(
      
      # ========================================================================
      # Start Here
      # ========================================================================
      
      tabItem(tabName = "start", 
              
              fluidPage(
                
                # ==============================================================
                # Special script to keep the session alive for a bit longer
                # ==============================================================
                
                tags$head(
                  HTML(
                    "
                    <script>
                    var socket_timeout_interval
                    var n = 0
                    $(document).on('shiny:connected', function(event) {
                    socket_timeout_interval = setInterval(function(){
                    Shiny.onInputChange('count', n++)
                    }, 10000)
                    });
                    $(document).on('shiny:disconnected', function(event) {
                    clearInterval(socket_timeout_interval)
                    });
                    </script>
                    "
                  )
                ),
                
                # ==============================================================
                # First Page
                # ==============================================================
                
                markdown("# **Shiny Numerati**"),
                markdown("### Community Dashboard for the Numerai Classic Tournament"),
                
                br(),
                
                fluidRow(
                  
                  column(6, 
                         
                         markdown("## **Step 1: Select Your Models**"),
                         
                         markdown("### First, click this ⬇"),
                         
                         pickerInput(inputId = "model",
                                     label = " ",
                                     # choices = sort(Rnumerai::get_leaderboard()$username),
                                     choices = unique(c(sort(Rnumerai::get_leaderboard()$username)
                                                        # "joe_the_validator_01",
                                                        # "joe_the_validator_02",
                                                        # "joe_the_validator_03",
                                                        # "joe_the_validator_04",
                                                        # "joe_the_validator_05"
                                                        # "joe_the_hedgehog_01",
                                                        # "joe_the_hedgehog_02",
                                                        # "joe_the_hedgehog_03",
                                                        # "joe_the_hedgehog_04",
                                                        # "joe_the_hedgehog_05"
                                     )
                                     ),
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
                         
                         markdown("## **Step 2: Download Data**"),
                         
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
                
                h2(strong(textOutput(outputId = "text_next"))),
                h3(textOutput(outputId = "text_note")),
                
                br()
                
              )
      ),
      
      # ========================================================================
      # Payout Summary
      # ========================================================================
      
      tabItem(tabName = "performance", 
              
              fluidPage(
                
                markdown("# **Performance Summary**"),
                markdown("### Remember to refresh the charts after making changes to model selection or settings below."),
                markdown("### **NOTE**: the charts may take a while to render if you have selected a lot of models."),
                
                br(),
                
                fluidRow(
                  
                  column(6,
                         
                         markdown("## **Step 4: Adjust the Filter**"),
                         
                         sliderInput(inputId = "range_round", 
                                     label = "Numerai Classic Tournament Rounds",
                                     width = "100%",
                                     step = 1,
                                     min = 168, # first tournament round
                                     max = Rnumerai::get_current_round(), # note: daily payouts from round 474
                                     value = c(496, Rnumerai::get_current_round())
                         )
                  ),
                  
                  column(6, 
                         
                         markdown("## **Step 5: Generate Summary**"),
                         br(),
                         actionBttn(inputId = "button_filter", 
                                    label = "Generate / Refresh",
                                    color = "primary",
                                    icon = icon("refresh"),
                                    style = "gradient",
                                    block = TRUE)
                  )
                ), # end of fluidRow
                
                br(),
                
                tabsetPanel(type = "tabs",
                            
                            
                            tabPanel("KPI (C&T)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_performance_models"))),
                                     
                                     h4(textOutput(outputId = "text_performance_models_note")),
                                     
                                     br(),
                                     
                                     fluidRow(
                                       column(width = 6, plotlyOutput("plot_performance_avg")),
                                       column(width = 6, plotlyOutput("plot_performance_sharpe"))
                                     ),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     
                                     fluidRow(DTOutput("dt_performance_summary"),
                                              
                                              br(),
                                              
                                              markdown("#### **Notes**:
                                              
                                              - **avg_corrV2**: Average `CORRv2`
                                              - **sharpe_corrV2**: Sharpe Ratio of `CORRv2`
                                              
                                              - **avg_tc**: Average True Contribution (`TC`)
                                              - **sharpe_tc**: Sharpe Ratio of True Contribution (`TC`)
                                              
                                              - **avg_2C1T**: Average `2xCORRv2 + 1xTC`
                                              - **sharpe_2C1T**: Sharpe Ratio of `2xCORRv2 + 1xTC`
                                              
                                              "),
                                              
                                              br()
                                     ),
                                     
                                     
                                     br()
                                     
                            ),
                            
                            # Coming soon
                            tabPanel("KPI (All)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_performance_chart"))),
                                     
                                     h4(textOutput(outputId = "text_performance_chart_note")),
                                     
                                     br(),
                                     
                                     # Controls
                                     
                                     fluidRow(
                                       
                                       column(6, 
                                              markdown("#### **Pick ONE of the KPIs:**"),
                                              pickerInput(
                                                inputId = "kpi_choice",
                                                choices = c("CORRv2: CORRelation with target cyrus_v4_20",
                                                            "TC: True Contribtuion to the hedge fund's returns",
                                                            "FNCv3: Feature Neutral Correlation with respect to the FNCv3 features",
                                                            # "CORJ60: CORRelation with target Jerome_v4_60", # add this later
                                                            
                                                            "CWMM: Correlation With the Meta Model",
                                                            "MCWNM: Maximum Correlation With Numerai Models staked at least 10 NMR",
                                                            "APCWNM: Average Pairwise Correlation With Numerai Models staked at least 10 NMR",
                                                            
                                                            "Score Multipliers: 0.5 x CORRv2",
                                                            "Score Multipliers: 1.5 x CORRv2",
                                                            "Score Multipliers: 2.0 x CORRv2",
                                                            "Score Multipliers: 2.0 x CORRv2 + 0.5 x TC",
                                                            "Score Multipliers: 2.0 x CORRv2 + 1.0 x TC",
                                                            
                                                            "Percentile: CORRv2",
                                                            "Percentile: TC",
                                                            "Percentile: FNCv3",
                                                            
                                                            "Payout",
                                                            "Rate of Return (%): Payout / Stake x 100"),
                                                multiple = FALSE,
                                                width = "95%")
                                       ),
                                       
                                       column(2,
                                              markdown("#### **Cumulative**"),
                                              switchInput(
                                                inputId = "kpi_cumulative",
                                                onLabel = "Yes",
                                                offLabel = "No",
                                                value = TRUE)
                                       ),
                                       
                                       column(2,
                                              markdown("#### **Hide Pending**"),
                                              switchInput(
                                                inputId = "kpi_hide_pending",
                                                onLabel = "Yes",
                                                offLabel = "No",
                                                value = FALSE)
                                       ),
                                       
                                       column(2,
                                              markdown("#### **Facet**"),
                                              switchInput(
                                                inputId = "kpi_facet",
                                                onLabel = "Yes",
                                                offLabel = "No",
                                                value = FALSE)
                                       )
                                       
                                       
                                     ),

                                     h4(strong(textOutput(outputId = "text_performance_chart_data"))),
                                     
                                     br(),
                                     
                                     DTOutput("dt_kpi"),
                                     
                                     br(),
                                     br(),
                                     
                                     h4(strong(textOutput(outputId = "text_performance_chart_title"))),
                                     
                                     fluidRow(column(12, plotlyOutput("plot_kpi"))),
                                     
                                     br()
                                     
                            ),
                            
                            
                            
                            tabPanel("Payout (Overview)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout_overview"))),
                                     
                                     br(),
                                     
                                     fluidRow(
                                       class = "text-center",
                                       
                                       valueBoxOutput("payout_n_round_resolved", width = 3),
                                       valueBoxOutput("payout_resolved", width = 3),
                                       valueBoxOutput("payout_average_resolved", width = 3),
                                       valueBoxOutput("payout_avg_ror_resolved", width = 3),
                                       
                                       valueBoxOutput("payout_n_round_pending", width = 3),
                                       valueBoxOutput("payout_pending", width = 3),
                                       valueBoxOutput("payout_average_pending", width = 3),
                                       valueBoxOutput("payout_avg_ror_pending", width = 3),
                                       
                                       valueBoxOutput("payout_n_round", width = 3),
                                       valueBoxOutput("payout_total", width = 3),
                                       valueBoxOutput("payout_average", width = 3),
                                       valueBoxOutput("payout_avg_ror", width = 3)
                                     ),
                                     
                                     br(),
                                     
                                     shinycssloaders::withSpinner(plotlyOutput("plot_payout_net")),
                                     
                                     br()
                                     
                            ),
                            
                            
                            tabPanel("Payout (Rounds)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout_rnd"))),
                                     
                                     br(),
                                     
                                     DTOutput("dt_payout_summary"),
                                     
                                     br()
                                     
                            ),
                            
                            
                            tabPanel("Payout (Models)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout_ind"))),
                                     
                                     br(),
                                     
                                     DTOutput("dt_model_payout_summary"),
                                     
                                     br()
                                     
                            ),
                            
                            
                            tabPanel("Payout (Sim)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout_sim"))),
                                     
                                     br(),
                                     
                                     markdown("![new_tc_change](https://i.ibb.co/XjKwtzr/screenshot-2023-10-05-at-10.png)"),
                                     
                                     br(),
                                     
                                     markdown("#### **Notes**:
                                              
                                              - **sum_pay**: Sum of Payouts
                                              - **shp_pay**: Sharpe Ratio of Payouts
                                              - **1C0T**: 1xCORRv2 + 0xTC
                                              - **2C0T**: 2xCORRv2 + 0xTC (New Payout Mode)
                                              - **2C1T**: 2xCORRv2 + 1xTC (New Payout Mode)
                                              - **1C3T**: 1xCORRv2 + 3xTC (Original Degen Mode)
                                              
                                              "),
                                     
                                     br(),
                                     
                                     markdown("### **Payout Simulation (Overall)**"),
                                     
                                     DTOutput("dt_payout_sim_overall"),
                                     
                                     br(),
                                     
                                     br(),
                                     
                                     markdown("### **Payout Simulation (Individual Models)**"),
                                     
                                     br(),
                                     
                                     DTOutput("dt_payout_sim_model"),
                                     
                                     br()
                                     
                            ),
                            
                            tabPanel("Payout Chart (Rounds)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout_all_models"))),
                                     
                                     br(),
                                     
                                     shinycssloaders::withSpinner(plotlyOutput("plot_payout_stacked")),
                                     
                                     br()
                                     
                            ),
                            
                            tabPanel("Payout Chart (Models)",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout_ind_models"))),
                                     
                                     br(),
                                     
                                     shinycssloaders::withSpinner(plotlyOutput("plot_payout_individual")),
                                     
                                     br()
                            )
                            
                ) # end of tabsetPanel
                
              ) # end of fluidPage
              
      ),
      
      
      # ========================================================================
      # Raw Data
      # ========================================================================
      
      tabItem(tabName = "raw_data", 
              
              markdown("# **Download Raw Data**"),
              markdown("### Wanna run your own analysis? No problem."),
              markdown("### Remember to select your model(s) first."),
              br(),
              fluidRow(
                column(6,
                       downloadBttn(outputId = "download_raw", 
                                    label = "Download Raw Data CSV", 
                                    icon = icon("cloud-download"),
                                    style = "gradient",
                                    block = T)
                )
              )
      ),
      
      
      # ========================================================================
      # Community
      # ========================================================================
      
      tabItem(tabName = "community",
              fluidRow(
                column(10,
                       htmltools::includeMarkdown('https://raw.githubusercontent.com/councilofelders/meetups/master/README.md')
                )
              )
              # markdown("![image](https://media.giphy.com/media/cftSzNoCTfSyAWctcl/giphy.gif)")
      ),
      
      
      # ========================================================================
      # About
      # ========================================================================
      
      tabItem(tabName = "about", 
              
              markdown("# **About this App**"),
              markdown('### Yet another Numerai community dashboard by <b><a href="https://linktr.ee/jofaichow" target="_blank">Jo-fai Chow</a></b>.'),
              
              br(),
              markdown("## **Acknowledgements**"),
              markdown("- #### This hobby project was inspired by Rajiv's <b><a href='https://huggingface.co/spaces/rajistics/shiny-kmeans' target='_blank'>shiny-kmeans</a></b> on 🤗 Spaces."),
              markdown('- #### The <b><a href="https://linktr.ee/jofaichow" target="_blank">Rnumerai</a></b> package from Omni Analytics Group.'),
              
              br(),
              markdown("## **Source Code**"),
              markdown("- #### GitHub: https://github.com/woobe/shiny-numerati"),
              markdown("- #### Hugging Face: https://huggingface.co/spaces/jofaichow/shiny-numerati/tree/main"),
              
              br(),
              markdown("## **Changelog**"),
              markdown(
                "
                - #### **0.1.0** — First prototype with an interactive table output
                - #### **0.1.1** — Added a functional `Payout Summary` page
                - #### **0.1.2** — `Payout Summary` layout updates
                - #### **0.1.3** — Added `Raw Data`
                - #### **0.1.4** — Various improvements in `Payout Summary`
                - #### **0.1.5** — Replaced `corrV1` with `corrV2`
                - #### **0.1.6** — Added `apcwnm` and `mcwnm`
                - #### **0.1.7** — Added CoE Meetup GitHub page to `Community`
                - #### **0.1.8** — Various improvements in `Payout Summary`
                - #### **0.1.9** — Added `Payout Sim` based on new Corr and TC multipier settings
                - #### **0.2.0** — Replaced `Payout Summary` with `Performance Summary`. Added KPIs summary
                - #### **0.2.1** — Added `KPI (All)`
                - #### **0.2.2** — Sped up chart rendering with `toWebGL()`
                "),
              
              br(),
              markdown("## **Session Info**"),
              verbatimTextOutput(outputId = "session_info"),
              
              br(),
              textOutput("keepAlive") # trick to keep session alive
      )
      
      # ========================================================================
      
    ) # end of tabItems
    
  ),
  
  footer = shinydashboardPlus::dashboardFooter(
    left = "Powered by ❤️, ☕, Shiny, and 🤗 Spaces",
    right = paste0("Version 0.2.2"))
  
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
  # Reactive: Data
  # ============================================================================
  
  react_ls_model <- eventReactive(input$button_download, {sort(input$model)})
  
  output$print_download <- renderPrint({react_ls_model()})
  
  output$text_download <- renderText({
    if (length(react_ls_model()) >= 1) "Your Selection:" else " "
  })
  
  output$text_preview <- renderText({
    if (length(react_ls_model()) >= 1) "Data Preview:" else " "
  })
  
  output$text_next <- renderText({
    if (length(react_ls_model()) >= 1) "Step 3: Performance Summary (see ←)" else " "
  })
  
  output$text_note <- renderText({
    if (length(react_ls_model()) >= 1) "Note: you can also download [Raw Data] and check out our [Community Events] (see ←)" else " "
  })
  
  react_d_raw <- eventReactive(
    input$button_download,
    {
      
      # Parallelised download
      d_raw <- rbindlist(mclapply(X = input$model, 
                                  FUN = download_raw_data, 
                                  mc.cores = detectCores()))
      
      # Return
      d_raw
      
    }
  )
  
  
  
  # ============================================================================
  # Reactive: DataTable
  # ============================================================================
  
  
  output$dt_model <- DT::renderDT({
    
    # Raw Data
    d_raw <- react_d_raw()
    
    # Reformat
    d_munged <- reformat_data(d_raw)
    
    # Main DT
    DT::datatable(
      
      # Data
      d_munged,
      
      # Other Options
      rownames = FALSE,
      extensions = "Buttons",
      options =
        list(
          dom = 'Bflrtip', # https://datatables.net/reference/option/dom
          buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          order = list(list(0, 'asc'), list(1, 'asc')),
          pageLength = 10,
          lengthMenu = c(10, 20, 100, 500, 1000, 50000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("corrV2", "tc", "fncV3", "corr_meta", "pay_ftr"), digits = 4) |>
      formatRound(columns = c("apcwnm", "mcwnm"), digits = 4) |>
      formatRound(columns = c("corrV2_pct", "tc_pct", "fncV3_pct"), digits = 1) |>
      formatRound(columns = c("stake", "payout"), digits = 2) |>
      
      formatStyle(columns = c("model"),
                  fontWeight = "bold") |>
      
      formatStyle(columns = c("stake"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "#2196F3"))) |>
      
      formatStyle(columns = c("corrV2", "fncV3"),
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "black"))) |>
      
      formatStyle(columns = c("tc"),
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "#A278DC"))) |>
      
      formatStyle(columns = c("corrV2_pct", "tc_pct", "fncV3_pct"),
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
  # Reactive: filtering data for all charts
  # ============================================================================
  
  react_d_filter <- eventReactive(
    input$button_filter,
    {
      
      # Reformat and Filter
      d_filter <- reformat_data(react_d_raw())
      d_filter <- d_filter[round >= input$range_round[1], ]
      d_filter <- d_filter[round <= input$range_round[2], ]
      
      # Return
      d_filter
      
    })
  
  
  react_d_payout_summary <- eventReactive(
    input$button_filter,
    {
      
      # Summarise payout
      d_smry <- 
        react_d_filter() |> 
        lazy_dt() |>
        filter(pay_ftr > 0) |>
        filter(stake > 0) |>
        group_by(round, date_open, date_resolved, resolved) |>
        summarise(staked_models = n(),
                  total_stake = sum(stake, na.rm = T),
                  net_payout = sum(payout, na.rm = T)) |>
        as.data.table()
      d_smry$rate_of_return <- (d_smry$net_payout / d_smry$total_stake) * 100
      
      # Return
      d_smry
      
    })
  
  
  react_d_model_payout_summary <- eventReactive(
    input$button_filter,
    {
      
      # Get filtered data
      # d_smry <- as.data.table(react_d_filter() |> filter(pay_ftr > 0))
      d_smry <- 
        react_d_filter() |> 
        lazy_dt() |>
        filter(pay_ftr > 0) |>
        filter(stake > 0) |>
        as.data.table()
      
      # Calculate rate of return (%)
      d_smry[, rate_of_return_percent := payout / stake * 100]
      
      # Summarise
      d_smry <- 
        d_smry |> 
        lazy_dt() |> 
        group_by(model) |>
        summarise(staked_rounds = n(),
                  net_payout = sum(payout, na.rm = T),
                  avg_payout = mean(payout, na.rm = T),
                  avg_rate_of_return_percent = mean(rate_of_return_percent, na.rm = T),
                  sharpe_rate_of_return = mean(rate_of_return_percent, na.rm = T) / sd(rate_of_return_percent, na.rm = T)
        ) |> as.data.table()
      
      # Return
      d_smry
      
    })
  
  
  react_d_payout_sim_model <- eventReactive(
    input$button_filter,
    {
      
      # Get filtered data
      # d_smry <- as.data.table(react_d_filter() |> filter(pay_ftr > 0))
      d_payout <- 
        react_d_filter() |> 
        lazy_dt() |>
        filter(pay_ftr > 0) |>
        filter(stake > 0) |>
        as.data.table()
      
      # Apply clip to corrV2
      d_payout[, corrV2_final := corrV2]
      d_payout[corrV2 > 0.25, corrV2_final := 0.25]
      d_payout[corrV2 < -0.25, corrV2_final := -0.25]
      
      # Apply clip to tc
      d_payout[, tc_final := tc]
      d_payout[tc > 0.25, tc_final := 0.25]
      d_payout[tc < -0.25, tc_final := -0.25]
      
      # Calculate different payout
      d_payout[, payout_1C0T := (corrV2_final) * stake * pay_ftr]
      d_payout[, payout_2C0T := (2*corrV2_final) * stake * pay_ftr]
      d_payout[, payout_2C1T := (2*corrV2_final + tc_final) * stake * pay_ftr]
      d_payout[, payout_1C3T := (corrV2_final + 3*tc_final) * stake * pay_ftr]
      
      # Summarise
      d_payout_smry <-
        d_payout |>
        lazy_dt() |>
        group_by(model) |>
        summarise(
          rounds = n(),
          
          sum_pay_1C0T = sum(payout_1C0T, na.rm = T),
          sum_pay_2C0T = sum(payout_2C0T, na.rm = T),
          sum_pay_2C1T = sum(payout_2C1T, na.rm = T),
          sum_pay_1C3T = sum(payout_1C3T, na.rm = T),
          
          shp_pay_1C0T = mean(payout_1C0T, na.rm = T) / sd(payout_1C0T, na.rm = T),
          shp_pay_2C0T = mean(payout_2C0T, na.rm = T) / sd(payout_2C0T, na.rm = T),
          shp_pay_2C1T = mean(payout_2C1T, na.rm = T) / sd(payout_2C1T, na.rm = T),
          shp_pay_1C3T = mean(payout_1C3T, na.rm = T) / sd(payout_1C3T, na.rm = T)
          
        ) |>
        as.data.table()
      
      # Return
      d_payout_smry
      
    })
  
  
  react_d_payout_sim_overall <- eventReactive(
    input$button_filter,
    {
      
      # Get filtered data
      # d_payout <- as.data.table(react_d_filter() |> filter(pay_ftr > 0))
      d_payout <- 
        react_d_filter() |> 
        lazy_dt() |>
        filter(pay_ftr > 0) |>
        filter(stake > 0) |>
        as.data.table()
      
      # Apply clip to corrV2
      d_payout[, corrV2_final := corrV2]
      d_payout[corrV2 > 0.25, corrV2_final := 0.25]
      d_payout[corrV2 < -0.25, corrV2_final := -0.25]
      
      # Apply clip to tc
      d_payout[, tc_final := tc]
      d_payout[tc > 0.25, tc_final := 0.25]
      d_payout[tc < -0.25, tc_final := -0.25]
      
      # Calculate different payout
      d_payout[, payout_1C0T := (corrV2_final) * stake * pay_ftr]
      d_payout[, payout_2C0T := (2*corrV2_final) * stake * pay_ftr]
      d_payout[, payout_2C1T := (2*corrV2_final + tc_final) * stake * pay_ftr]
      d_payout[, payout_1C3T := (corrV2_final + 3*tc_final) * stake * pay_ftr]
      
      # Summarise
      d_payout_smry <-
        d_payout |>
        lazy_dt() |>
        summarise(
          
          sum_pay_1C0T = sum(payout_1C0T, na.rm = T),
          sum_pay_2C0T = sum(payout_2C0T, na.rm = T),
          sum_pay_2C1T = sum(payout_2C1T, na.rm = T),
          sum_pay_1C3T = sum(payout_1C3T, na.rm = T),
          
          shp_pay_1C0T = mean(payout_1C0T, na.rm = T) / sd(payout_1C0T, na.rm = T),
          shp_pay_2C0T = mean(payout_2C0T, na.rm = T) / sd(payout_2C0T, na.rm = T),
          shp_pay_2C1T = mean(payout_2C1T, na.rm = T) / sd(payout_2C1T, na.rm = T),
          shp_pay_1C3T = mean(payout_1C3T, na.rm = T) / sd(payout_1C3T, na.rm = T)
          
        ) |>
        as.data.table()
      
      # Return
      d_payout_smry
      
    })
  
  
  react_d_performance_summary <- eventReactive(
    input$button_filter,
    {
      
      # Get filtered data
      d_pref <- as.data.table(react_d_filter())
      
      # Add 2xCORRv2 + 1xTC
      d_pref[, twoC_oneT := 2*corrV2 + tc]
      
      # Calculate some high level stats
      d_pref <- 
        d_pref |>
        lazy_dt() |>
        group_by(model) |>
        summarise(total_rounds = n(),
                  
                  avg_corrV2 = mean(corrV2, na.rm = T),
                  sharpe_corrV2 = mean(corrV2, na.rm = T) / sd(corrV2, na.rm = T),
                  # mdd_corrV2 = maxdrawdown(corrV2),
                  
                  avg_tc = mean(tc, na.rm = T),
                  sharpe_tc = mean(tc, na.rm = T) / sd(tc, na.rm = T),
                  # mdd_tc = maxdrawdown(tc),
                  
                  avg_2C1T = mean(twoC_oneT, na.rm = T),
                  sharpe_2C1T = mean(twoC_oneT, na.rm = T) / sd(tc, na.rm = T)
                  # mdd_2C1T = maxdrawdown(twoC_oneT)
                  
        ) |> as.data.table()
      
      # Return
      d_pref
      
    })
  
  
  react_d_kpi <- eventReactive(
    input$button_filter,
    {
      
      # Get filtered data
      d_pref <- as.data.table(react_d_filter())
      
      # Hide Pending?
      if (input$kpi_hide_pending) d_pref <- d_pref[resolved == TRUE]
      
      # Add Rate of Return
      d_pref[stake >0, rate_of_return := payout / stake * 100]
      
      # Extract Raw KPI
      if (input$kpi_choice == "CORRv2: CORRelation with target cyrus_v4_20") d_pref[, KPI := corrV2]
      if (input$kpi_choice == "TC: True Contribtuion to the hedge fund's returns") d_pref[, KPI := tc]
      if (input$kpi_choice == "FNCv3: Feature Neutral Correlation with respect to the FNCv3 features") d_pref[, KPI := fncV3]
      if (input$kpi_choice == "CWMM: Correlation With the Meta Model") d_pref[, KPI := corr_meta]
      if (input$kpi_choice == "MCWNM: Maximum Correlation With Numerai Models staked at least 10 NMR") d_pref[, KPI := mcwnm]
      if (input$kpi_choice == "APCWNM: Average Pairwise Correlation With Numerai Models staked at least 10 NMR") d_pref[, KPI := apcwnm]
      
      # Calculate Score Multiplies
      if (input$kpi_choice == "Score Multipliers: 0.5 x CORRv2") d_pref[, KPI := 0.5 * corrV2]
      if (input$kpi_choice == "Score Multipliers: 1.5 x CORRv2") d_pref[, KPI := 1.5 * corrV2]
      if (input$kpi_choice == "Score Multipliers: 2.0 x CORRv2") d_pref[, KPI := 2.0 * corrV2]
      if (input$kpi_choice == "Score Multipliers: 2.0 x CORRv2 + 0.5 x TC") d_pref[, KPI := 2.0 * corrV2 + 0.5 * tc]
      if (input$kpi_choice == "Score Multipliers: 2.0 x CORRv2 + 1.0 x TC") d_pref[, KPI := 2.0 * corrV2 + 1.0 * tc]
      
      # Extract Percentile
      if (input$kpi_choice == "Percentile: CORRv2") d_pref[, KPI := corrV2_pct]
      if (input$kpi_choice == "Percentile: TC") d_pref[, KPI := tc_pct]
      if (input$kpi_choice == "Percentile: FNCv3") d_pref[, KPI := fncV3_pct]
      
      # Extract Payout info
      if (input$kpi_choice == "Payout") d_pref[, KPI := payout]
      if (input$kpi_choice == "Rate of Return (%): Payout / Stake x 100") d_pref[, KPI := rate_of_return]
      
      # Remove rows with NA (quick hack for now)
      d_pref <- d_pref[!is.na(KPI)]
      
      # Calculate Cumulative KPI (if needed) 
      if (input$kpi_cumulative) {
        
        # Sort before doing cumsum
        setorderv(d_pref, c("model", "round"))
        
        # The data.table way
        d_pref[, KPI := cumsum(KPI), "model"]
        
      }
      
      # Trim and sort
      d_trim <- d_pref[, c("model", "round", "date_resolved", "resolved", "KPI")]
      setorderv(d_trim, c("model", "round"))
      
      # Return
      d_trim
      
    })
  
  
  
  
  # ============================================================================
  # Reactive: Payout Value Boxes
  # ============================================================================
  
  output$text_payout_overview <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payout Summary (Overview)" else " "
  })
  
  output$text_payout_rnd <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payout Summary (Tournament Rounds)" else " "
  })
  
  output$text_payout_ind <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payout Summary (Individual Models)" else " "
  })
  
  output$text_payout_all_models <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payout Summary Chart (All Models - Stacked)" else " "
  })
  
  output$text_payout_ind_models <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payout Summary Chart (Individual Models)" else " "
  })
  
  output$text_payout_sim <- renderText({
    if (nrow(react_d_filter()) >= 1) "New Payout Simulation (NOTE: Experimental!)" else " "
  })
  
  output$text_performance_models <- renderText({
    if (nrow(react_d_filter()) >= 1) "KPI Analysis (CORRv2 and TC)" else " "
  })
  
  output$text_performance_models_note <- renderText({
    if (nrow(react_d_filter()) >= 1) "NOTE: You may want to find out which models have high CORRv2 Sharpe and high TC Sharpe." else " "
  })
  
  output$text_performance_chart <- renderText({
    if (nrow(react_d_filter()) >= 1) "KPI Analysis (Model Comparison)" else " "
  })
  
  output$text_performance_chart_note <- renderText({
    if (nrow(react_d_filter()) >= 1) "NOTE: Remember to refresh the chart (Step 5 ↑) after making any changes." else " "
  })
  
  output$text_performance_chart_title <- renderText({
    if (nrow(react_d_filter()) >= 1) "KPI Chart (Remember to Click the 'Refresh' Button ↑↑↑)" else " "
  })
  
  output$text_performance_chart_data <- renderText({
    if (nrow(react_d_filter()) >= 1) "KPI Data" else " "
  })
  
  
  # ============================================================================
  # Reactive valueBox outputs: Rounds 
  # ============================================================================
  
  
  output$payout_n_round_resolved <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = nrow(react_d_payout_summary()[resolved == TRUE & total_stake > 0, ]),
             subtitle = "Staked Rounds (Resolved)",
             color = "olive")
  })
  
  
  output$payout_n_round_pending <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = nrow(react_d_payout_summary()[resolved == FALSE & total_stake > 0, ]),
             subtitle = "Staked Rounds (Pending)",
             color = "yellow")
  })
  
  
  output$payout_n_round <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = nrow(react_d_payout_summary()[total_stake > 0, ]),
             subtitle = "Staked Rounds (All)",
             color = "light-blue")
  })
  
  
  # ============================================================================
  # Reactive valueBox outputs: Payouts
  # ============================================================================
  
  
  output$payout_resolved <- renderValueBox({
    valueBox(value = paste(as.character(format(round(sum(react_d_filter()[resolved == T, ]$payout, na.rm = T), 2), nsmall = 2)), "NMR"),
             subtitle = "Total Payout (Resolved)",
             color = "olive")
  })
  
  
  output$payout_pending <- renderValueBox({
    valueBox(value = paste(as.character(format(round(sum(react_d_filter()[resolved == F, ]$payout, na.rm = T), 2), nsmall = 2)), "NMR"),
             subtitle = "Total Payout (Pending)",
             color = "yellow")
  })
  
  
  output$payout_total <- renderValueBox({
    valueBox(value = paste(as.character(format(round(sum(react_d_filter()$payout, na.rm = T), 2), nsmall = 2)), "NMR"),
             subtitle = "Total Payout (All)",
             color = "light-blue")
  })
  
  
  # ============================================================================
  # Reactive valueBox outputs: Average Round Payouts
  # ============================================================================
  
  
  output$payout_average_resolved <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = paste(as.character(format(round(mean(react_d_payout_summary()[resolved == T & total_stake > 0, ]$net_payout, na.rm = T), 2), nsmall = 2)), "NMR"),
             subtitle = "Avg. Round Payout (Resolved)",
             color = "olive")
  })
  
  
  output$payout_average_pending <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = paste(as.character(format(round(mean(react_d_payout_summary()[resolved == F & total_stake > 0, ]$net_payout, na.rm = T), 2), nsmall = 2)), "NMR"),
             subtitle = "Avg. Round Payout (Pending)",
             color = "yellow")
  })
  
  
  output$payout_average <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = paste(as.character(format(round(mean(react_d_payout_summary()[total_stake > 0, ]$net_payout, na.rm = T), 2), nsmall = 2)), "NMR"),
             subtitle = "Avg. Round Payout (All)",
             color = "light-blue")
  })
  
  
  # ============================================================================
  # Reactive valueBox outputs: Average Rate of Return
  # ============================================================================
  
  
  output$payout_avg_ror_resolved <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = paste(as.character(format(round(mean(react_d_payout_summary()[resolved == T & total_stake > 0, ]$rate_of_return), 2), nsmall = 2)), "%"),
             subtitle = "Avg. Round ROR (Resolved)",
             color = "olive")
  })
  
  
  output$payout_avg_ror_pending <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = paste(as.character(format(round(mean(react_d_payout_summary()[resolved == F & total_stake > 0, ]$rate_of_return), 2), nsmall = 2)), "%"),
             subtitle = "Avg. Round ROR (Pending)",
             color = "yellow")
  })
  
  
  output$payout_avg_ror <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = paste(as.character(format(round(mean(react_d_payout_summary()[total_stake > 0, ]$rate_of_return), 2), nsmall = 2)), "%"),
             subtitle = "Avg. Round ROR (All)",
             color = "light-blue")
  })
  
  
  # ============================================================================
  # Reactive: Payout Charts
  # ============================================================================
  
  
  # Net Payouts  Bar Chart
  output$plot_payout_net <- renderPlotly({
    
    # Data
    d_filter <- react_d_payout_summary()
    
    # Filter
    d_filter <- d_filter[total_stake > 0]
    
    # Divider (resolved vs pending)
    x_marker <- max(d_filter[resolved == TRUE]$round) + 0.5
    y_marker <- max(d_filter$net_payout) 
    
    # ggplot
    p <- ggplot(d_filter, 
                aes(x = round, y = net_payout, fill = net_payout, 
                    text = paste("Round:", round,
                                 "\nRound Open Date:", date_open,
                                 "\nRound Resolved Date:", date_resolved,
                                 "\nRound Resolved?:", resolved,
                                 "\nPayout:", round(net_payout,2), "NMR"))) +
      
      geom_bar(position = "stack", stat = "identity") +
      theme(
        panel.border = element_rect(fill = 'transparent', 
                                    color = "grey", linewidth = 0.25),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(),
        strip.clip = "on",
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent')
      ) +
      
      geom_vline(aes(xintercept = x_marker), linewidth = 0.25, color = "grey", linetype = "dashed") +
      geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey") +
      
      annotate("text", x = x_marker, y = y_marker*1.2, label = "← Resolved vs. Pending →") +
      
      scale_fill_scico(palette = "vikO", direction = -1, midpoint = 0) +
      # scale_x_date(breaks = breaks_pretty(10),
      #              labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "\n")
      # ) +
      xlab("\nTournament Round") +
      ylab("Round Payout (NMR)")
    
    # Generate plotly
    ggplotly(p, height = 500, tooltip = "text") |> toWebGL()
    
  })
  
  
  # Stacked Bar Chart
  output$plot_payout_stacked <- renderPlotly({
    
    # Data
    d_filter <- react_d_filter()
    
    # Filter
    d_filter <- d_filter[stake > 0]
    
    # Divider (resolved vs pending)
    x_marker <- max(d_filter[resolved == TRUE]$round) + 0.5
    
    # ggplot
    p <- ggplot(d_filter, 
                aes(x = round, y = payout, fill = payout, 
                    text = paste("Model:", model, 
                                 "\nRound:", round,
                                 "\nRound Open Date:", date_open,
                                 "\nRound Resolved Date:", date_resolved,
                                 "\nRound Resolved?:", resolved,
                                 "\nPayout:", round(payout,2), "NMR"))) +
      geom_bar(position = "stack", stat = "identity") +
      theme(
        panel.border = element_rect(fill = 'transparent', 
                                    color = "grey", linewidth = 0.25),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(),
        strip.clip = "on",
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent')
      ) +
      geom_vline(aes(xintercept = x_marker), linewidth = 0.25, color = "grey", linetype = "dashed") +
      geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey") +
      scale_fill_scico(palette = "vikO", direction = -1, midpoint = 0) +
      # scale_x_date(breaks = breaks_pretty(10),
      #              labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "\n")
      # ) +
      xlab("\nTournament Round") +
      ylab("Round Payout (NMR)")
    
    # Generate plotly
    ggplotly(p, height = 500, tooltip = "text") |> toWebGL()
    
  })
  
  
  # Individual
  output$plot_payout_individual <- renderPlotly({
    
    # Data
    d_filter <- react_d_filter()
    
    # Filter
    d_filter <- d_filter[stake > 0]
    
    # Get the number of unique models
    n_model <- length(unique(d_filter$model))
    
    # Base plot
    p <- ggplot(d_filter, 
                aes(x = round, y = payout, fill = payout, 
                    text = paste("Model:", model, 
                                 "\nRound:", round,
                                 "\nRound Open Date:", date_open,
                                 "\nRound Resolved Date:", date_resolved,
                                 "\nRound Resolved:", resolved,
                                 "\nPayout:", round(payout,2), "NMR"))) +
      geom_bar(stat = "identity") +
      theme(
        panel.border = element_rect(fill = 'transparent', color = "grey", linewidth = 0.25),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(),
        strip.clip = "on",
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey") +
      scale_fill_scico(palette = "vikO", direction = -1, midpoint = 0) +
      scale_x_continuous(breaks = breaks_pretty(5)) +
      xlab("\nTournament Round") +
      ylab("Payout (NMR)")
    
    # Facet setting
    # if ((n_model %% 4) == 0) {
    #   p <- p + facet_wrap(. ~ model, ncol = 4, scales = "fixed")
    # } else if ((n_model %% 5) == 0) {
    #   p <- p + facet_wrap(. ~ model, ncol = 5, scales = "fixed")
    # } else {
    #   p <- p + facet_wrap(. ~ model, ncol = 6, scales = "fixed")
    # }
    p <- p + facet_wrap(. ~ model, ncol = 5, scales = "fixed") # fixed
    
    # Dynamic height adjustment
    height <- 500 # default minimum height
    if (n_model >= 10) height = 800
    if (n_model >= 15) height = 1000
    if (n_model >= 20) height = 1200
    if (n_model >= 25) height = 1400
    if (n_model >= 30) height = 1600
    if (n_model >= 35) height = 1800
    if (n_model >= 40) height = 2000
    if (n_model >= 45) height = 2200
    if (n_model >= 50) height = 2400
    if (n_model >= 55) height = 2600
    if (n_model >= 60) height = 2800
    if (n_model >= 65) height = 3000
    
    # Generate plotly
    ggplotly(p, height = height, tooltip = "text") |> toWebGL()
    
  })
  
  
  # KPI Chart: Avg Corr vs. Avg TC
  output$plot_performance_avg <- renderPlotly({
    
    # Data
    d_pref <- react_d_performance_summary()
    
    # Plot
    p_avg <- ggplot(d_pref, 
                    aes(x = avg_tc, y = avg_corrV2,
                        text = paste("Model:", model, 
                                     "\nAverage CORRv2:", round(avg_corrV2, 4),
                                     "\nAverage TC:", round(avg_tc, 4))
                    )) +
      geom_point() +
      theme(
        panel.border = element_rect(fill = 'transparent', color = "grey", linewidth = 0.25),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(),
        strip.clip = "on",
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(breaks = breaks_pretty(5)) +
      scale_y_continuous(breaks = breaks_pretty(5)) +
      xlab("\nAverage TC") +
      ylab("\nAverage CORRv2")
    
    # Add vline and hline if needed
    if (min(d_pref$avg_corrV2) <0) p_avg <- p_avg + geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey", linetype = "dashed")
    if (min(d_pref$avg_tc) <0) p_avg <- p_avg + geom_vline(aes(xintercept = 0), linewidth = 0.25, color = "grey", linetype = "dashed")
    
    # Convert to Plotly
    ggplotly(p_avg, tooltip = "text") |> toWebGL()
    
  })
  
  
  # KPI Chart: Corr Sharpe vs. TC Sharpe
  output$plot_performance_sharpe <- renderPlotly({
    
    # Data
    d_pref <- react_d_performance_summary()
    
    # Plot
    p_sharpe <- ggplot(d_pref, 
                       aes(x = sharpe_tc, y = sharpe_corrV2,
                           text = paste("Model:", model, 
                                        "\nSharpe Ratio of CORRv2:", round(sharpe_corrV2, 4),
                                        "\nSharpe Ratio of TC:", round(sharpe_tc, 4))
                       )) +
      geom_point() +
      theme(
        panel.border = element_rect(fill = 'transparent', color = "grey", linewidth = 0.25),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(),
        strip.clip = "on",
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(breaks = breaks_pretty(5)) +
      scale_y_continuous(breaks = breaks_pretty(5)) +
      xlab("\nSharpe Ratio of TC") +
      ylab("\nSharpe Ratio of CORRv2")
    
    # Add vline and hline if needed
    if (min(d_pref$sharpe_corrV2) <0) p_sharpe <- p_sharpe + geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey", linetype = "dashed")
    if (min(d_pref$sharpe_tc) <0) p_sharpe <- p_sharpe + geom_vline(aes(xintercept = 0), linewidth = 0.25, color = "grey", linetype = "dashed")
    
    # Convert to Plotly
    ggplotly(p_sharpe, tooltip = "text")  |> toWebGL()
    
  })
  
  
  # KPI Chart: All KPIs
  output$plot_kpi <- renderPlotly({
    
    # Data
    d_kpi <- react_d_kpi()
    
    # Dynamic Labels
    if (input$kpi_choice == "CORRv2: CORRelation with target cyrus_v4_20") y_label <- "CORRv2"
    if (input$kpi_choice == "TC: True Contribtuion to the hedge fund's returns") y_label <- "TC"
    if (input$kpi_choice == "FNCv3: Feature Neutral Correlation with respect to the FNCv3 features") y_label <- "FNCv3"
    if (input$kpi_choice == "CWMM: Correlation With the Meta Model") y_label <- "CWMM"
    if (input$kpi_choice == "MCWNM: Maximum Correlation With Numerai Models staked at least 10 NMR") y_label <- "MCWNM"
    if (input$kpi_choice == "APCWNM: Average Pairwise Correlation With Numerai Models staked at least 10 NMR") y_label <- "APCWNM"
    
    if (input$kpi_choice == "Score Multipliers: 0.5 x CORRv2") y_label <- "0.5 x CORRv2"
    if (input$kpi_choice == "Score Multipliers: 1.5 x CORRv2") y_label <- "1.5 x CORRv2"
    if (input$kpi_choice == "Score Multipliers: 2.0 x CORRv2") y_label <- "2.0 x CORRv2"
    if (input$kpi_choice == "Score Multipliers: 2.0 x CORRv2 + 0.5 x TC") y_label <- "2.0 x CORRv2 + 0.5 x TC"
    if (input$kpi_choice == "Score Multipliers: 2.0 x CORRv2 + 1.0 x TC") y_label <- "2.0 x CORRv2 + 1.0 x TC"
    
    if (input$kpi_choice == "Percentile: CORRv2") y_label <- "CORRv2 Percentile"
    if (input$kpi_choice == "Percentile: TC") y_label <- "TC Percentile"
    if (input$kpi_choice == "Percentile: FNCv3") y_label <- "FNCv3 Percentile"
    
    if (input$kpi_choice == "Payout") y_label <- "Payout (NMR)"
    if (input$kpi_choice == "Rate of Return (%): Payout / Stake x 100") y_label <- "Rate of Return (%)"
    
    # If cumulative
    if (input$kpi_cumulative) y_label <- paste("Cumulative", y_label)
    
    # Other settings
    y_min <- min(d_kpi$KPI) * 0.95
    if (y_min > 0) y_min <- 0
    y_max <- max(d_kpi$KPI) * 1.05
    height <- 500 # default minimum height

    # Plot 
    p <- ggplot(d_kpi, aes(x = round, y = KPI, 
                           ymin = y_min, ymax = y_max,
                           color = model)) +
      geom_line() +
      theme(
        panel.border = element_rect(fill = 'transparent', color = "grey", linewidth = 0.25),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(),
        strip.clip = "on",
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent'),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(breaks = breaks_pretty(5)) +
      scale_y_continuous(breaks = breaks_pretty(5)) +
      geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey", linetype = "dashed") +
      ylab(y_label) +
      xlab("\nTournament Round")
    
    # Facet wrap?
    if (input$kpi_facet) {
      
      # Extract no. of models
      n_model <- length(unique(d_kpi$model))
      
      # Add facet_wrap
      p <- p + facet_wrap(. ~ model, ncol = 5, scales = "fixed") # fixed
      
      # Dynamic height adjustment
      if (n_model >= 10) height = 800
      if (n_model >= 15) height = 1000
      if (n_model >= 20) height = 1200
      if (n_model >= 25) height = 1400
      if (n_model >= 30) height = 1600
      if (n_model >= 35) height = 1800
      if (n_model >= 40) height = 2000
      if (n_model >= 45) height = 2200
      if (n_model >= 50) height = 2400
      if (n_model >= 55) height = 2600
      if (n_model >= 60) height = 2800
      if (n_model >= 65) height = 3000

    }
    
    # Convert to Plotly
    ggplotly(p, height = height) |> toWebGL()
    

  })
  
  
  
  # ============================================================================
  # Reactive: Payout Summary Table
  # ============================================================================
  
  # Net Round Payout Summary
  output$dt_payout_summary <- DT::renderDT({
    
    # Generate a new DT
    DT::datatable(
      
      # Data
      react_d_payout_summary(),
      
      # Other Options
      rownames = FALSE,
      extensions = "Buttons",
      options =
        list(
          dom = 'Bflrtip', # https://datatables.net/reference/option/dom
          buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          order = list(list(0, 'asc'), list(1, 'asc')),
          pageLength = 100,
          lengthMenu = c(10, 50, 100, 500, 1000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("total_stake", "net_payout", "rate_of_return"), digits = 2) |>
      
      formatStyle(columns = c("round"), fontWeight = "bold") |>
      
      formatStyle(columns = c("resolved"),
                  target = "row",
                  backgroundColor = styleEqual(c(1,0), c("transparent", "#FFF8E1"))) |>
      
      
      formatStyle(columns = c("total_stake"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "#2196F3"))) |>
      
      formatStyle(columns = c("net_payout"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = c(-1e-15, 1e-15), 
                                        values = c("#D24141", "#D1D1D1", "#00A800"))) |>
      
      formatStyle(columns = c("rate_of_return"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = c(-1e-15, 1e-15), 
                                        values = c("#D24141", "#D1D1D1", "#00A800")))
    
  })
  
  
  # Individual Model Payout Summary
  output$dt_model_payout_summary <- DT::renderDT({
    
    # Generate a new DT
    DT::datatable(
      
      # Data
      react_d_model_payout_summary(),
      
      # Other Options
      rownames = FALSE,
      extensions = "Buttons",
      options =
        list(
          dom = 'Bflrtip', # https://datatables.net/reference/option/dom
          buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          order = list(list(0, 'asc'), list(1, 'asc')),
          pageLength = 100,
          lengthMenu = c(10, 50, 100, 500, 1000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("net_payout", "avg_payout", 
                              "avg_rate_of_return_percent",
                              "sharpe_rate_of_return"), digits = 4) |>
      
      # formatStyle(columns = c("model"), fontWeight = "bold") |>
      
      formatStyle(columns = c("net_payout", "avg_payout", 
                              "avg_rate_of_return_percent",
                              "sharpe_rate_of_return"),
                  # fontWeight = "bold",
                  color = styleInterval(cuts = c(-1e-15, 1e-15), 
                                        values = c("#D24141", "#D1D1D1", "#00A800")))
    
  })
  
  
  # Payout Sim (Model)
  output$dt_payout_sim_model <- DT::renderDT({
    
    # Generate a new DT
    DT::datatable(
      
      # Data
      react_d_payout_sim_model(),
      
      # Other Options
      rownames = FALSE,
      extensions = "Buttons",
      options =
        list(
          dom = 'Bflrtip', # https://datatables.net/reference/option/dom
          buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          order = list(list(0, 'asc'), list(1, 'asc')),
          pageLength = 100,
          lengthMenu = c(10, 50, 100, 500, 1000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("sum_pay_1C0T", "sum_pay_2C0T", "sum_pay_2C1T", "sum_pay_1C3T",
                              "shp_pay_1C0T", "shp_pay_2C0T", "shp_pay_2C1T", "shp_pay_1C3T"),
                  digits = 2) |>
      
      formatStyle(columns = c("sum_pay_1C0T", "sum_pay_2C0T", "sum_pay_2C1T", "sum_pay_1C3T",
                              "shp_pay_1C0T", "shp_pay_2C0T", "shp_pay_2C1T", "shp_pay_1C3T"),
                  color = styleInterval(cuts = c(-1e-15, 1e-15),
                                        values = c("#D24141", "#D1D1D1", "#00A800"))) |>
      
      formatStyle(columns = c("model", 
                              "sum_pay_2C1T", "sum_pay_1C3T",
                              "shp_pay_2C1T", "shp_pay_1C3T"
      ), fontWeight = "bold")
    
  })
  
  
  # Payout Sim (Overall)
  output$dt_payout_sim_overall <- DT::renderDT({
    
    # Generate a new DT
    DT::datatable(
      
      # Data
      react_d_payout_sim_overall(),
      
      # Other Options
      rownames = FALSE,
      # extensions = "Buttons",
      options =
        list(
          dom = 't', # https://datatables.net/reference/option/dom
          # buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          # order = list(list(0, 'asc'), list(1, 'asc')),
          # pageLength = 10,
          # lengthMenu = c(10, 50, 100, 500, 1000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("sum_pay_1C0T", "sum_pay_2C0T", "sum_pay_2C1T", "sum_pay_1C3T",
                              "shp_pay_1C0T", "shp_pay_2C0T", "shp_pay_2C1T", "shp_pay_1C3T"),
                  digits = 2) |>
      
      formatStyle(columns = c("sum_pay_1C0T", "sum_pay_2C0T", "sum_pay_2C1T", "sum_pay_1C3T",
                              "shp_pay_1C0T", "shp_pay_2C0T", "shp_pay_2C1T", "shp_pay_1C3T"),
                  color = styleInterval(cuts = c(-1e-15, 1e-15),
                                        values = c("#D24141", "#D1D1D1", "#00A800"))) |>
      
      formatStyle(columns = c("sum_pay_2C1T", "sum_pay_1C3T",
                              "shp_pay_2C1T", "shp_pay_1C3T"
      ), fontWeight = "bold")
    
  })
  
  
  # Performance Summary
  output$dt_performance_summary <- DT::renderDT({
    
    # Generate a new DT
    DT::datatable(
      
      # Data
      react_d_performance_summary(),
      
      # Other Options
      rownames = FALSE,
      extensions = "Buttons",
      options =
        list(
          dom = 'Bflrtip', # https://datatables.net/reference/option/dom
          buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          order = list(list(0, 'asc'), list(1, 'asc')),
          pageLength = 10,
          lengthMenu = c(10, 50, 100, 500, 1000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("avg_corrV2", "sharpe_corrV2",
                              "avg_tc", "sharpe_tc",
                              "avg_2C1T", "sharpe_2C1T"
      ),
      digits = 4) |>
      
      formatStyle(columns = c("avg_corrV2", "sharpe_corrV2",
                              "avg_tc", "sharpe_tc",
                              "avg_2C1T", "sharpe_2C1T"
      ),
      color = styleInterval(cuts = c(-1e-15, 1e-15),
                            values = c("#D24141", "#D1D1D1", "#00A800")))

  })
  
  
  # KPI Filter
  output$dt_kpi <- DT::renderDT({
    
    # Generate a new DT
    DT::datatable(
      
      # Data
      react_d_kpi(),
      
      # Other Options
      rownames = FALSE,
      extensions = "Buttons",
      options =
        list(
          dom = 'Bflrtip', # https://datatables.net/reference/option/dom
          buttons = list('csv', 'excel', 'copy', 'print'), # https://rstudio.github.io/DT/003-tabletools-buttons.html
          order = list(list(0, 'asc'), list(1, 'asc')),
          pageLength = 5,
          lengthMenu = c(5, 10, 50, 100, 500, 1000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("KPI"), digits = 4) |>
      
      formatStyle(columns = c("KPI"), 
                  color = styleInterval(cuts = c(-1e-15, 1e-15),
                                        values = c("#D24141", "#D1D1D1", "#00A800")))
    
  })
  
  
  
  # ============================================================================
  # Reactive: Model Performance Charts
  # ============================================================================
  
  # Boxplot - TC Percentile
  output$plot_boxplot_tcp <- renderPlotly({
    
    # Data
    d_filter <- react_d_filter()
    
    # Order by TC_PCT
    d_model_order <- with(d_filter, reorder(model, tc_pct, median))
    d_filter$model_order <- factor(d_filter$model, levels = levels(d_model_order))
    
    # ggplot2
    p <- ggplot(d_filter, aes(x = model_order, y = tc_pct, group = model_order, color = model_order)) +
      geom_boxplot() +
      theme(
        panel.border = element_blank(),
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.25),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(),
        strip.clip = "on",
        legend.position = "none"
      ) +
      scale_color_manual(values = gen_custom_palette(d_filter$model)) +
      xlab("Model") +
      ylab("TC Percentile") +
      scale_y_continuous(limits = c(0,100), breaks = breaks_pretty(4)) +
      coord_flip()
    
    
    # Dynamic height adjustment
    n_model <- length(unique(d_filter$model))
    height <- 600 # default
    if (n_model > 10) height = 800
    if (n_model > 15) height = 1000
    if (n_model > 20) height = 1200
    if (n_model > 25) height = 1400
    if (n_model > 30) height = 1600
    if (n_model > 35) height = 1800
    if (n_model > 40) height = 2000
    if (n_model > 45) height = 2200
    if (n_model > 50) height = 2400
    
    # Generate plotly
    ggplotly(p, height = height)
    
  })
  
  
  # ============================================================================
  # Reactive: Downloads
  # ============================================================================
  
  output$download_raw <- downloadHandler(
    filename = "raw_data.csv",
    content = function(file) {fwrite(react_d_raw(), file, row.names = FALSE)}
  )
  
  
  # ============================================================================
  # Session Info
  # ============================================================================
  
  output$session_info <- renderPrint({
    sessionInfo()
  })
  
  
  # ============================================================================
  # Trick to keep session alive
  # https://tickets.dominodatalab.com/hc/en-us/articles/360015932932-Increasing-the-timeout-for-Shiny-Server
  # ============================================================================
  
  output$keepAlive <- renderText({
    req(input$count)
    # paste("keep alive ", input$count) 
    " "
  })
  
}


# ==============================================================================
# App
# ==============================================================================

shinyApp(ui, server)

