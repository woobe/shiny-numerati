library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(scico)
library(ggthemes)
library(data.table)
library(dtplyr)
library(parallel)
library(Rnumerai)


# ==============================================================================
# Tournament Information
# ==============================================================================

# Download latest leaderboard from Numerai and get a list of all models
d_lb <- get_leaderboard()
ls_model <- sort(d_lb$username)

# Round info
d_comp <- get_competitions()


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
  d_munged <- d_raw[, col_keep, with = FALSE]
  
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
  
  
  # ============================================================================
  # Sidebar
  # ============================================================================
  
  sidebar = shinydashboardPlus::dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      menuItem(text = "Start Here", tabName = "start", icon = icon("play")),
      menuItem(text = "Payout Summary", tabName = "payout", icon = icon("credit-card")),
      menuItem(text = "Model Performance", tabName = "performance", icon = icon("line-chart")),
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
                # Special script to keep session alive
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
                    }, 5000)
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
      # Payout Summary
      # ========================================================================
      
      tabItem(tabName = "payout", 
              fluidPage(
                
                markdown("# **Payout Summary**"),
                markdown("### Remember to refresh the charts after making changes to model selection or settings below"),
                br(),
                
                fluidRow(
                  
                  column(6,
                         
                         markdown("## **Step 1 - Define the Range**"),
                         
                         sliderInput(inputId = "range_round", 
                                     label = "Numerai Classic Tournament Rounds",
                                     width = "100%",
                                     min = min(d_comp$number),
                                     max = max(d_comp$number),
                                     # note: daily rounds from round 339
                                     value = c(394, max(d_comp$number))
                         )
                  ),
                  
                  column(6, 
                         
                         markdown("## **Step 2 - Visualise**"),
                         br(),
                         actionBttn(inputId = "button_filter", 
                                    label = "Create / Refresh Charts",
                                    color = "primary",
                                    icon = icon("refresh"),
                                    style = "gradient",
                                    block = TRUE)
                  )
                ),
                
                br(),
                
                tabsetPanel(type = "tabs",
                            
                            tabPanel("All Models",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout"))),
                                     
                                     fluidRow(
                                       # class = "text-center",
                                       valueBoxOutput("payout_confirmed", width = 3),
                                       valueBoxOutput("payout_pending", width = 3),
                                       valueBoxOutput("payout_total", width = 3),
                                       valueBoxOutput("payout_average", width = 3)
                                     ),
                                     
                                     br(),
                                     
                                     shinycssloaders::withSpinner(plotlyOutput("plot_payout_stacked")),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     
                                     DTOutput("dt_payout_summary")
                                     
                            ),
                            
                            tabPanel("Individual Models",
                                     br(),
                                     shinycssloaders::withSpinner(plotlyOutput("plot_payout_individual")))
                            

                )
                
              )
              
      ),
      
      
      # ========================================================================
      # Model Performance
      # ========================================================================
      
      tabItem(tabName = "performance", 
              fluidPage(
                markdown("![image](https://media.giphy.com/media/cftSzNoCTfSyAWctcl/giphy.gif)")
              )
      ),
      
      
      # ========================================================================
      # About
      # ========================================================================
      
      tabItem(tabName = "about", 
              markdown("## **About this App**"),
              markdown('#### Yet another Numerai community dashboard by <b><a href="https://linktr.ee/jofaichow" target="_blank">Jo-fai Chow</a></b>.'),
              
              br(),
              markdown("## **Acknowledgements**"),
              markdown("- #### This hobby project was inspired by Rajiv's <b><a href='https://huggingface.co/spaces/rajistics/shiny-kmeans' target='_blank'>shiny-kmeans</a></b> on 🤗 Spaces."),
              markdown('- #### The <b><a href="https://linktr.ee/jofaichow" target="_blank">Rnumerai</a></b> package from Omni Analytics Group.'),
              
              br(),
              markdown("## **Changelog**"),
              markdown(
                "
                - #### **0.1.0** — First prototype with an interactive table output
                - #### **0.1.1** — Added a functional `Payout Summary` page
                - #### **0.1.2** — `Payout Summary` layout updates
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
    right = paste0("Version 0.1.2"))
  
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
    if (length(react_ls_model()) >= 1) "⬅ [NEW] Payout Summary 📈📊🔥" else " "
  })
  
  react_d_model <- eventReactive(
    input$button_download,
    {
      
      # Parallelised download
      d_raw <- rbindlist(mclapply(X = input$model, 
                                  FUN = download_raw_data, 
                                  mc.cores = detectCores()))
      
      # Data munging
      d_munged <- reformat_data(d_raw)
      
      # Return final result
      d_munged
      
    }
  )
  
  # ============================================================================
  # Reactive: DataTable
  # ============================================================================
  
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
  # Reactive: filtering data for all charts
  # ============================================================================
  
  react_d_filter <- eventReactive(
    input$button_filter,
    {
      
      # Model data
      d_filter <- react_d_model()
      
      # Filtering
      d_filter <- d_filter[pay_ftr > 0, ] # ignoring the new daily rounds for now
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
        react_d_filter() |> lazy_dt() |> 
        group_by(round, resolved) |>
        summarise(stake = sum(stake, na.rm = T),
                  payout = sum(payout, na.rm = T)) |>
        as.data.table()
      d_smry$rate_of_return <- (d_smry$payout / d_smry$stake) * 100
      
      # Rename
      colnames(d_smry) <- c("round", "resolved", "total_stake", 
                            "total_payout", "rate_of_return")
      
      # Return
      d_smry
      
    })
  
  
  # ============================================================================
  # Reactive: Payout Value Boxes
  # ============================================================================
  
  output$text_payout <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payouts in NMR" else " "
  })
  
  output$payout_confirmed <- renderValueBox({
    valueBox(value = round(sum(react_d_filter()[resolved == TRUE, ]$payout, na.rm = T), 2),
             subtitle = "Confirmed",
             icon = icon("check"),
             color = "green")
  })
  
  output$payout_pending <- renderValueBox({
    valueBox(value = round(sum(react_d_filter()[resolved == FALSE, ]$payout, na.rm = T), 2),
             subtitle = "Pending",
             icon = icon("clock"),
             color = "yellow")
  })
  
  output$payout_total <- renderValueBox({
    valueBox(value = round(sum(react_d_filter()$payout, na.rm = T), 2),
             subtitle = "Confirmed + Pending",
             icon = icon("plus"),
             color = "aqua")
  })
  
  output$payout_average <- renderValueBox({
    valueBox(value = round((sum(react_d_filter()$payout, na.rm = T) / length(unique(react_d_filter()$round))), 2),
             subtitle = "Round Average",
             icon = icon("credit-card"),
             color = "light-blue")
  })
  
  
  # ============================================================================
  # Reactive: Payout Charts
  # ============================================================================
  
  # Stacked Bar Chart
  output$plot_payout_stacked <- renderPlotly({
    
    # ggplot
    p <- ggplot(react_d_filter(), 
                aes(x = round, y = payout, fill = payout, 
                    text = paste("Model:", model, 
                                 "\nRound:", round,
                                 "\nResolved:", resolved, 
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
      geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey") +
      scale_fill_scico(palette = "vikO", direction = -1, midpoint = 0) +
      xlab("Tournament Round") +
      ylab("Payout (NMR)")
      
    # Generate plotly
    ggplotly(p, tooltip = "text")
    
  })
  
  
  # Individual
  output$plot_payout_individual <- renderPlotly({
    
    # Get the number of unique models
    n_model <- length(unique(react_d_filter()$model))
    
    # Base plot
    p <- ggplot(react_d_filter(), aes(x = round, y = payout, fill = payout, 
                                      text = paste("Round:", round,
                                                   "\nResolved:", resolved,
                                                   "\nPayout:", round(payout,2), "NMR"))) +
      geom_bar(stat = "identity") +
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
      geom_hline(aes(yintercept = 0), linewidth = 0.25, color = "grey") +
      scale_fill_scico(palette = "vikO", direction = -1, midpoint = 0) +
      xlab("Tournament Round") +
      ylab("Confirmed / Pending Payout (NMR)")
    
    # Facet setting
    if (n_model %% 5 == 0) {
      p <- p + facet_wrap(. ~ model, ncol = 5)
    } else {
      p <- p + facet_wrap(. ~ model)
    }
    
    # Dynamic height adjustment
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
    ggplotly(p, height = height, tooltip = "text")
    
  })
  
  
  # ============================================================================
  # Reactive: Payout Summary Table
  # ============================================================================
  
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
          lengthMenu = c(5, 10, 20, 100, 500, 1000),
          columnDefs = list(list(className = 'dt-center', targets = "_all")))
    ) |>
      
      # Reformat individual columns
      formatRound(columns = c("total_stake", "total_payout", "rate_of_return"), digits = 2) |>
      
      formatStyle(columns = c("round"), fontWeight = "bold") |>
      
      formatStyle(columns = c("total_stake"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = -1e-15, values = c("#D24141", "#2196F3"))) |>
      
      formatStyle(columns = c("total_payout"),
                  fontWeight = "bold",
                  color = styleInterval(cuts = c(-1e-15, 1e-15), 
                                        values = c("#D24141", "#D1D1D1", "#00A800"))) |>
      
      formatStyle(columns = c("rate_of_return"),
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

