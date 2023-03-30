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
library(Rnumerai)


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
  return(as.data.table(d_raw))
  
}

# Reformat
reformat_data <- function(d_raw) {
  
  # Keep some columns only
  col_keep <- c("model", "roundNumber", 
                "roundOpenTime", "roundResolveTime",
                "roundResolved", "selectedStakeValue",
                "corr", "corrPercentile", 
                "fncV3", "fncV3Percentile",
                "tc", "tcPercentile",
                "corrWMetamodel",
                "roundPayoutFactor", "payout")
  d_munged <- d_raw[, col_keep, with = FALSE]
  
  # Date
  d_munged[, roundOpenTime := as.Date(roundOpenTime)]
  d_munged[, roundResolveTime := as.Date(roundResolveTime)]
  
  # Reformat percentile
  d_munged[, corrPercentile := round(corrPercentile * 100, 6)]
  d_munged[, fncV3Percentile := round(fncV3Percentile * 100, 6)]
  d_munged[, tcPercentile := round(tcPercentile * 100, 6)]
  
  # Rename columns
  colnames(d_munged) <- c("model", "round", 
                          "date_open", "date_resolved",
                          "resolved", "stake",
                          "corr", "corr_pct",
                          "fncv3", "fncv3_pct",
                          "tc", "tc_pct", 
                          "corr_meta",
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
      menuItem(text = "Payout Summary", tabName = "payout", icon = icon("credit-card")),
      menuItem(text = "Model Performance", tabName = "performance", icon = icon("line-chart")),
      menuItem(text = "Raw Data", tabName = "raw_data", icon = icon("download")),
      menuItem(text = "Community", tabName = "community", icon = icon("users")),
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
                         
                         markdown("## **Step 1 - Select Your Models**"),
                         
                         markdown("### First, click this ⬇"),
                         
                         pickerInput(inputId = "model",
                                     label = " ",
                                     choices = sort(Rnumerai::get_leaderboard()$username),
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
                
                h3(strong(textOutput(outputId = "text_next"))),
                h3(strong(textOutput(outputId = "text_soon")))
                
              )
      ),
      
      # ========================================================================
      # Payout Summary
      # ========================================================================
      
      tabItem(tabName = "payout", 
              
              fluidPage(
                
                markdown("# **Payout Summary**"),
                markdown("### Remember to refresh the charts after making changes to model selection or settings below."),
                br(),
                
                fluidRow(
                  
                  column(8,
                         
                         markdown("## **Step 1 - Define the Range**"),
                         
                         sliderInput(inputId = "range_round", 
                                     label = "Numerai Classic Tournament Rounds",
                                     width = "100%",
                                     step = 1,
                                     min = 168, # first tournament round
                                     max = Rnumerai::get_current_round(),
                                     # note: daily rounds from round 339
                                     pre = "Round ",
                                     value = c(339, Rnumerai::get_current_round())
                         )
                  ),
                  
                  column(4, 
                         
                         markdown("## **Step 2 - Visualise**"),
                         br(),
                         actionBttn(inputId = "button_filter", 
                                    label = "Create / Refresh Charts",
                                    color = "primary",
                                    icon = icon("refresh"),
                                    style = "gradient",
                                    block = TRUE)
                  )
                ), # end of fluidRow
                
                br(),
                
                tabsetPanel(type = "tabs",
                            
                            tabPanel("All Models",
                                     
                                     br(),
                                     
                                     h3(strong(textOutput(outputId = "text_payout_all_models"))),
                                     
                                     fluidRow(
                                       class = "text-center",
                                       valueBoxOutput("payout_confirmed", width = 2),
                                       valueBoxOutput("payout_pending", width = 2),
                                       valueBoxOutput("payout_total", width = 2),
                                       valueBoxOutput("payout_n_round", width = 2),
                                       valueBoxOutput("payout_average", width = 2),
                                       valueBoxOutput("payout_avg_ror", width = 2)
                                     ),
                                     
                                     br(),
                                     
                                     shinycssloaders::withSpinner(plotlyOutput("plot_payout_stacked")),
                                     
                                     br(),
                                     br(),
                                     
                                     DTOutput("dt_payout_summary")
                                     
                            ),
                            
                            tabPanel("Individual Models",
                                     # br(),
                                     # materialSwitch(inputId = "switch_scale_payout", 
                                     #                label = "Fixed Scale?",
                                     #                value = TRUE,
                                     #                status = "primary",
                                     #                ),
                                     br(),
                                     h3(strong(textOutput(outputId = "text_payout_ind_models"))),
                                     br(),
                                     shinycssloaders::withSpinner(plotlyOutput("plot_payout_individual"))
                            )
                            
                ) # end of tabsetPanel
                
              ) # end of fluidPage
              
      ),
      
      
      # ========================================================================
      # Model Performance
      # ========================================================================
      
      tabItem(tabName = "performance", 
              fluidPage(
                
                markdown("# **Model Performance**"),
                
                markdown("![image](https://media.giphy.com/media/cftSzNoCTfSyAWctcl/giphy.gif)")
                
                # markdown("### **Note 1**: Experimental features. Changes to be expected in the coming days."),
                # markdown("### **Note 2**: Define the range in `Payout Summary` first."),
                # br(),
                # tabsetPanel(type = "tabs",
                #             tabPanel("Boxplot - TCP",
                #                      br(),
                #                      markdown("### **TC Percentile by Model**"),
                #                      shinycssloaders::withSpinner(plotlyOutput("plot_boxplot_tcp"))
                #             )
                # ) # End of tabsetPanel
                
              ) # End of fluidPage
              
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
              markdown("![image](https://media.giphy.com/media/cftSzNoCTfSyAWctcl/giphy.gif)")
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
              markdown("## **Changelog**"),
              markdown(
                "
                - #### **0.1.0** — First prototype with an interactive table output
                - #### **0.1.1** — Added a functional `Payout Summary` page
                - #### **0.1.2** — `Payout Summary` layout updates
                - #### **0.1.3** — Added `Raw Data`
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
    right = paste0("Version 0.1.3"))
  
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
    if (length(react_ls_model()) >= 1) "⬅ [NEW] Payout Summary and Raw Data 📊💸" else " "
  })
  
  output$text_soon <- renderText({
    if (length(react_ls_model()) >= 1) "⬅ [COMING SOON] Model Performance 📈🔥" else " "
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
      
      # Reformat and Filter
      d_filter <- reformat_data(react_d_raw())
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
        react_d_filter() |> 
        lazy_dt() |> 
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
  
  
  # ============================================================================
  # Reactive: Payout Value Boxes
  # ============================================================================
  
  output$text_payout_all_models <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payouts in NMR (All Models)" else " "
  })
  
  output$text_payout_ind_models <- renderText({
    if (nrow(react_d_filter()) >= 1) "Payouts in NMR (Individual Models)" else " "
  })
  
  output$payout_confirmed <- renderValueBox({
    valueBox(value = round(sum(react_d_filter()[resolved == TRUE, ]$payout, na.rm = T), 2),
             subtitle = "Realised",
             color = "olive")
  })
  
  output$payout_pending <- renderValueBox({
    valueBox(value = round(sum(react_d_filter()[resolved == FALSE, ]$payout, na.rm = T), 2),
             subtitle = "Pending",
             color = "yellow")
  })
  
  output$payout_total <- renderValueBox({
    valueBox(value = round(sum(react_d_filter()$payout, na.rm = T), 2),
             subtitle = "Realised + Pending",
             color = "light-blue")
  })
  
  output$payout_n_round <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = nrow(react_d_payout_summary()[total_stake > 0, ]),
             subtitle = "Staked Rounds",
             color = "light-blue")
  })
  
  output$payout_average <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = round(mean(react_d_payout_summary()[total_stake > 0, ]$net_payout, na.rm = T), 2),
             subtitle = "Avg. Round Payout",
             color = "light-blue")
  })
  
  output$payout_avg_ror <- renderValueBox({
    # Use rounds with stake > 0 only
    valueBox(value = paste(round(mean(react_d_payout_summary()[total_stake > 0, ]$rate_of_return), 2), "%"),
             subtitle = "Avg. Round ROR",
             color = "light-blue")
  })
  
  # ============================================================================
  # Reactive: Payout Charts
  # ============================================================================
  
  # Stacked Bar Chart
  output$plot_payout_stacked <- renderPlotly({
    
    # Data
    d_filter <- react_d_filter()
    
    # Filter
    d_filter <- d_filter[stake > 0]
    
    # ggplot
    p <- ggplot(d_filter, 
                aes(x = date_resolved, y = payout, fill = payout, 
                    text = paste("Model:", model, 
                                 "\nRound:", round,
                                 "\nRound Open Date:", date_open,
                                 "\nRound Resolved Date:", date_resolved,
                                 "\nRound Status:", resolved,
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
      scale_x_date(breaks = breaks_pretty(10),
                   labels = label_date_short(format = c("%Y", "%b", "%d"), sep = "\n")
      ) +
      xlab(" \nDate (Round Resolved / Resolving)") +
      ylab("Realised / Pending Payout (NMR)")
    
    # Generate plotly
    ggplotly(p, tooltip = "text")
    
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
                                 "\nRound Status:", resolved,
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
      xlab(" \nTournament Round") +
      ylab("Realised / Pending Payout (NMR)")
    
    # Facet setting
    if ((n_model %% 4) == 0) {
      p <- p + facet_wrap(. ~ model, ncol = 4, scales = "fixed")
    } else if ((n_model %% 5) == 0) {
      p <- p + facet_wrap(. ~ model, ncol = 5, scales = "fixed")
    } else {
      p <- p + facet_wrap(. ~ model, ncol = 6, scales = "fixed")
    }
    
    # Dynamic height adjustment
    height <- 600 # default minimum height
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

