library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(gt)
library(gtExtras)
library(ggplot2)
library(glue)
library(reactable)
library(plotly)

# Function to generate a Reactable with colored columns
PlayCallsReactable1 <- function(data) {
  # Normalize PPP values
  ppp_normalized <- (data$PPP - min(data$PPP, na.rm = TRUE)) / (max(data$PPP, na.rm = TRUE) - min(data$PPP, na.rm = TRUE))
  ppp_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(ppp_normalized), maxColorValue = 255)
  
  # Normalize eFG% values
  efg_normalized <- (data$`eFG%` - min(data$`eFG%`, na.rm = TRUE)) / (max(data$`eFG%`, na.rm = TRUE) - min(data$`eFG%`, na.rm = TRUE))
  efg_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(efg_normalized), maxColorValue = 255)
  
  # Normalize FG% values
  fg_normalized <- (data$`FG%` - min(data$`FG%`, na.rm = TRUE)) / (max(data$`FG%`, na.rm = TRUE) - min(data$`FG%`, na.rm = TRUE))
  fg_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(fg_normalized), maxColorValue = 255)
  
  # Normalize 2pt% values
  twop_normalized <- (data$`2pt%` - min(data$`2pt%`, na.rm = TRUE)) / (max(data$`2pt%`, na.rm = TRUE) - min(data$`2pt%`, na.rm = TRUE))
  twop_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(twop_normalized), maxColorValue = 255)
  
  # Normalize 3pt% values
  threep_normalized <- (data$`3pt%` - min(data$`3pt%`, na.rm = TRUE)) / (max(data$`3pt%`, na.rm = TRUE) - min(data$`3pt%`, na.rm = TRUE))
  threep_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(threep_normalized), maxColorValue = 255)
  
  # Create the Reactable
  reactable(
    data,
    defaultSorted = '# of Possessions',
    defaultSortOrder = 'desc',
    defaultPageSize = 15,  # Set maximum number of rows to 15
    bordered = TRUE,
    highlight = TRUE,
    theme = reactableTheme(
      stripedColor = '#f0f0f0',
      backgroundColor = 'transparent',
      borderColor = 'black'
    ),
    columns = list(
      'Play Calls' = colDef(name = 'Offensive Set', minWidth = 175, align = 'left', style = list(fontWeight = "bold"), filterable = TRUE),
      '# of Possessions' = colDef(name = '# of Possessions', align = 'left'),
      'PPP' = colDef(name = 'Points per Possession', align = 'center', style = JS("function(rowInfo, column, state) {
        const { showColors, ppp_colors } = state.meta;
        return { backgroundColor: ppp_colors[rowInfo.index] };
      }")),
      'eFG%' = colDef(name = 'eFG%', align = 'center', style = JS("function(rowInfo, column, state) {
        const { showColors, efg_colors } = state.meta;
        return { backgroundColor: efg_colors[rowInfo.index] };
      }")),
      'FG' = colDef(name = 'FG', align = 'right'),
      'FG%' = colDef(name = 'FG%', align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, fg_colors } = state.meta;
        return { backgroundColor: fg_colors[rowInfo.index] };
      }")),
      '2pt' = colDef(name = '2pt', align = 'right'),
      '2pt%' = colDef(name = '2pt%', align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, twop_colors } = state.meta;
        return { backgroundColor: twop_colors[rowInfo.index] };
      }")),
      '3pt' = colDef(name = '3pt', align = 'right'),
      '3pt%' = colDef(name = '3pt%', align = 'left', style = JS("function(rowInfo, column, state) {
        const { showColors, threep_colors } = state.meta;
        return { backgroundColor: threep_colors[rowInfo.index] };
      }"))
    ),
    rowStyle = function(row) {
      list(height = '45px')
    },
    meta = list(
      ppp_colors = ppp_colors,
      efg_colors = efg_colors,
      fg_colors = fg_colors,
      twop_colors = twop_colors,
      threep_colors = threep_colors,
      showColors = TRUE
    )
  )
}

PlayCallsReactable2 <- function(data1) {
  # Normalize fd rate values
  fd_normalized <- (data1$`Foul Drawn Rate` - min(data1$`Foul Drawn Rate`, na.rm = TRUE)) / (max(data1$`Foul Drawn Rate`, na.rm = TRUE) - min(data1$`Foul Drawn Rate`, na.rm = TRUE))
  fd_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(fd_normalized), maxColorValue = 255)
  
  # Normalize adv values
  adv_normalized <- (data1$`1v1 Advantage Rate` - min(data1$`1v1 Advantage Rate`, na.rm = TRUE)) / (max(data1$`1v1 Advantage Rate`, na.rm = TRUE) - min(data1$`1v1 Advantage Rate`, na.rm = TRUE))
  adv_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(adv_normalized), maxColorValue = 255)
  
  # Normalize contest values
  contest_normalized <- (data1$`Contested Rate` - min(data1$`Contested Rate`, na.rm = TRUE)) / (max(data1$`Contested Rate`, na.rm = TRUE) - min(data1$`Contested Rate`, na.rm = TRUE))
  contest_colors <- rgb(colorRamp(c("#7bafd4", 'transparent'))(contest_normalized), maxColorValue = 255)
  
  # Normalize open values
  open_normalized <- (data1$`Open Rate` - min(data1$`Open Rate`, na.rm = TRUE)) / (max(data1$`Open Rate`, na.rm = TRUE) - min(data1$`Open Rate`, na.rm = TRUE))
  open_colors <- rgb(colorRamp(c("transparent", "#7bafd4"))(open_normalized), maxColorValue = 255)
  
  # Normalize tov values
  tov_normalized <- (data1$`Tov Rate` - min(data1$`Tov Rate`, na.rm = TRUE)) / (max(data1$`Tov Rate`, na.rm = TRUE) - min(data1$`Tov Rate`, na.rm = TRUE))
  tov_colors <- rgb(colorRamp(c("#7bafd4", 'transparent'))(tov_normalized), maxColorValue = 255)
  
  # Create the Reactable
  reactable(
    data1,
    defaultSorted = '# of Possessions',
    defaultSortOrder = 'desc',
    defaultPageSize = 15,  # Set maximum number of rows to 15
    bordered = TRUE,
    highlight = TRUE,
    theme = reactableTheme(
      stripedColor = '#f0f0f0',
      backgroundColor = 'transparent',
      borderColor = 'black'
    ),
    columns = list(
      'Play Calls' = colDef(name = 'Offensive Set', minWidth = 175, align = 'left', style = list(fontWeight = "bold"), filterable = TRUE),
      '# of Possessions' = colDef(name = '# of Possessions', align = 'left'),
      'Foul Drawn Rate' = colDef(name = 'Foul Drawn Rate', align = 'center', style = JS("function(rowInfo, column, state) {
        const { showColors, fd_colors } = state.meta;
        return { backgroundColor: fd_colors[rowInfo.index] };
      }")),
      '1v1 Advantage Rate' = colDef(name = '1v1 Advantage Rate', align = 'center', style = JS("function(rowInfo, column, state) {
        const { showColors, adv_colors } = state.meta;
        return { backgroundColor: adv_colors[rowInfo.index] };
      }")),
      'Contested Rate' = colDef(name = 'Contested Rate', align = 'center', style = JS("function(rowInfo, column, state) {
        const { showColors, contest_colors } = state.meta;
        return { backgroundColor: contest_colors[rowInfo.index] };
      }")),
      'Open Rate' = colDef(name = 'Open Rate', align = 'center', style = JS("function(rowInfo, column, state) {
        const { showColors, open_colors } = state.meta;
        return { backgroundColor: open_colors[rowInfo.index] };
      }")),
      'Tov Rate' = colDef(name = 'Tov Rate', align = 'center', style = JS("function(rowInfo, column, state) {
        const { showColors, tov_colors } = state.meta;
        return { backgroundColor: tov_colors[rowInfo.index] };
      }"))
    ),
    rowStyle = function(row) {
      list(height = '45px')
    },
    meta = list(
      fd_colors = fd_colors,
      adv_colors = adv_colors,
      contest_colors = contest_colors,
      open_colors = open_colors,
      tov_colors = tov_colors,
      showColors = TRUE
    )
  )
}

all_sets_data <- read.csv('Possession Outcome Stats.csv')
all_sets <- c(unique(all_sets_data$`PLAY.CALLS`))

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .btn-custom {
        background-color: #cfcfcf;
        color: black;
        text-decoration: underline;
        margin-right: 10px;
      }
      .title-container {
        display: flex;
        align-items: center;
        justify-content: center;
        margin-top: 28px;
      }
      .title-text {
        text-align: center;
        line-height: 1.2;
      }
      .nav-tabs > li > a {
        color: black !important;
        font-size: 18px;
        font-weight: bold;
        text-decoration: underline;
      }
      .nav-tabs > li.active > a {
        color: black !important;
        font-size: 18px;
        font-weight: bold;
        text-decoration: underline;
      }
    "))
  ),
  
  # Title with images
  div(
    class = "title-container",
    img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/North_Carolina_Tar_Heels_logo.svg/1200px-North_Carolina_Tar_Heels_logo.svg.png", height = "85px", style = "margin-right: 10px;"),
    div(
      class = "title-text",
      span("Offensive Set Metrics", style = "font-size: 28px; font-weight: bold; display: block;"),
      span("24-25 WBB", style = "font-size: 18px; display: block;")
    ),
    img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/North_Carolina_Tar_Heels_logo.svg/1200px-North_Carolina_Tar_Heels_logo.svg.png", height = "85px", style = "margin-left: 10px;")
  ),
  
  # Tabs
  tabsetPanel(
    tabPanel(
      "All Sets",
      fluidRow(
        column(12, align = "center",
               actionButton("show_poss", "Possession Outcome Stats", class = "btn btn-custom"),
               actionButton("show_rating", "Possession Rating Stats", class = "btn btn-custom")
        )
      ),
      fluidRow(
        column(12,
               reactableOutput("metrics_table")
        )
      )
    ),
    tabPanel(
      "Grouped Sets",
      fluidRow(
        column(12, align = "center",
               actionButton("show_poss_grouped", "Possession Outcome Stats", class = "btn btn-custom"),
               actionButton("show_rating_grouped", "Possession Rating Stats", class = "btn btn-custom")
        )
      ),
      fluidRow(
        column(12,
               reactableOutput("grouped_metrics_table")
        )
      )
    ),
    tabPanel(
      "Set Comparisons - By Group",
      fluidRow(
        column(4, 
               selectInput("set_select", "Select Set:", 
                           choices = c("BLOB", "Blue", "Chin", "Horns", "SLOB", "Transition", "Zipper", "Zone", 'Iverson', 'Fist', 'Floppy', 'Drag'), 
                           selected = "BLOB")
        ),
        column(8, align = "center",
               actionButton("show_poss_set", "Possession Outcome Stats", class = "btn btn-custom"),
               actionButton("show_rating_set", "Possession Rating Stats", class = "btn btn-custom")
        )
      ),
      fluidRow(
        column(12,
               gt_output("set_comparisons_table")
        )
      )
    ),
    tabPanel(
      "Set Comparisons - 1v1",
      fluidRow(
        column(4, 
               selectizeInput("select_set_1", "Select Set 1:", 
                           choices = all_sets, selected = all_sets[1])
        ),
        column(4, 
               selectizeInput("select_set_2", "Select Set 2:", 
                           choices = all_sets, selected = all_sets[2])
        ),
        column(4, align = "center",
               actionButton("show_poss_two", "Possession Outcome Stats", class = "btn btn-custom"),
               actionButton("show_rating_two", "Possession Rating Stats", class = "btn btn-custom")
        )
      ),
      fluidRow(
        column(12,
               gt_output("two_set_comparisons_table")
        )
      )
    ),
    #--
    tabPanel(
      "Plot Comparisons",
      fluidRow(
        column(12, align = "center",
               actionButton("show_poss_plot", "Possession Outcome Stats", class = "btn btn-custom"),
               actionButton("show_rating_plot", "Possession Rating Stats", class = "btn btn-custom")
        )
      ),
      fluidRow(
        column(6, offset = 3, align = "center",
               uiOutput("x_axis_select"),
               uiOutput("y_axis_select")
        )
      ),
      fluidRow(
        column(12, plotlyOutput("play_call_plot", height = "600px"))
      )
    )
    #--
  )
)

# Define server logic
server <- function(input, output, session) {
#---plot stuff----
  observeEvent(input$show_poss_plot, {
    output$x_axis_select <- renderUI({
      selectInput('x_axis', 'Select X-Axis Stat:',
                  choices = c('PPP', 'eFG_Percentage', 'FG_Percentage', 'Two_Pt_Percentage', 'Three_Pt_Percentage'), selected = 'PPP')
    })
    output$y_axis_select <- renderUI({
      selectInput('y_axis', 'Select Y-Axis Stat:',
                  choices = c('PPP', 'eFG_Percentage', 'FG_Percentage', 'Two_Pt_Percentage', 'Three_Pt_Percentage'), selected = 'eFG_Percentage')
    })
    
    output$play_call_plot <- renderPlotly({
      req(input$x_axis, input$y_axis)
      data <- read.csv('Possession Outcome Stats.csv')
      data <- data[,-1]
      data <- data %>%
        rename(
          'PlayCall' = `PLAY.CALLS`,
          '# of Possessions' = total_calls,
          'FG_Percentage' = FG_percent,
          'eFG_Percentage' = eFG,
          'Two_Pt_Percentage' = two_pt_percent,
          'Three_Pt_Percentage' = three_pt_percent
        )
      x_mean <- mean(data[[input$x_axis]], na.rm = TRUE)
      y_mean <- mean(data[[input$y_axis]], na.rm = TRUE)
      data[[input$x_axis]] <- as.numeric(data[[input$x_axis]])
      data[[input$y_axis]] <- as.numeric(data[[input$y_axis]])
      plot <- ggplot(data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
        geom_point(aes(text = paste('Play Call:', PlayCall, '<br># of Possessions:', `# of Possessions`)), 
                   color = "black", size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dotted", fill = "lightgray", alpha = 0.3) +
        geom_vline(xintercept = x_mean, linetype = "dotted", color = "black") +
        geom_hline(yintercept = y_mean, linetype = "dotted", color = "black") +
        theme_minimal() +
        labs(title = paste0("Play Call Outcome Visualization: ", input$x_axis, " vs ", input$y_axis), 
             subtitle = "Hover over point to see the Play Call",
             x = input$x_axis, y = input$y_axis) +
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
              plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10))
      
      ggplotly(plot, tooltip = c("text", "x", "y")) %>%
        layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
      #--
      # plot <- ggplot(data, aes_string(x = input$x_axis, y = input$y_axis, 
      #                                 text = "paste('Play Call:', PlayCall, '<br># of Possessions:', `# of Possessions`)")) +
      #   geom_point(color = "black", size = 3) +
      #   geom_vline(xintercept = x_mean, linetype = "dotted", color = "black") +
      #   geom_hline(yintercept = y_mean, linetype = "dotted", color = "black") +
      #   theme_minimal() +
      #   labs(title = paste0("Play Call Outcome Visualization: ", input$x_axis, " vs ", input$y_axis, '\n Hover over point to see the Play Call'), 
      #        x = input$x_axis, y = input$y_axis) +
      #   theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      #         axis.title = element_text(size = 12),
      #         axis.text = element_text(size = 10))
      # 
      # ggplotly(plot, tooltip = c("text", "x", "y")) %>%
      #   layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
    })
  })
  #--
  observeEvent(input$show_rating_plot, {
    output$x_axis_select <- renderUI({
      selectInput("x_axis", "Select X-Axis Stat:",
                  choices = c('PPP', "Foul_Drawn_Rate", "One.v.One_Advantage", "Contested", "Open", "Tov_Rate"), selected = "PPP")
    })
    
    output$y_axis_select <- renderUI({
      selectInput("y_axis", "Select Y-Axis Stat:",
                  choices = c('PPP', "Foul_Drawn_Rate", "One.v.One_Advantage", "Contested", "Open", "Tov_Rate"), selected = "Foul_Drawn_Rate")
    })
    output$play_call_plot <- renderPlotly({
      req(input$x_axis, input$y_axis)
      data <- read.csv('Possession Rating Stats.csv')
      data <- data[,-1]
      join_data <- read.csv('Possession Outcome Stats.csv')
      join_data <- join_data[,-1]
      data <- right_join(data, join_data, by = 'PLAY.CALLS') %>% select(PLAY.CALLS, total_calls.x, FD_rate, Advantage_rate, Contested_rate, Open_rate, TO_rate, PPP)
      data <- data %>%
        rename(
          'PlayCall' = `PLAY.CALLS`,
          '# of Possessions' = total_calls.x,
          'Foul_Drawn_Rate' = FD_rate,
          'One.v.One_Advantage' = Advantage_rate,
          'Contested' = Contested_rate,
          'Open' = Open_rate,
          'Tov_Rate' = TO_rate
        )
      x_mean <- mean(data[[input$x_axis]], na.rm = TRUE)
      y_mean <- mean(data[[input$y_axis]], na.rm = TRUE)
      data[[input$x_axis]] <- as.numeric(data[[input$x_axis]])
      data[[input$y_axis]] <- as.numeric(data[[input$y_axis]])
      plot <- ggplot(data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
        geom_point(aes(text = paste('Play Call:', PlayCall, '<br># of Possessions:', `# of Possessions`)), 
                   color = "black", size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dotted", fill = "lightgray", alpha = 0.3) +
        geom_vline(xintercept = x_mean, linetype = "dotted", color = "black") +
        geom_hline(yintercept = y_mean, linetype = "dotted", color = "black") +
        theme_minimal() +
        labs(title = paste0("Play Call Rating Visualization: ", input$x_axis, " vs ", input$y_axis, '\n Hover over point to see the Play Call'), 
             x = input$x_axis, y = input$y_axis) +
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 10))
      
      ggplotly(plot, tooltip = c("text", "x", "y")) %>%
        layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
      })
    })
#----plot stuff over---
  # Observers for buttons
  observeEvent(input$show_poss, {
    data <- read.csv('Possession Outcome Stats.csv')
    data <- data[,-1]  # Remove unnecessary columns if needed
    data <- data %>%
      rename(
        'Play Calls' = `PLAY.CALLS`,
        '# of Possessions' = total_calls,
        'FG' = fg_text,
        'FG%' = FG_percent,
        'eFG%' = eFG,
        '2pt' = two_text,
        '2pt%' = two_pt_percent,
        '3pt' = three_text,
        '3pt%' = three_pt_percent
      )
    output$metrics_table <- renderReactable({
      PlayCallsReactable1(data)
    })
  })
  
  observeEvent(input$show_rating, {
    data1 <- read.csv('Possession Rating Stats.csv')
    data1 <- data1[,-1]  # Remove unnecessary columns if needed
    data1 <- data1 %>%
      rename(
        'Play Calls' = `PLAY.CALLS`,
        '# of Possessions' = total_calls,
        'Foul Drawn Rate' = FD_rate,
        '1v1 Advantage Rate' = Advantage_rate,
        'Contested Rate' = Contested_rate,
        'Open Rate' = Open_rate,
        'Tov Rate' = TO_rate
      )
    
    output$metrics_table <- renderReactable({
      PlayCallsReactable2(data1)
    })
  })
  
  observeEvent(input$show_poss_grouped, {
    data2 <- read.csv('Possession Outcome Stats - Grouped.csv')
    data2 <- data2[,-1]  # Remove unnecessary columns if needed
    data2 <- data2 %>%
      rename(
        'Play Calls' = `PLAY.CALLS`,
        '# of Possessions' = total_calls,
        'FG' = fg_text,
        'FG%' = FG_percent,
        'eFG%' = eFG,
        '2pt' = two_text,
        '2pt%' = two_pt_percent,
        '3pt' = three_text,
        '3pt%' = three_pt_percent
      )
    output$grouped_metrics_table <- renderReactable({
      PlayCallsReactable1(data2)
    })
  })
  
  observeEvent(input$show_rating_grouped, {
    data3 <- read.csv('Possession Rating Stats - Grouped.csv')
    data3 <- data3[,-1]  # Remove unnecessary columns if needed
    data3 <- data3 %>%
      rename(
        'Play Calls' = `PLAY.CALLS`,
        '# of Possessions' = total_calls,
        'Foul Drawn Rate' = FD_rate,
        '1v1 Advantage Rate' = Advantage_rate,
        'Contested Rate' = Contested_rate,
        'Open Rate' = Open_rate,
        'Tov Rate' = TO_rate
      )
    
    output$grouped_metrics_table <- renderReactable({
      PlayCallsReactable2(data3)
    })
  })
  
  observeEvent(input$set_select, {
    select_set <- input$set_select
    csv_name_outcome <- paste0(select_set, ' Possession Outcome Metrics.csv')
    csv_name_rating <- paste0(select_set, ' Possession Rating Metrics.csv')
    data_outcome <- read.csv(csv_name_outcome)
    data_outcome <- data_outcome[,-1]
    data_rating <- read.csv(csv_name_rating)
    data_rating <- data_rating[,-1]
    observeEvent(input$show_poss_set, {
      output$set_comparisons_table <- render_gt({
        title_header <- glue("
        <div style='display: flex; align-items: center; justify-content: space-between;'>
          <img src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/North_Carolina_Tar_Heels_logo.svg/1200px-North_Carolina_Tar_Heels_logo.svg.png' style='height: 50px;'>
        <div style='text-align: center;'>
          <div style='font-size: 20px; font-weight: bold;'>{select_set} Set Metrics</div>
          <div style='font-size: 14px; font-weight: normal;'>Possession Outcome Stats</div>
        </div>
          <img src='https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Jumpman_logo.svg/400px-Jumpman_logo.svg.png' style='height: 50px;'>
        </div>
        ")
        tbl_poss <- data_outcome %>%
          gt() %>%
          cols_label(`PLAY.CALLS` = md('**Play Call**'), total_calls = md('**Poss.**'), PPP = md('**PPP**'), eFG = md('**eFG%**'), fg_text = md('**FG**'), FG_percent = md('**FG%**'), two_text = md('**2pt**'), two_pt_percent = md('**2pt%**'), three_text = md('**3pt**'), three_pt_percent = md('**3pt%**')) %>%
          tab_style(locations = cells_column_labels(), style = cell_text(align = 'center', decorate = 'underline')) %>%
          tab_style(locations = cells_body(vars(`PLAY.CALLS`)), style = cell_text(align = 'left')) %>%
          tab_style(locations = cells_body(vars(total_calls, PPP, eFG, fg_text, two_text, three_text, FG_percent, two_pt_percent, three_pt_percent)), style = cell_text(align = 'center')) %>%
          gt_add_divider(`PLAY.CALLS`, weight = px(1.5)) %>%
          gt_add_divider(eFG, weight = px(1)) %>%
          gt_add_divider(FG_percent, weight = px(1)) %>%
          gt_add_divider(two_pt_percent, weight = px(1)) %>%
          data_color(columns = vars(PPP), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_outcome$PPP), max(data_outcome$PPP)))) %>%
          data_color(columns = vars(eFG), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_outcome$eFG), max(data_outcome$eFG)))) %>%
          data_color(columns = vars(FG_percent), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_outcome$FG_percent), max(data_outcome$FG_percent)))) %>%
          data_color(columns = vars(two_pt_percent), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_outcome$two_pt_percent), max(data_outcome$two_pt_percent)))) %>%
          data_color(columns = vars(three_pt_percent), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_outcome$three_pt_percent), max(data_outcome$three_pt_percent)))) %>%
          tab_header(title = html(title_header))
      })
    })
    observeEvent(input$show_rating_set, {
      output$set_comparisons_table <- render_gt({
        title_header <- glue("
        <div style='display: flex; align-items: center; justify-content: space-between;'>
          <img src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/North_Carolina_Tar_Heels_logo.svg/1200px-North_Carolina_Tar_Heels_logo.svg.png' style='height: 50px;'>
        <div style='text-align: center;'>
          <div style='font-size: 20px; font-weight: bold;'>{select_set} Set Metrics</div>
          <div style='font-size: 14px; font-weight: normal;'>Possession Rating Stats</div>
        </div>
          <img src='https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Jumpman_logo.svg/400px-Jumpman_logo.svg.png' style='height: 50px;'>
        </div>
         ")
        tbl_rating <- data_rating %>%
          gt() %>%
          cols_label(`PLAY.CALLS` = md('**Play Call**'), total_calls = md('**Poss.**'), FD_rate = md('**Foul**'), Advantage_rate = md('**1v1 Advantage**'), Contested_rate = md('**Contested**'), Open_rate = md('**Open**'), TO_rate = md('**Tov**')) %>%
          tab_style(locations = cells_column_labels(), style = cell_text(align = 'center', decorate = 'underline')) %>%
          tab_style(locations = cells_body(vars(`PLAY.CALLS`)), style = cell_text(align = 'left')) %>%
          tab_style(locations = cells_body(vars(total_calls, FD_rate, Advantage_rate, Contested_rate, Open_rate, TO_rate)), style = cell_text(align = 'center')) %>%
          gt_add_divider(`PLAY.CALLS`, weight = px(1.5)) %>%
          tab_spanner(label = md('**Rates**'), columns = vars(FD_rate, Advantage_rate, Contested_rate, Open_rate, TO_rate)) %>%
          tab_style(locations = cells_column_spanners(), style = cell_text(align = 'center', decorate = 'underline')) %>%
          tab_header(title = html(title_header)) %>%
          data_color(columns = vars(FD_rate), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_rating$FD_rate), max(data_rating$FD_rate)))) %>%
          data_color(columns = vars(Advantage_rate), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_rating$Advantage_rate), max(data_rating$Advantage_rate)))) %>%
          data_color(columns = vars(Contested_rate), colors = scales::col_numeric(palette = c('#7bafd4', 'transparent'), domain = c(min(data_rating$Contested_rate), max(data_rating$Contested_rate)))) %>%
          data_color(columns = vars(Open_rate), colors = scales::col_numeric(palette = c("transparent", '#7bafd4'), domain = c(min(data_rating$Open_rate), max(data_rating$Open_rate)))) %>%
          data_color(columns = vars(TO_rate), colors = scales::col_numeric(palette = c('#7bafd4', 'transparent'), domain = c(min(data_rating$TO_rate), max(data_rating$TO_rate))))
      })
    })
  })
  
  observeEvent(input$select_set_1, {
    observeEvent(input$select_set_2, {
      set1 <- input$select_set_1
      set2 <- input$select_set_2
      observeEvent(input$show_poss_two, {
        set1_poss_comp <- read.csv('Possession Outcome Stats.csv')
        set1_poss_comp <- set1_poss_comp %>% filter(`PLAY.CALLS` == set1)
        set2_poss_comp <- read.csv('Possession Outcome Stats.csv')
        set2_poss_comp <- set2_poss_comp %>% filter(`PLAY.CALLS` == set2)
        set1_poss_comp <- set1_poss_comp[,-(1:2)]
        set2_poss_comp <- set2_poss_comp[,-(1:2)]
        comp_poss_df <- data.frame(Stat = c('# of Possessions', 'PPP', 'eFG%', 'FG', 'FG%', '2pt', '2pt%', '3pt', '3pt%'))
        set1_poss_comp <- as.data.frame(t(set1_poss_comp))
        set2_poss_comp <- as.data.frame(t(set2_poss_comp))
        colnames(set1_poss_comp) <- c(set1)
        colnames(set2_poss_comp) <- c(set2)
        comp_poss_df <- cbind(set1_poss_comp, comp_poss_df, set2_poss_comp)
        range1 <- sort(as.numeric(c(comp_poss_df[1,1], comp_poss_df[1,3])))
        range2 <- sort(as.numeric(c(comp_poss_df[2,1], comp_poss_df[2,3])))
        range3 <- sort(as.numeric(c(comp_poss_df[3,1], comp_poss_df[3,3])))
        range4 <- sort(as.numeric(c(comp_poss_df[5,1], comp_poss_df[5,3])))
        range5 <- sort(as.numeric(c(comp_poss_df[7,1], comp_poss_df[7,3])))
        range6 <- sort(as.numeric(c(comp_poss_df[9,1], comp_poss_df[9,3])))
        output$two_set_comparisons_table <- render_gt({
          title_header <- glue("
          <div style='display: flex; align-items: center; justify-content: space-between;'>
            <img src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/North_Carolina_Tar_Heels_logo.svg/1200px-North_Carolina_Tar_Heels_logo.svg.png' style='height: 50px;'>
          <div style='text-align: center;'>
            <div style='font-size: 20px; font-weight: bold;'>{set1} vs {set2}</div>
            <div style='font-size: 14px; font-weight: normal;'>Possession Outcome Stats</div>
          </div>
            <img src='https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Jumpman_logo.svg/400px-Jumpman_logo.svg.png' style='height: 50px;'>
          </div>
                               ")
          comp_poss_tbl <- comp_poss_df %>%
            gt() %>%
            tab_style(locations = cells_column_labels(), style = cell_text(align = 'center', weight = 'bold', decorate = 'underline')) %>%
            tab_style(locations = cells_body(vars(Stat)), style = cell_text(align = 'center', weight = 'bold', decorate = 'underline')) %>%
            gt_add_divider(Stat) %>%
            gt_add_divider(columns = 1) %>%
            tab_style(locations = cells_body(vars(-Stat)), style = cell_text(align = 'center')) %>%
            data_color(columns = c(1,3), rows = 1, palette = c('white', '#7bafd4'), direction = c('row'), domain = range1) %>%
            data_color(columns = c(1,3), rows = 2, palette = c('white', '#7bafd4'), direction = c('row'), domain = range2) %>%
            data_color(columns = c(1,3), rows = 3, palette = c('white', '#7bafd4'), direction = c('row'), domain = range3) %>%
            data_color(columns = c(1,3), rows = 5, palette = c('white', '#7bafd4'), direction = c('row'), domain = range4) %>%
            data_color(columns = c(1,3), rows = 7, palette = c('white', '#7bafd4'), direction = c('row'), domain = range5) %>%
            data_color(columns = c(1,3), rows = 9, palette = c('white', '#7bafd4'), direction = c('row'), domain = range6) %>%
            tab_header(title = html(title_header)) %>%
            cols_width(c(1,2,3)~px(150))
        })
      })
      observeEvent(input$show_rating_two, {
        set1_rating_comp <- read.csv('Possession Rating Stats.csv')
        set1_rating_comp <- set1_rating_comp %>% filter(`PLAY.CALLS` == set1)
        set1_rating_comp <- set1_rating_comp[,-(1:2)]
        set2_rating_comp <- read.csv('Possession Rating Stats.csv')
        set2_rating_comp <- set2_rating_comp %>% filter(`PLAY.CALLS` == set2)
        set2_rating_comp <- set2_rating_comp[,-(1:2)]
        comp_rating_df <- data.frame(Stat = c('# of Possessions', 'FD Rate', '1v1 Advantage Rate', 'Contested Rate', 'Open Rate', 'Tov Rate'))
        set1_rating_comp <- as.data.frame(t(set1_rating_comp))
        set2_rating_comp <- as.data.frame(t(set2_rating_comp))
        colnames(set1_rating_comp) <- c(set1)
        colnames(set2_rating_comp) <- c(set2)
        comp_rating_df <- cbind(set1_rating_comp, comp_rating_df, set2_rating_comp)
        range1 <- sort(as.numeric(c(comp_rating_df[1,1], comp_rating_df[1,3])))
        range2 <- sort(as.numeric(c(comp_rating_df[2,1], comp_rating_df[2,3])))
        range3 <- sort(as.numeric(c(comp_rating_df[3,1], comp_rating_df[3,3])))
        range4 <- sort(as.numeric(c(comp_rating_df[4,1], comp_rating_df[4,3])))
        range5 <- sort(as.numeric(c(comp_rating_df[5,1], comp_rating_df[5,3])))
        range6 <- sort(as.numeric(c(comp_rating_df[6,1], comp_rating_df[6,3])))
        output$two_set_comparisons_table <- render_gt({
          title_header <- glue("
          <div style='display: flex; align-items: center; justify-content: space-between;'>
            <img src='https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/North_Carolina_Tar_Heels_logo.svg/1200px-North_Carolina_Tar_Heels_logo.svg.png' style='height: 50px;'>
          <div style='text-align: center;'>
            <div style='font-size: 20px; font-weight: bold;'>{set1} vs {set2}</div>
            <div style='font-size: 14px; font-weight: normal;'>Possession Rating Stats</div>
          </div>
            <img src='https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Jumpman_logo.svg/400px-Jumpman_logo.svg.png' style='height: 50px;'>
          </div>
                               ")
          comp_rating_tbl <- comp_rating_df %>%
            gt() %>%
            tab_style(locations = cells_column_labels(), style = cell_text(align = 'center', weight = 'bold', decorate = 'underline')) %>%
            tab_style(locations = cells_body(vars(Stat)), style = cell_text(align = 'center', weight = 'bold', decorate = 'underline')) %>%
            gt_add_divider(Stat) %>%
            gt_add_divider(columns = 1) %>%
            tab_style(locations = cells_body(vars(-Stat)), style = cell_text(align = 'center')) %>%
            data_color(columns = c(1,3), rows = 1, palette = c('white', '#7bafd4'), direction = c('row'), domain = range1) %>%
            data_color(columns = c(1,3), rows = 2, palette = c('white', '#7bafd4'), direction = c('row'), domain = range2) %>%
            data_color(columns = c(1,3), rows = 3, palette = c('white', '#7bafd4'), direction = c('row'), domain = range3) %>%
            data_color(columns = c(1,3), rows = 4, palette = c('#7bafd4', 'white'), direction = c('row'), domain = range4) %>%
            data_color(columns = c(1,3), rows = 5, palette = c('white', '#7bafd4'), direction = c('row'), domain = range5) %>%
            data_color(columns = c(1,3), rows = 6, palette = c('#7bafd4', 'white'), direction = c('row'), domain = range6) %>%
            tab_header(title = html(title_header)) %>%
            cols_width(c(1,2,3)~px(150))
        })
      })
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
