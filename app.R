library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)
library(readxl)
library(reshape)
library(plotly)
library(tidyr)
library(readxl)
library(sf)
library(plotly)
library(shinydashboardPlus)
library(dashboardthemes)
library(xgboost)
library(caret)
library(ggplot2)
library(pROC)
library(ggcorrplot)
library(caret)
library(e1071)
library(randomForest)
library(pROC)
library(naivebayes)

# PREPROCESSING KLASIFIKASI
data = read.csv("Hotel Reservations.csv", sep =',')
data <- na.omit(data)
data$required_car_parking_space <- as.factor(data$required_car_parking_space)
data$repeated_guest <- as.factor(data$repeated_guest)
data$type_of_meal_plan <- as.factor(data$type_of_meal_plan)
data$market_segment_type <- as.factor(data$market_segment_type)
data$room_type_reserved <- as.factor(data$room_type_reserved)
data$booking_status <- as.factor(data$booking_status)
df = data[,-1]
selected_variables <- c(
  'lead_time', 'market_segment_type', 'no_of_adults', 'no_of_children',
  'no_of_weekend_nights', 'no_of_week_nights', 'room_type_reserved',
  'arrival_month', 'arrival_date', 'repeated_guest',
  'no_of_previous_cancellations', 'avg_price_per_room','booking_status'
)
df1 <- df[selected_variables]
df1$no_of_previous_cancellations = as.factor(df1$no_of_previous_cancellations)
###############################################################################################################################################

#DATA VISUALISASI
df <- read.csv("Hotel Reservations.csv", sep =',')

###############################################################################################################################################
# TRAINING MODEL

###############################################################################################################################################
# DASHBOARD SHINY

ui <- dashboardPage(
  skin = "blue-light",
  dashboardHeader(title = "Hotel Reservation Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("house")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("table-columns"),
               menuSubItem("Visualisasi", tabName = "visualisasi"),
               menuSubItem("Klasifikasi", tabName = "pemodelan")),
      menuItem("Database", tabName = "database", icon = icon("database"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem(tabName = "beranda",
              titlePanel(
                carousel(width = 12,
                         id="mycarousel",
                         carouselItem(
                           tags$img(src = "https://github.com/SFikri-Project/Tugas/raw/main/page.png")
                         ),
                         carouselItem(
                           tags$img(src = "https://github.com/SFikri-Project/Tugas/raw/main/profile1.png")
                         )
                )
              )
      ),
      tabItem(tabName = "dashboard"),
      tabItem(tabName = "visualisasi",
              fluidRow(box(title = h1(strong("DASHBOARD HOTEL RESERVATIONS")), status = "primary", solidHeader = FALSE,
                           width = 12)),
              fluidRow(
                box(title = strong('Filter Booking Status'),
                    selectInput("bookstatus", " ", choices = c("All" = "all", "Not Canceled" = 'Not_Canceled', 'Canceled' = 'Canceled')),
                    width = 6),
                box(title = strong('Pilih Rentang Waktu'),
                    dateRangeInput("dateRange", "", start = "2017-07-01", end = "2018-12-31",
                                   min = "2017-07-01", max = "2018-12-31", format = "yyyy-mm-dd", startview = "year"),
                    width = 6)
              ),
              fluidRow(
                infoBoxOutput(outputId = "jumlahpelanggan", width = 3),
                infoBoxOutput(outputId = "rataharga", width = 3),
                infoBoxOutput(outputId = "weekend", width = 3),
                infoBoxOutput(outputId = 'weekdays', width = 3)
              ),
              fluidRow(
                box(title = tags$b("Proporsi Pelanggan yang Kembali"), width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("PIEDF", height = 300)),
                box(title = tags$b("Jumlah Booking"), width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("LINEDF", height = 300))
              ),
              fluidRow(
                box(title = tags$b(tags$strong("Distribusi Variabel")), 
                    style = "text-align:center;", width = 6, status = "primary", solidHeader = TRUE,
                    selectInput(inputId = "selvio", 
                                label = "Pilih Variabel", 
                                choices = c("Harga Kamar" = 'avg_price_per_room', "Jarak Pemesanan (Hari)" = "lead_time")),
                    plotOutput("HISTDF", height = 220)
                ),
                box(title = tags$b(tags$strong("Distribusi Segmentasi Pasar")), 
                    style = "text-align:center;", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("BAR1DF", height = 294)
                )
              ),
              fluidRow(box(title = tags$b(tags$strong("Karateristik Tipe Kamar")), 
                           style = "text-align:center;", width = 12, status = "primary", solidHeader = TRUE,
                           selectInput(inputId = "selroom", 
                                       label = "Tipe Kamar", 
                                       multiple = TRUE,
                                       selected = c("Room_Type 1", "Room_Type 2", "Room_Type 3"),
                                       choices = c("Tipe 1"="Room_Type 1", "Tipe 2"="Room_Type 2", "Tipe 3"="Room_Type 3", "Tipe 4"="Room_Type 4",
                                                   "Tipe 5"="Room_Type 5", "Tipe 6"="Room_Type 6", "Tipe 7"="Room_Type 7")),
                           box(title = tags$b("Jumlah Tipe Kamar"), width = 3, status = "primary", solidHeader = FALSE,
                               plotOutput("barroom", height = 250)),
                           box(title = tags$b("Jumlah Banyak Pelanggan"), width = 3, status = "primary", solidHeader = FALSE,
                               plotOutput("cusroom", height = 250),
                               selectInput(inputId = "cussel",
                                           label = "",
                                           choices = c("Adult" = "no_of_adults", "Children" = "no_of_children"))),
                           box(title = tags$b("Proporsi Pilihan Tipe Hidangan"), width = 3, status = "primary", solidHeader = FALSE,
                               plotOutput("mealroom", height = 250)),
                           box(title = tags$b("Jumlah Permintaan Khusus dari Pelanggan"), width = 3, status = "primary", solidHeader = FALSE,
                               plotOutput("specroom", height = 250))
              ))
      ),
      tabItem(tabName = "pemodelan",
              fluidRow(
                box(
                  title = h1(strong("Prediksi Status Booking Pelanggan Hotel")),
                  status = "primary",
                  solidHeader = FALSE,
                  width = 12
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = strong("Input Fitur"),
                  fluidRow(
                    box(
                      selectInput("method","Metode",
                                  choices = c("Decision Tree" = "dt", 
                                              "Regresi Logistik" = "reglog",
                                              "Naive Bayes" = "nb",
                                              "XGBoost" = "xgb")
                      ),
                      width = 12
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput("num1",
                                   label = "Jarak Pemesanan (Hari):",
                                   value = NA),
                      
                      numericInput("num3",
                                   label = "Jumlah pelanggan dewasa:",
                                   value = NA),
                      numericInput("num4",
                                   label = "Jumlah pelanggan anak-anak:",
                                   value = NA),
                      selectInput("num10",
                                  label = "Pelanggan ulang:",
                                  choices = c("","Ya", "Tidak")
                      ),
                      selectInput("num11",
                                  label = "Pembatalan sebelumnya:",
                                  choices = c("","Ya", "Tidak")
                      ),
                      selectInput("num2",
                                  label = "Tipe pesanan:",
                                  choices = unique(df1$market_segment_type)
                                  
                      )
                    ),
                    column(
                      width = 6,
                      numericInput("num5",
                                   label = paste("Lama waktu menginap (weekend):"),
                                   value = NA),
                      numericInput("num6",
                                   label = "Lama waktu menginap (weekdays):",
                                   value = NA),
                      numericInput("num12",
                                   label = "Harga kamar:",
                                   value = NA),
                      selectInput("num7",
                                  label = "Tipe kamar:",
                                  choices = unique(df1$room_type_reserved)
                                  
                      ),
                      selectInput("num8",
                                  label = "Bulan datang:",
                                  choices = unique(sort(df1$arrival_month))
                                  
                      ),
                      selectInput("num9",
                                  label = "Tanggal datang:",
                                  choices = unique(sort(df1$arrival_date))
                      )
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                           actionButton("predict_btn", "Prediksi", class = "btn-primary"),
                           br(), br()
                    )
                  )
                ),
                box(title = strong("Hasil Prediksi"), width = 6,
                    plotOutput("probability_pie"),
                    textOutput("interpretation")
                )
                
              ),
              fluidRow(
                box(width = 6,title = strong("Ringkasan Model"), tableOutput("model_summary")),
                box(width = 6,title = strong("Fitur Terpenting"),plotOutput("model_summary_importance_plot"))
              ),
              
              fluidRow(
                box(title = strong("Kurva ROC (Rata-rata)"), width = 6,
                    plotOutput("roc_curve")),
                box(title = strong("Kurva ROC (Per Fold)"), width = 6,
                    plotOutput("roc_curve_folds"))
              )
      ),
      
      tabItem(tabName = "database",
              tabBox(id = "t2", width = 12,
                     tabPanel("Data", icon = icon("address-card"), dataTableOutput("dfb")),
                     tabPanel("Struktur", icon = icon("address-card"), verbatimTextOutput("structure")),
                     tabPanel("Summary", icon = icon("address-card"), verbatimTextOutput("sumari"))
              ))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  fitControl <- trainControl(method = "cv", 
                             number = 10, 
                             savePredictions = "all", 
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  
  set.seed(123)
  dt_model <- train(booking_status ~ ., data = df1, method = "rpart", trControl = fitControl, metric = "ROC")
  nb_model <- train(booking_status ~ ., data = df1, method = "naive_bayes", trControl = fitControl, metric = "ROC")
  logit_model <- train(booking_status ~ ., data = df1, method = "glm", trControl = fitControl, metric = "ROC")
  tuneGrid <- expand.grid(nrounds = 100, max_depth = 5, eta = 0.3, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 1)
  xgb_model <- train(booking_status ~ ., data = df1, method = "xgbTree", trControl = fitControl, metric = "ROC", tuneGrid = tuneGrid)
  
  # Fungsi untuk menghitung metrik dan ROC curve untuk setiap model
  get_model_metrics <- function(model) {
    resamples <- model$pred
    resamples <- na.omit(resamples)
    
    auc_values <- c()
    accuracy_values <- c()
    sensitivity_values <- c()
    specificity_values <- c()
    roc_curves <- list()
    folds <- unique(resamples$Resample)
    
    for (fold in folds) {
      fold_data <- subset(resamples, Resample == fold)
      cm <- confusionMatrix(fold_data$pred, fold_data$obs)
      
      accuracy_values <- c(accuracy_values, cm$overall['Accuracy'])
      sensitivity_values <- c(sensitivity_values, cm$byClass['Sensitivity'])
      specificity_values <- c(specificity_values, cm$byClass['Specificity'])
      
      roc_fold <- roc(response = fold_data$obs, predictor = fold_data$Not_Canceled, levels = rev(levels(fold_data$obs)))
      auc_fold <- auc(roc_fold)
      auc_values <- c(auc_values, auc_fold)
      roc_curves[[fold]] <- roc_fold
    }
    
    list(
      accuracy = accuracy_values,
      sensitivity = sensitivity_values,
      specificity = specificity_values,
      auc = auc_values,
      roc_all = roc(response = resamples$obs, predictor = resamples$Not_Canceled, levels = rev(levels(resamples$obs))),
      roc_folds = roc_curves
    )
  }
  
  # Mendapatkan metrik untuk setiap model
  dt_metrics <- get_model_metrics(dt_model)
  nb_metrics <- get_model_metrics(nb_model)
  logit_metrics <- get_model_metrics(logit_model)
  xgb_metrics <- get_model_metrics(xgb_model)
  
  get_varimp <- function(model) {
    importance <- varImp(model)
    importance_df <- as.data.frame(importance$importance)
    importance_df <- importance_df[order(-importance_df$Overall), , drop = FALSE]
    return(importance_df)
  }
  
  
  # Fungsi untuk membuat plot varImp
  plot_varimp <- function(importance_df) {
    top_10_importance <- head(importance_df, 10)
    
    ggplot(top_10_importance, aes(x = reorder(rownames(top_10_importance), -Overall), y = Overall)) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.7, color = "black") +
      labs(x = "Variable", y = "Importance", title = "Top 10 Variable Importance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  
  
  # VISUALISASI
  
  filtered_df <- reactive({
    df_filtered <- df
    
    # Apply booking status filter
    if (input$bookstatus == "Canceled") {
      df_filtered <- df_filtered[df_filtered$booking_status == "Canceled", ]
    } else if (input$bookstatus == "Not_Canceled") {
      df_filtered <- df_filtered[df_filtered$booking_status == "Not_Canceled", ]
    }
    
    # Create combined date and handle NA values
    df_filtered <- df_filtered %>%
      mutate(arrival_date_combined = make_date(arrival_year, arrival_month, arrival_date)) %>%
      filter(!is.na(arrival_date_combined)) %>%  # Remove rows with NA dates
      filter(arrival_date_combined >= input$dateRange[1] & arrival_date_combined <= input$dateRange[2])
    
    df_filtered
  })
  
  
  output$jumlahpelanggan <- renderInfoBox({
    value <- nrow(filtered_df())
    infoBox(tags$p("Jumlah Booking", style = "font-weight:bold;"),
            value = paste(value, "Booking"),
            color = "red",
            fill = TRUE,
            icon = icon("user-alt"))
  })
  
  output$rataharga <- renderInfoBox({
    value <- sum(filtered_df()$avg_price_per_room, na.rm = TRUE)
    value = round(value, 2)
    infoBox(tags$p("Total Pendapatan", style = "font-weight:bold;"),
            value = paste("$", value),
            color = "green",
            fill = TRUE,
            icon = icon("money-bill-wave-alt"))
  })
  
  output$weekend <- renderInfoBox({
    value <- mean(filtered_df()$no_of_weekend_nights, na.rm = TRUE)
    value = round(value, 2)
    infoBox(tags$p("Lama Menginap",br(),"(Weekend)", style = "font-weight:bold;"),
            value = paste(value, "Hari","/",ceiling(value), "Hari"),
            color = "blue",
            fill = TRUE,
            icon = icon("bed"))
  })
  
  output$weekdays <- renderInfoBox({
    value <- mean(filtered_df()$no_of_week_nights, na.rm = TRUE)
    value = round(value, 2)
    infoBox(tags$p("Lama Menginap",br(),"(Weekdays)", style = "font-weight:bold;"),
            value = paste(value, "Hari","/",ceiling(value), "Hari"),
            color = "yellow",
            fill = TRUE,
            icon = icon("calendar-week"))
  })
  
  output$PIEDF <- renderPlot({
    lang_data <- filtered_df()$repeated_guest
    proportions <- prop.table(table(lang_data))
    
    pie_data <- data.frame(lang_data = factor(names(proportions)),
                           proportion = as.numeric(proportions))
    
    ggplot(pie_data, aes(x = "", y = proportion, fill = lang_data)) +
      geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +  # Add border to the segments
      coord_polar("y", start = 0) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Repeated Guest Status") +  # Add title
      scale_fill_manual(values = c("#6495ed", "#E71D36"), 
                        labels = c("Bukan Pelanggan yang Kembali", "Pelanggan yang Kembali")) +
      geom_text(aes(label = scales::percent(proportion)), 
                position = position_stack(vjust = 0.5), 
                size = 5, color = "white", fontface = "bold") +  # Adjust text style
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title and adjust size
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) + 
      guides(fill = guide_legend(title = "Tipe Pelanggan"))  # Adjust legend title
  })
  
  output$LINEDF <- renderPlot({
    df_mod <- filtered_df()
    df_mod$arrival_date_combined <- make_date(df_mod$arrival_year, df_mod$arrival_month, df_mod$arrival_date)
    df_mod$arrival_year_month <- floor_date(df_mod$arrival_date_combined, "month")
    
    arrivals_per_month <- df_mod %>%
      group_by(arrival_year_month) %>%
      summarise(count = n())
    
    ggplot(arrivals_per_month, aes(x = arrival_year_month, y = count)) +
      geom_line(color = "#6495ed", size = 1) +
      geom_point(color = "#FF6347", size = 2) +
      labs(x = "Year-Month", y = "Jumlah Booking") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  output$BAR1DF <- renderPlot({
    df_sorted <- filtered_df() %>%
      count(market_segment_type) %>%
      arrange(desc(n))
    
    ggplot(df_sorted, aes(x = reorder(market_segment_type, -n), y = n)) +
      geom_bar(stat = "identity", fill = "#6495ed", color = "black", width = 0.6) +
      geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3, fontface = "bold") +
      labs(x = "Tipe Segemntasi Pasar", y = "Jumlah") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = 'dashed', color = 'grey'),
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(df_sorted$n) * 1.1)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  }, height = 300)
  
  output$HISTDF <- renderPlot({
    df <- filtered_df()
    selected_var <- input$selvio
    
    # Define a mapping for human-readable titles
    var_titles <- list(
      "lead_time" = "Jarak Pemesanan (hari)",
      "avg_price_per_room" = "Harga Kamar"
    )
    
    # Get the human-readable title for the selected variable
    plot_title <- var_titles[[selected_var]]
    
    # Calculate the density to find the maximal count
    density_data <- density(df[[selected_var]], na.rm = TRUE)
    max_density <- density_data$x[which.max(density_data$y)]
    max_count <- max(density_data$y)
    
    ggplot(df, aes_string(x = selected_var)) +
      geom_histogram(fill = "#6495ed", color = "black", bins = 30, alpha = 1) +
      geom_vline(xintercept = max_density, color = "red", linetype = "dashed", size = 1) +
      annotate("text", x = max_density, y = 0, label = round(max_density, 2), color = "red", size = 6, vjust = -1) +
      theme_minimal() +
      labs(x = plot_title, y = "Jumlah", title = paste("Distribusi dari", plot_title)) +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dashed', color = 'grey'),
        panel.grid.minor = element_blank()
      )
  })
  
  
  output$barroom <- renderPlot({
    df_filtered <- filtered_df() %>% filter(room_type_reserved %in% input$selroom)
    
    ggplot(df_filtered, aes(x = room_type_reserved)) +
      geom_bar(stat = "count", fill = "#6495ed", color = "black", width = 0.6) +  # Use stat = "count" to count the occurrences
      labs(x = "Tipe Kamar", y = "jumlah") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = 'dashed', color = 'grey'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5", color = NA),  # Custom panel background color
        plot.background = element_rect(fill = "#f5f5f5", color = NA)    # Custom plot background color
      ) +
      geom_bar(stat = "count", fill = NA, color = "#6495ed", width = 0.6)  # Add outline to the bars
  }, height = 330)
  
  
  
  output$cusroom <- renderPlot({
    df_filtered <- filtered_df() %>% filter(room_type_reserved %in% input$selroom)
    
    ggplot(df_filtered, aes_string(x = input$cussel)) +  
      geom_bar(stat = "count", fill = "#6495ED", color = "#6495ED", width = 0.6) +  # Using stat = "count" for bar heights
      labs(x = "Banyak Pelanggan", y = "jumlah") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = 'dashed', color = 'grey'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.background = element_rect(fill = "#f5f5f5", color = NA)
      )
  })
  
  
  output$mealroom <- renderPlot({
    df_filtered <- filtered_df() %>% filter(room_type_reserved %in% input$selroom)
    
    meal_data <- df_filtered$type_of_meal_plan
    proportions <- prop.table(table(meal_data))
    
    pie_data <- data.frame(meal_type = factor(names(proportions)),
                           proportion = as.numeric(proportions))
    
    ggplot(pie_data, aes(x = "", y = proportion, fill = meal_type)) +
      geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +  # Add border to the segments
      coord_polar("y", start = 0) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Meal Plan") +  # Add title
      scale_fill_manual(values = c("#6495ed", "#ff6347", "#32cd32", "#ffd700")) +
      geom_text(aes(label = scales::percent(proportion)), 
                position = position_stack(vjust = 0.5), 
                size = 4, color = "white", fontface = "bold") +  # Adjust text style
      theme(
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5", color = NA),  # Custom panel background color
        plot.background = element_rect(fill = "#f5f5f5", color = NA)    # Custom plot background color
      )
  }, height = 300)
  
  
  output$specroom <- renderPlot({
    df_filtered <- filtered_df() %>% filter(room_type_reserved %in% input$selroom)
    
    # Calculate counts and reorder the factor levels
    spec_requests <- df_filtered %>% 
      count(no_of_special_requests) %>%
      arrange(desc(no_of_special_requests)) %>%
      mutate(no_of_special_requests = factor(no_of_special_requests, levels = no_of_special_requests))
    
    ggplot(spec_requests, aes(x = no_of_special_requests, y = n)) +  
      geom_bar(stat = "identity", fill = "#6495ED", color = "#6495ED", width = 0.6) +  # Using stat = "identity" for bar heights
      labs(x = "Jumlah Perimntaan Khusus", y = "jumlah") +
      theme_minimal(base_size = 10) +
      theme(
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = 'dashed', color = 'grey'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5", color = NA),
        plot.background = element_rect(fill = "#f5f5f5", color = NA)
      ) +
      coord_flip()  # Flip the coordinates to make the plot horizontal
  }, height = 330)
  
  # KLASIFIKASI
  user_input <- reactive({
    data.frame(
      lead_time = as.numeric(input$num1),
      market_segment_type = as.factor(input$num2),
      no_of_adults = as.numeric(input$num3),
      no_of_children = as.numeric(input$num4),
      no_of_weekend_nights = as.numeric(input$num5),
      no_of_week_nights = as.numeric(input$num6),
      room_type_reserved = as.factor(input$num7),
      arrival_month = as.numeric(input$num8),
      arrival_date = as.numeric(input$num9),
      repeated_guest = as.factor(ifelse(input$num10 == "Ya", 1, 0)),
      no_of_previous_cancellations = as.factor(ifelse(input$num11 == "Ya", 1, 0)),
      avg_price_per_room = as.numeric(input$num12),
      stringsAsFactors = FALSE
    )
  })
  
  selected_model <- eventReactive(input$predict_btn, {
    switch(input$method,
           dt = dt_model,
           nb = nb_model,
           reglog = logit_model,
           xgb = xgb_model)
  })
  
  selected_metrics <- eventReactive(input$predict_btn, {
    switch(input$method,
           dt = dt_metrics,
           nb = nb_metrics,
           reglog = logit_metrics,
           xgb = xgb_metrics)
  })
  
  prediction <- eventReactive(input$predict_btn, {
    predict(selected_model(), user_input(), type = "prob")
  })
  
  
  # Output probability pie chart
  output$probability_pie <- renderPlot({
    prob <- prediction()
    pie_data <- data.frame(
      status = c("Canceled", "Not_Canceled"),
      probability = c(prob[1, "Canceled"], prob[1, "Not_Canceled"]), height = 1000
    )
    
    ggplot(pie_data, aes(x = "", y = probability, fill = status)) +
      geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +  # Add border to the segments
      coord_polar("y", start = 0) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Booking Status", 
           title = "Persentase Kemungkinan Melakukan Pembatalan") +  # Add title
      scale_fill_manual(values = c("#E71D36","#6495ed"), 
                        labels = c("Canceled", "Not_Canceled")) +
      geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), 
                position = position_stack(vjust = 0.5), 
                size = 5, color = "white", fontface = "bold") +  # Adjust text style
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title and adjust size
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) + 
      guides(fill = guide_legend(title = "Booking Status"))  # Adjust legend title
  })
  
  output$interpretation <- renderText({
    prob <- prediction()
    cancel_prob <- round(prob[1, "Canceled"] * 100, 2)
    not_cancel_prob <- round(prob[1, "Not_Canceled"] * 100, 2)
    paste("Pelanggan berkemungkinan ", cancel_prob, "% untuk melakukan pembatalan reservasi dan ", not_cancel_prob, "% tidak melakukan pembatalan reservasi.", sep = "")
  })
  
  output$model_summary <- renderTable({
    req(selected_metrics())
    metrics_data <- selected_metrics()
    summary_df <- data.frame(
      Fold = 1:length(metrics_data$accuracy),
      Accuracy = round(metrics_data$accuracy, 4),
      Sensitivity = round(metrics_data$sensitivity, 4),
      Specificity = round(metrics_data$specificity, 4),
      AUC = round(metrics_data$auc, 4)
    )
    summary_df
  })
  
  
  output$model_summary_importance_plot <- renderPlot({
    req(selected_model())
    model <- selected_model()
    if (inherits(model, "train")) {
      importance_df <- get_varimp(model)
      plot_varimp(importance_df)  # Menampilkan plot varImp
    } else {
      plot(NULL)  # Jika model tidak tersedia, tampilkan plot kosong
    }
  })
  
  output$roc_curve <- renderPlot({
    req(selected_metrics())
    metrics_data <- selected_metrics()
    roc_all <- metrics_data$roc_all
    plot.roc(roc_all, col = "blue", main = "Kurva Rata-rata ROC", print.auc=TRUE, auc.polygon=TRUE, auc.polygon.col="skyblue", legacy.axes=TRUE)
  })
  
  output$roc_curve_folds <- renderPlot({
    req(selected_metrics())
    metrics_data <- selected_metrics()
    folds <- names(metrics_data$roc_folds)
    par(mfrow = c(5, 2))  
    for (fold in folds) {
      roc_fold <- metrics_data$roc_folds[[fold]]
      plot(roc_fold, main = paste("Kurva ROC Per Fold", fold), col = "blue", print.auc = TRUE, auc.polygon = TRUE, auc.polygon.col = "skyblue", legacy.axes = TRUE)
    }
  })
  
  output$structure = renderPrint(
    str(data)
  )
  output$sumari = renderPrint(
    summary(data)
  )
  output$dfb = renderDataTable(
    data,extensions = 'Buttons',options=list(dom='Bfrtip',buttons=list('copy','pdf','excel','csv','print'))
  )
  
  
}


shinyApp(ui, server)
