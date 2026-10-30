if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
library(shiny)

set.seed(101)
initial_n <- 40

initial_types <- sample(c("Truck", "Delivery Van", "Sedan"), initial_n, replace = TRUE, prob = c(0.35, 0.45, 0.20))
initial_dist  <- round(runif(initial_n, 500, 2600), 1)
initial_kmpl  <- ifelse(initial_types == "Truck", rnorm(initial_n, 5.5, 0.4),
                        ifelse(initial_types == "Delivery Van", rnorm(initial_n, 11.2, 0.8),
                               rnorm(initial_n, 16.8, 1.1)))
initial_fuel  <- round(initial_dist / initial_kmpl, 1)

base_data <- data.frame(
  Vehicle_ID   = 1000 + seq_len(initial_n),
  Reg_No       = paste0("HR-", sample(10:99, initial_n, replace = TRUE), "-AB-", sample(1000:9999, initial_n)),
  Type         = initial_types,
  Distance_KM  = initial_dist,
  Fuel_L       = initial_fuel,
  Mileage_kmpl = round(initial_dist / initial_fuel, 2),
  Cost_INR     = round(initial_fuel * 95.50, 2),
  Status       = ifelse(initial_dist >= 2000, "Service Due", "Operational"),
  Audit_Alert  = ifelse(runif(initial_n) < 0.1, "High Fuel Burn", "Normal"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #F4F6F9; font-family: 'Segoe UI', Tahoma, sans-serif; }
      .header-box { background: #1E293B; color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
      .card-box { background: white; border: 1px solid #E2E8F0; border-radius: 8px; padding: 18px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
      .metric-badge { font-size: 22px; font-weight: 700; color: #0EA5E9; }
      .action-header { font-weight: 700; color: #1E293B; margin-bottom: 12px; }
    "))
  ),
  
  div(class = "header-box",
      h2("FleetPulse: Interactive Fleet Operations & Telematics Engine", style = "margin: 0; font-weight: 700;"),
      p("Live vehicle addition, predictive route planning, and workshop maintenance dispatch system", style = "color: #94A3B8; margin-top: 5px;")
  ),
  
  fluidRow(
    column(4,
           div(class = "card-box",
               h4(class = "action-header", "Action 1: Register New Vehicle"),
               p("Fleet mein naya vehicle inject karein aur metrics live update dekhein:"),
               textInput("new_reg", "Registration Number:", placeholder = "e.g., HR-26-BR-5555"),
               selectInput("new_type", "Vehicle Category:", choices = c("Truck", "Delivery Van", "Sedan")),
               numericInput("new_dist", "Distance Covered (KM):", value = 850, min = 50, max = 5000),
               numericInput("new_fuel", "Fuel Consumed (Litres):", value = 80, min = 5, max = 1000),
               actionButton("btn_add", "Inject Vehicle to Fleet", class = "btn-primary", width = "100%")
           ),
           
           div(class = "card-box",
               h4(class = "action-header", "Action 2: Trip Cost Predictor"),
               p("Naye route ke liye fuel aur cost calculate karein:"),
               selectInput("calc_type", "Vehicle Type:", choices = c("Truck", "Delivery Van", "Sedan")),
               sliderInput("calc_dist", "Planned Distance (KM):", min = 50, max = 1500, value = 400, step = 50),
               actionButton("btn_calc", "Calculate Trip Projection", class = "btn-info", width = "100%"),
               hr(),
               uiOutput("calc_result")
           )
    ),
    
    column(8,
           fluidRow(
             column(4, div(class = "card-box", div(style = "color: #64748B; font-size: 13px;", "TOTAL FLEET SIZE"), div(class = "metric-badge", textOutput("txt_fleet_count")))),
             column(4, div(class = "card-box", div(style = "color: #64748B; font-size: 13px;", "SERVICE QUEUE"), div(class = "metric-badge", style = "color: #EF4444;", textOutput("txt_service_queue")))),
             column(4, div(class = "card-box", div(style = "color: #64748B; font-size: 13px;", "AVG FLEET MILEAGE"), div(class = "metric-badge", style = "color: #10B981;", textOutput("txt_avg_mileage"))))
           ),
           
           div(class = "card-box",
               h4(class = "action-header", "Action 3: Workshop Dispatch & Operations"),
               fluidRow(
                 column(8, uiOutput("ui_service_select")),
                 column(4, style = "margin-top: 25px;",
                        actionButton("btn_dispatch", "Send to Service", class = "btn-warning", width = "100%"))
               )
           ),
           
           div(class = "card-box",
               tabsetPanel(
                 tabPanel("Live Charts", 
                          fluidRow(style = "margin-top: 15px;",
                                   column(6, plotOutput("plot_bar", height = "300px")),
                                   column(6, plotOutput("plot_scatter", height = "300px"))
                          )
                 ),
                 tabPanel("Active Fleet Telematics Log", 
                          style = "margin-top: 15px;",
                          downloadButton("btn_down", "Download Telematics CSV", class = "btn-success", style = "margin-bottom: 10px;"),
                          tableOutput("tbl_fleet")
                 )
               )
           )
    )
  )
)

server <- function(input, output, session) {
  
  fleet_state <- reactiveVal(base_data)
  
  observeEvent(input$btn_add, {
    req(input$new_reg, input$new_dist, input$new_fuel)
    
    current_df <- fleet_state()
    new_id <- max(current_df$Vehicle_ID) + 1
    new_kmpl <- round(input$new_dist / input$new_fuel, 2)
    new_cost <- round(input$new_fuel * 95.50, 2)
    new_status <- ifelse(input$new_dist >= 2000, "Service Due", "Operational")
    
    exp_kmpl <- ifelse(input$new_type == "Truck", 5.5, ifelse(input$new_type == "Delivery Van", 11.2, 16.8))
    new_alert <- ifelse(new_kmpl < (exp_kmpl * 0.75), "High Fuel Burn", "Normal")
    
    new_row <- data.frame(
      Vehicle_ID   = new_id,
      Reg_No       = toupper(input$new_reg),
      Type         = input$new_type,
      Distance_KM  = round(input$new_dist, 1),
      Fuel_L       = round(input$new_fuel, 1),
      Mileage_kmpl = new_kmpl,
      Cost_INR     = new_cost,
      Status       = new_status,
      Audit_Alert  = new_alert,
      stringsAsFactors = FALSE
    )
    
    fleet_state(rbind(new_row, current_df))
    showNotification(paste("Vehicle", new_row$Reg_No, "successfully added to active fleet!"), type = "message")
  })
  
  output$calc_result <- renderUI({
    req(input$btn_calc)
    isolate({
      kmpl <- ifelse(input$calc_type == "Truck", 5.5, ifelse(input$calc_type == "Delivery Van", 11.2, 16.8))
      est_fuel <- round(input$calc_dist / kmpl, 1)
      est_cost <- round(est_fuel * 95.50, 0)
      
      div(style = "background: #F1F5F9; padding: 12px; border-radius: 6px;",
          p(strong("Chassis: "), input$calc_type, style = "margin: 2px 0;"),
          p(strong("Expected Fuel: "), est_fuel, " Litres", style = "margin: 2px 0;"),
          p(strong("Projected Expense: "), "INR ", est_cost, style = "margin: 2px 0; color: #0369A1; font-weight: bold;")
      )
    })
  })
  
  output$ui_service_select <- renderUI({
    df <- fleet_state()
    due_vehicles <- df[df$Status == "Service Due", ]
    if (nrow(due_vehicles) == 0) {
      return(p(style = "color: #10B981; font-weight: 600;", "All vehicles are in operational condition. Zero units pending service."))
    }
    choices <- setNames(due_vehicles$Vehicle_ID, paste0(due_vehicles$Reg_No, " (", due_vehicles$Distance_KM, " KM)"))
    selectInput("sel_dispatch", "Select Vehicle for Service Booking:", choices = choices)
  })
  
  observeEvent(input$btn_dispatch, {
    req(input$sel_dispatch)
    df <- fleet_state()
    idx <- which(df$Vehicle_ID == as.numeric(input$sel_dispatch))
    if (length(idx) > 0) {
      df$Status[idx] <- "In Workshop"
      df$Distance_KM[idx] <- 0
      fleet_state(df)
      showNotification("Vehicle dispatched to workshop! Odometer reset for overhaul.", type = "warning")
    }
  })
  
  output$txt_fleet_count <- renderText({ paste(nrow(fleet_state()), "Units") })
  output$txt_service_queue <- renderText({ paste(sum(fleet_state()$Status == "Service Due"), "Units") })
  output$txt_avg_mileage <- renderText({ paste(round(mean(fleet_state()$Mileage_kmpl), 2), "km/L") })
  
  output$plot_bar <- renderPlot({
    counts <- table(fleet_state()$Type)
    barplot(counts, col = c("#0EA5E9", "#6366F1", "#EC4899"),
            main = "Live Category Distribution", ylab = "Active Units", border = NA)
  })
  
  output$plot_scatter <- renderPlot({
    df <- fleet_state()
    cols <- ifelse(df$Status == "Service Due", "#EF4444", "#10B981")
    plot(df$Distance_KM, df$Fuel_L, col = cols, pch = 19, cex = 1.3,
         main = "Odometer vs Fuel (Red = Service Due)",
         xlab = "Distance (KM)", ylab = "Fuel Consumed (L)")
    grid()
  })
  
  output$tbl_fleet <- renderTable({
    head(fleet_state(), 12)
  })
  
  output$btn_down <- downloadHandler(
    filename = function() { paste0("Fleet_Data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(fleet_state(), file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)