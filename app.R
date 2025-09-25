# app.R — House Price Predictor (Tailored to your DAT303 script)

library(shiny)
library(caret)         
library(xgboost)       
library(randomForest) 
library(e1071)    

# --- Load your trained model bundle ---
if (!file.exists("model_bundle.rds")) {
  stop("Missing 'model_bundle.rds'. Please create it from your training script (see README).")
}
bundle <- readRDS("model_bundle.rds")
model <- bundle$model
preproc <- if (!is.null(bundle$preproc)) bundle$preproc else NULL
feature_names <- if (!is.null(bundle$feature_names)) bundle$feature_names else NULL

# Build a single-row data.frame from the UI inputs (MUST match your training features)
build_input_df <- function(input) {
  df <- data.frame(
    LND_SQFOOT     = as.numeric(input$LND_SQFOOT),
    TOT_LVG_AREA   = as.numeric(input$TOT_LVG_AREA),
    SPEC_FEAT_VAL  = as.numeric(input$SPEC_FEAT_VAL),
    RAIL_DIST      = as.numeric(input$RAIL_DIST),
    OCEAN_DIST     = as.numeric(input$OCEAN_DIST),
    WATER_DIST     = as.numeric(input$WATER_DIST),
    CNTR_DIST      = as.numeric(input$CNTR_DIST),
    SUBCNTR_DI     = as.numeric(input$SUBCNTR_DI),
    HWY_DIST       = as.numeric(input$HWY_DIST),
    age            = as.numeric(input$age),
    avno60plus     = as.numeric(input$avno60plus),
    month_sold     = as.numeric(input$month_sold),
    structure_quality = factor(input$structure_quality, levels = c("1","2","3","4","5"))
  )
  df
}

apply_preproc <- function(df) {
  if (is.null(preproc)) return(df)
  try({
    df <- predict(preproc, df)
    return(df)
  }, silent = TRUE)
  df
}

ui <- fluidPage(
  titlePanel("House Price Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("LND_SQFOOT", "Land Square Foot", value = 10000, min = 0, step = 100),
      numericInput("TOT_LVG_AREA", "Total Living Area (sqft)", value = 2000, min = 0, step = 10),
      numericInput("SPEC_FEAT_VAL", "Special Feature Value", value = 15000, min = 0, step = 100),
      numericInput("RAIL_DIST", "Distance to Rail (m)", value = 1000, min = 0, step = 10),
      numericInput("OCEAN_DIST", "Distance to Ocean (m)", value = 11000, min = 0, step = 10),
      numericInput("WATER_DIST", "Distance to Water (m)", value = 300, min = 0, step = 10),
      numericInput("CNTR_DIST", "Distance to City Centre (m)", value = 43000, min = 0, step = 10),
      numericInput("SUBCNTR_DI", "Distance to Sub-Centre (m)", value = 37500, min = 0, step = 10),
      numericInput("HWY_DIST", "Distance to Highway (m)", value = 18000, min = 0, step = 10),
      numericInput("age", "Property Age (years)", value = 60, min = 0, step = 1),
      numericInput("avno60plus", "Avg # of 60+ residents", value = 0, min = 0, step = 1),
      sliderInput("month_sold", "Month Sold", min = 1, max = 12, value = 6, step = 1),
      selectInput("structure_quality", "Structure Quality (1=Low, 5=High)", choices = c("1","2","3","4","5"), selected = "4"),
      actionButton("predict_btn", "Predict Price", class = "btn-primary")
    ),
    mainPanel(
      tags$h3("Estimated Price:"),
      uiOutput("price_out"),
      tags$hr(),
      tags$h4("Debug Info"),
      verbatimTextOutput("debug_out"),
      tags$p("Tip: This app expects the same feature names and factor levels as of the training pipeline.")
    )
  )
)

server <- function(input, output, session) {
  last_pred <- reactiveVal(NULL)
  
  observeEvent(input$predict_btn, {
    df <- build_input_df(input)
    
    if (!is.null(feature_names)) {
      missing_cols <- setdiff(feature_names, names(df))
      if (length(missing_cols) > 0) for (m in missing_cols) df[[m]] <- NA
      df <- df[, feature_names, drop = FALSE]  # enforce order
    }
    
    df_proc <- apply_preproc(df)
    pred <- try(predict(model, newdata = df_proc), silent = TRUE)
    
    if (inherits(pred, "try-error")) {
      last_pred(NULL)
      output$price_out <- renderUI({
        tags$div(style="color:#b00020;", "Prediction failed. Check model_bundle.rds.")
      })
    } else {
      if (is.data.frame(pred)) pred <- pred[[1]]
      price_val <- as.numeric(pred[1])
      last_pred(price_val)
      output$price_out <- renderUI({
        if (is.na(price_val)) {
          tags$div(style="color:#b00020;","Prediction returned NA.")
        } else {
          tags$div(style="font-size:2rem;font-weight:700;", paste0("$", format(round(price_val, 0), big.mark=",")))
        }
      })
    }
    
    output$debug_out <- renderPrint({
      list(
        input_row = df,
        processed_row = df_proc,
        feature_names_expected = feature_names,
        model_class = class(model)
      )
    })
  })
}

shinyApp(ui, server)


