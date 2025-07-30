# app.R

library(shiny)
library(shinydashboard)
library(DT)

# Load backend files
source("R/models.R")
source("R/modules/teacher_module.R")

ui <- dashboardPage(
  dashboardHeader(title = "School Timetable Generator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input", icon = icon("edit")),
      menuItem("Generate Timetable", tabName = "generate", icon = icon("cogs")),
      menuItem("View Timetable", tabName = "view", icon = icon("table")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "input",
              h2("Enter Teacher Information"),
              teacherModuleUI("teacher_ui")
      ),
      tabItem(tabName = "generate", h2("Run Scheduling Logic")),
      tabItem(tabName = "view", h2("Timetable Viewer")),
      tabItem(tabName = "export", h2("Export to PDF or Excel"))
    )
  )
)

server <- function(input, output, session) {
  # Initialize all reactive data models
  models <- init_models()
  
  # Activate teacher form module
  teacherModuleServer("teacher_ui", models)
}

shinyApp(ui, server)