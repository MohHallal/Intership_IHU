library(shiny)
shinyUI(fluidPage(
    titlePanel("MSProfiler database manipulation"),
    sidebarLayout(
        sidebarPanel(
          fluidRow(column(width = 9, h1("Adding samples"))),
          fileInput("file","Select the csv file of your samples",buttonLabel = "Browse...",placeholder = "No file selected"),
          actionButton("addButton","Add the subset"),
          verbatimTextOutput("message_add"),
          fluidRow(column(width = 9, h1("Deleting a sample"))),
          textInput("sample_name", label = "Enter the sample name to delete:", value = ""),
          actionButton("deleteButton","Delete the sample"),
          verbatimTextOutput("message_delete"),
          fluidRow(column(width = 9, h1("Interrogating the database"))),
          textInput("Sample_Name", label = "Sample name:", value = NA),
          textInput("family", label = "Family:", value = NA),
          textInput("genus", label = "Genus:", value = NA),
          textInput("species", label = "Species:", value = NA),
          textInput("arthropod_type", label = "Arthropod type:", value = NA),
          textInput("body_part", label = "Body part:", value = NA),
          textInput("dvlpt_stage", label = "Developmental stage:", value = NA),
          textInput("protocol_id", label = "Protocol ID:", value = 0),
          textInput("location_id", label = "Location ID:", value = 0),
          actionButton("filter","Filter"),
          actionButton("reset","Reset")
          ),
        mainPanel(
          verbatimTextOutput("size_database"),
          fluidRow(
            column(width = 6, plotOutput("plot_family",width = "600px", height = "450px")),
            column(width = 6, plotOutput("plot_genus",width = "600px", height = "450px"))
          ),
          fluidRow(
            column(width = 6, plotOutput("plot_body_part",width = "600px", height = "450px")),
            column(width = 6, plotOutput("plot_arthropod_type",width = "600px", height = "450px"))
          ),
          tableOutput("result")
        )
    )
))
