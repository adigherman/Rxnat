library(shiny)
library(nitrcbot)

#nitrc_sets_all <- list_image_sets()
sets <- list("NITRC" = nitrc_sets_all[,c("ID","Name","Subjects","Description")])

ui <- fluidPage(
  titlePanel("neuroCluster"),
  sidebarLayout(
    sidebarPanel(
      selectInput("DatasetSelection","Select Image Dataset", c("NITRC", "HCP (TBA)"), multiple = FALSE),
      uiOutput("Sets")
    ),
    mainPanel(
      h3(textOutput("selected_dataset")),
      textOutput("selected_dataset_subjects_nr"),
      textOutput("selected_dataset_description")
    )
  )
)

server <- function(input, output) {
  output$Sets <- renderUI({
    selectInput("ProjectSelection","Select Image Project",sets[[input$DatasetSelection]]$ID)
  })

  output$selected_dataset <- renderText({
    paste0("Set ID: ",input$ProjectSelection);
  })

  output$selected_dataset_subjects_nr <- renderText({
    paste0("Subjects: ",sets[[input$DatasetSelection]]$Subjects[sets[[input$DatasetSelection]]$ID == input$ProjectSelection]);
  })

  output$selected_dataset_description <- renderText({
    paste0("Description: ",sets[[input$DatasetSelection]]$Description[sets[[input$DatasetSelection]]$ID == input$ProjectSelection]);
  })
}

shinyApp(ui = ui, server = server)
