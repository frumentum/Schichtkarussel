library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(RSQLite)
library(DT)
library(ggplot2)
library(scales)

# Print current working directory
print(getwd())

# Path to sqlite database
sqlitePath <- "test.db"
# Name of the sqlite table
sqlitetable <- "switch_tour"

# Function for creating a new database if none already exists
createDatabase <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  createUsers <- "INSERT INTO %s (Datum, Schicht, Von, Uebernimmt) VALUES ('%s')"
  createResponses <- ""
  createCounters <- ""
  
  # Submit the create queries and disconnect from database
  dbGetQuery(db, createUsers)
  dbGetQuery(db, createResponses)
  dbGetQuery(db, createCounters)
  dbDisconnect(db) 
}

generateRandomData <- function(number) {
  for (i in 1:number) {
    #Tag <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
    Datum <- format(Sys.time(), "%Y.%m.%d %H:%M:%S")
    Schicht <- c("früh", "mitte", "spät")
    Von <- c("Harald", "Isolde", "Gerlinde", "Arnold", "Dietrich")[sample(1:5, 1, replace = T)]
    Uebernimmt <- c("Harald", "Isolde", "Gerlinde", "")[sample(1:4, 1, replace = T)]
    entry <- c(Datum, Schicht, Von, Uebernimmt)
    saveData(entry)
  }
}

# Function for saving the data in sqlite database
saveData <- function(data, type="insert") {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  
  # Construct the update query by looping over the data fields
  # condition "insert" saving into database
  # condition 2 delete row from database
  # condition 3 update Beglichen
  if(type=="insert"){
    query <- sprintf(
      "INSERT INTO %s (Datum, Schicht, Von, Uebernimmt) VALUES ('%s')",
      sqlitetable,
      paste(data, collapse = "', '")
    )
  } else if(type=="delete") { # condition for deleting a row from database, 
    query <- sprintf(
      "DELETE FROM switch_tour WHERE lfdNr = %i", data
    )
  } else if(type=="update") { # condition for turning Uebernimmt variable to TRUE
    query <- sprintf(
      "UPDATE switch_tour SET Uebernimmt != '' WHERE lfdNr = %i", data
    )
  }
  
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

# Function for loading the data from sqlite database
loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", sqlitetable)
  # Submit the fetch query and disconnect
  responses <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(responses)
}

# Load database at start for populating the selectInputs of user interface
responses <- loadData()
print(responses)

#READ
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

# Fill input fields with the values of the selected record in the table
# option "Uebernimmt" under datatable view
# these are seperate fields for editing the data
UpdateInputs <- function(data, session) {
  updateTextInput(session, inputId = "id_tab", value = unname(data["lfdNr"]))
  updateTextInput(session, inputId = "date_tab", value = unname(data["Datum"]))
  updateTextInput(session, inputId = "tour_tab", value = unname(data["Schicht"]))
  updateTextInput(session, inputId = "of_tab", value = unname(data["Von"]))
  updateSelectInput(session, inputId = "takes_tab", value = data["Uebernimmt"])
}

EmptyInputs <- function(session) {
  updateSelectizeInput(session, inputId = "of", selected = "Bitte wählen")
  updateDateInput(session, inputId = "date", value = Sys.Date())
  updateSelectizeInput(session, inputId = "tour", selected = "Bitte wählen")
}

##########################################################################################################
############################ USER INTERFACE ##############################################################
##########################################################################################################

ui = shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Die Schichttausch Datenbank"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Tabelle", tabName = "tabelle", icon = icon("table")),
    menuItem("Eingabe", tabName = "eingabe", icon = icon("edit"))
  )),
  
  dashboardBody(
    shinyjs::useShinyjs(), #use shiny js to disable the ID field
    tabItems(
      ############## INPUT TAB ##########################
      tabItem(
        tabName = "eingabe",
        fluidRow(
          selectizeInput("of", label = "Wer will was loswerden?", 
                         choices = c("Bitte wählen",
                                     "Harald", 
                                     "Isolde", 
                                     "Gerlinde", 
                                     "Arnold", 
                                     "Dietrich")
          ),
          
          conditionalPanel(
            condition = "input.of != 'Bitte wählen'",
            dateInput("date", "Datum", 
                      format = "yyyy-mm-dd", 
                      weekstart = 1, 
                      language = "de", 
                      value = Sys.Date())
          ),
          
          conditionalPanel(
            condition = "input.of != 'Bitte wählen' && input.date != Sys.Date()",
            selectizeInput("tour", "Schicht", choices = c("Bitte wählen",
                                                          "früh",
                                                          "mitte",
                                                          "spät"))
          ),
          
          conditionalPanel(
            condition = "input.date != Sys.Date() && input.tour != 'Bitte wählen' && input.of != 'Bitte wählen'",
            helpText("Nach der Eingabe einfach auf",
                     "den Abschicken-Knopf drücken."),
            actionButton("submit", "Abzugebende Schicht in Datenbank eintragen", icon = icon("send"))
          ),

        # dialog forcing user to check the input values (triggered by clicking on submit)
        bsModal(
          id = "confirm",
          title = "In Datenbank übernehmen?",
          trigger = "submit",
          HTML("Bitte überprüfe nochmal deine Eingabe!<br>Möchtest du den neuen Eintrag zur Datenbank hinzufügen?
               <br>Danke.<br>
               "),
          actionButton("BUTyes", "Jaaaa!")
          )
        
        )
      ),
      
      
      
      ############# TABLE TAB #############################
      tabItem(
        tabName = "tabelle",
        # Create a new row for the table.
        fluidRow(DT::dataTableOutput("responses")),

        box(title = "Änderungen in der Tabelle vornehmen", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
          fluidRow(
            column(2,
                   shinyjs::disabled(textInput(inputId = "id_tab", "lfdNr", ""))
                   ),
            column(4,
                   shinyjs::disabled(textInput(inputId = "of_tab", "Von", ""))
                   ),
            column(3,
                   shinyjs::disabled(textInput(inputId = "date_tab", "Datum", ""))
                   ),
            column(3,
                   shinyjs::disabled(textInput(inputId = "tour_tab", "Schicht", ""))
                   ),
            selectizeInput(inputId = "change_tab", "Übernimmt", choices = c("",
                                                                            "Harald", 
                                                                            "Isolde", 
                                                                            "Gerlinde")),
            actionButton("update", "Änderung übernehmen", icon = icon("send")), 
            actionButton("delete", "Zeile löschen (!!!)", icon = icon("trash"))
                   )
          )
        )
    ))
  ))

##########################################################################################################
############################ S E R V E R #################################################################
##########################################################################################################

server <- function(input, output, session) {
  
  # input fields are treated as a group
  # formData <- reactive({
  #   sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  # })
  
  # Click submit button and confirm with Yes
  observeEvent(input$BUTyes, {
    toggleModal(session, "confirm", toggle = "close")
    # Save values to database...
    newdata <-
      c(format(input$date, "%Y.%m.%d"),
        input$tour,
        input$of,
        ""
      )
    # print input values for debugging...
    #print(newdata)
    saveData(newdata)
    
    # Deactivated emptying the input fields for debugging...
    #EmptyInputs(session)
  })
  
  #Select row in table -> show details in "Beglichen"-Inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      # maybe here it is not necessary to read in the data from CSV maybe driectly from responses RAM...
      data <- loadData()[input$responses_rows_selected, ]
      # Updates the editing fields below datatable
      UpdateInputs(data, session)
    }
  })
  
  # Update the database based on the "Begleichen" dialog below the datatable
  observeEvent(input$update, {
    # Get the "lfdNr" of data
    lfdNr = loadData()[input$responses_rows_selected, ][1,1]
    saveData(data = lfdNr, type="update")
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    loadData() #%>%
      #formatStyle("EinAus", target = "row", backgroundColor = styleEqual(c("Ausgabe", "Einnahme"), c('gray', 'yellow')))
  }, server = FALSE, selection = "single", options = list(order = list(1, 'desc'), pageLength = 50), rownames = FALSE
  
  #colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
  
}


# Shiny app
shinyApp(ui = ui, server = server)