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
  createUsers <- "INSERT INTO %s (Datum, Schicht, Von, Priorität, Übernimmt, Kommentar) VALUES ('%s')"
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
saveData <- function(data, type="insert", input) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  
  # Construct the update query by looping over the data fields
  # condition "insert" saving into database
  # condition 2 delete row from database
  # condition 3 update Beglichen
  if(type=="insert"){
    query <- sprintf(
      "INSERT INTO %s (Datum, Schicht, Von, Priorität, Übernimmt, Kommentar) VALUES ('%s')",
      sqlitetable,
      paste(data, collapse = "', '")
    )
  } else if(type=="delete") { # condition for deleting a row from database, 
    query <- sprintf(
      "DELETE FROM switch_tour WHERE lfdNr = %i", data
    )
  } else if(type=="update") { # condition for turning 'Uebernimmt' variable to person's name
    query <- sprintf(
      paste("UPDATE switch_tour SET Übernimmt = ", input, " WHERE lfdNr = %i", sep = "'"), data
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
# print(responses)

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
  updateTextInput(session, inputId = "date_tab", value = unname(data["Datum"]))
  updateTextInput(session, inputId = "tour_tab", value = unname(data["Schicht"]))
  updateTextInput(session, inputId = "of_tab", value = unname(data["Von"]))
  updateTextInput(session, inputId = "prio_tab", value = unname(data["Priorität"]))
  updateTextInput(session, inputId = "takes_tab", value = unname(data["Übernimmt"]))
}

EmptyInputs <- function(session) {
  updateSelectizeInput(session, inputId = "of", selected = "")
  updateDateInput(session, inputId = "date", value = Sys.Date())
  updateSelectizeInput(session, inputId = "tour", selected = "")
  updateNumericInput(session, inputId = "priority", value = 2)
}

names <- c("Harald", 
           "Isolde", 
           "Gerlinde", 
           "Arnold", 
           "Dietrich")

##########################################################################################################
############################ USER INTERFACE ##############################################################
##########################################################################################################

ui = shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Das Schichtkarusell"),
  
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
          column(3,
                 selectizeInput("of", label = "Wer will was loswerden?", 
                                choices = c("Bitte wählen" = "",
                                            names)
                 ),
                 
                 conditionalPanel(
                   condition = "input.of != ''",
                   dateInput("date", "Datum", 
                             format = "yyyy-mm-dd", 
                             weekstart = 1, 
                             language = "de", 
                             value = Sys.Date())
                 )
          ),
          column(9,
                 conditionalPanel(
                   condition = "input.of != ''",
                   selectizeInput("tour", "Schicht", choices = c("Bitte wählen" = "",
                                                                 "früh",
                                                                 "mitte",
                                                                 "spät"))
                 ),
                 
                 conditionalPanel(
                   condition = "input.of != '' && input.tour != ''",
                   numericInput("priority", "Priorität", min = 1, max = 3, value = 2),
                   helpText("1 = super wichtig, 2 = mittel, 3 = wär schön, wenns klappt"),
                   textInput("comment", "Kommentar"),
                   helpText("Nach der Eingabe einfach auf",
                            "den Abschicken-Knopf drücken."),
                   actionButton("submit", "Abzugebende Schicht in Datenbank eintragen", icon = icon("send"))
                 )
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
        fluidRow(h2("Zeile in der Tabelle anklicken und über blauen Button unterhalb bearbeiten")),
        br(),
        # Create a new row for the table.
        fluidRow(DT::dataTableOutput("responses")),

        box(title = "Schicht übernehmen oder Zeile rauslöschen", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
          fluidRow(
            column(4,
                   shinyjs::disabled(textInput(inputId = "of_tab", "Von", ""))
                   ),
            column(3,
                   shinyjs::disabled(textInput(inputId = "date_tab", "Datum", ""))
                   ),
            column(3,
                   shinyjs::disabled(textInput(inputId = "tour_tab", "Schicht", ""))
                   ),
            column(2,
                   shinyjs::disabled(textInput(inputId = "prio_tab", "Priorität", ""))
            ),
            selectizeInput(inputId = "takes_tab", "Übernimmt", choices = c("Bitte wählen" = "",
                                                                           names)),
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
  # update input$tour if weekday is not a wednesday
  observeEvent(input$date, {
    if (wday(input$date, label = T) != "Wed") {
      updateSelectizeInput(session, "tour", label = "Schicht", choices = c("Bitte wählen" = "",
                                                                           "früh",
                                                                           "spät"))
    } else {
      updateSelectizeInput(session, "tour", label = "Schicht", choices = c("Bitte wählen" = "",
                                                                           "früh",
                                                                           "mitte",
                                                                           "spät"))
    }
  })
  
  # Click submit button and confirm with Yes
  observeEvent(input$BUTyes, {
    toggleModal(session, "confirm", toggle = "close")
    
    
    # stupid workaround: date has to be in a character string containing dates for visualize date in DT
    date <- as.character(input$date) %>%
      paste0(" 01:00:00") 
    # Save values to database...
    newdata <-
      c(date,
        input$tour,
        input$of,
        input$priority,
        "",
        input$comment
      )
    saveData(newdata)
    
    EmptyInputs(session)
  })
  
  #Press "delete row" button
  observeEvent(input$delete, {
    # Get the right ID selected row because selected row must not equal the ID in database
    #data <- ReadData()[input$responses_rows_selected, ]
    lfdNr = loadData()[input$responses_rows_selected, ][1,1]
    saveData(data = lfdNr, type = "delete")
    # Updates the editing fields below datatable
    #UpdateInputs(CreateDefaultRecord(), session)
    responses <- loadData()
  }, priority = 1)
  
  #Select row in table -> show details in "Übernehmen"-Inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      # maybe here it is not necessary to read in the data from CSV maybe driectly from responses RAM...
      data <- loadData()[input$responses_rows_selected, ]
      # Updates the editing fields below datatable
      UpdateInputs(data, session)
    }
  })
  
  # Update the database based on the "Übernehmen" dialog below the datatable
  observeEvent(input$update, {
    # Get the "lfdNr" of data
    lfdNr = loadData()[input$responses_rows_selected, ][1,1]
    saveData(data = lfdNr, type="update", input$takes_tab)
    # update responses variable
    responses <- loadData()
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update datatable view after submit button is clicked
    input$submit
    input$update
    input$delete
    # list(0, "asc"): date, list(4, "asc"): takes, list(3, "asc"): priority
    datatable(loadData()[,-1], options = list(pageLength = 50, order = list(list(0, 'asc'), list(4, "asc"), list(3, "asc"))), rownames = FALSE, selection = "single") %>%
      formatStyle("Übernimmt", backgroundColor = styleEqual(c("", names), c('red', rep('green', length(names))))) %>%
      formatStyle("Priorität", backgroundColor = styleEqual(c(1,2,3), c("red", "orange", "yellow"))) %>%
      formatDate("Datum")
  }
  )     
  
  
}


# Shiny app
shinyApp(ui = ui, server = server)
