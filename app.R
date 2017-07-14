library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(RSQLite)
library(DT)

# Print current working directory
print(getwd())

# Path to sqlite database
sqlitePath <- "test.db"
# Name of the sqlite table
sqlitetable <- "in_out"

# Function for creating a new database if none already exists
createDatabase <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  createUsers <- "INSERT INTO %s (Datum, Bewohner, EinAus, Kategorie, billdate, Wert, Kommentar, Beglichen) VALUES ('%s')"
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
    Bewohner <-
      c("Chris", "Sophie", "Jo", "Julia", "Miri")[sample(1:5, 1, replace = T)]
    Datum <- format(Sys.time(), "%Y.%m.%d %H:%M:%S")
    Wert <- sample(1:100, 1, replace = T)
    Kategorie <-
      c("Essen",
        "Fahrtkosten",
        "Kueche",
        "Party",
        "Spende",
        "Materialien")[sample(1:6, 1, replace = T)]
    entry <- c(Datum, Bewohner, "Ausgabe", Kategorie, Wert, TRUE)
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
  if(type=="insert"){
    query <- sprintf(
      "INSERT INTO %s (Datum, Bewohner, EinAus, Kategorie, billdate, Wert, Kommentar, Beglichen) VALUES ('%s')",
      sqlitetable,
      #paste(names(data), collapse = "', '")
      paste(data, collapse = "', '")
      #     paste(names(data), collapse = ", "),
      #     paste(data, collapse = "', '")
    )
  } else if(type=="delete") { # condition for deleting a row from database, 
    query <- sprintf(
      "DELETE FROM in_out WHERE lfdNr = %i", data
    )
  } else if(type=="update") {
    query <- sprintf(
      "UPDATE in_out SET Beglichen = 'TRUE' WHERE lfdNr = %i", data
    )
    # Just for debugging, prints the data, in that case the row that is selected
    print(data)
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


# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
GetTableMetadata <- function() {
  fields <- c(id = "Id",
              date = "Datum",
              name = "Name",
              in_out = "Einnahme / Ausgabe",
              category = "Kategorie",
              billdate = "Miet- / Rechnungsdatum",
              value = "Wert",
              comment = "Kommentar",
              done = "Beglichen")
  
  result <- list(fields = fields)
  return (result)
}

#CREATE
CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

#READ
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

#UPDATE
UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}

#DELETE
DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}

# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(date = data["date"],
                      name = data["name"],
                      in_out = data["in_out"],
                      category = data["category"],
                      billdate = data["billdate"],
                      value = as.integer(data["value"]),
                      comment = data["comment"],
                      done = as.logical(data["done"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

# Fill input fields with the values of the selected record in the table
# these are seperate fields for editing below the data table
UpdateInputs <- function(data, session) {
  updateTextInput(session, inputId = "id_tab", value = unname(data["lfdNr"]))
  updateTextInput(session, inputId = "name_tab", value = unname(data["Bewohner"]))
  updateTextInput(session, inputId = "category_tab", value = unname(data["Kategorie"]))
  updateTextInput(session, inputId = "value_tab", value = unname(data["Wert"]))
  updateCheckboxInput(session, inputId = "done_tab", value = as.logical(data["Beglichen"]))
}

EmptyInputs <- function(session) {
  updateSelectInput(session, inputId = "name", selected = "Bitte waehlen")
  updateSelectInput(session, inputId = "in_out", selected = "Bitte waehlen")
  updateSelectInput(session, inputId = "category", selected = "Bitte waehlen")
  updateDateInput(session, inputId = "billdate", value = NULL)
  updateNumericInput(session, inputId = "value", value = 0)
  updateTextInput(session, inputId = "comment", value = "")
}

##########################################################################################################
############################ USER INTERFACE ##############################################################
##########################################################################################################

ui = shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Spinnerei Kollnau"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Eingabe", tabName = "eingabe", icon = icon("edit")),
    menuItem("Tabelle", tabName = "tabelle", icon = icon("table")),
    menuItem("Ausgabe", tabName = "ausgabe", icon = icon("line-chart"))
  )),
  
  dashboardBody(
    shinyjs::useShinyjs(), #use shiny js to disable the ID field
    tabItems(
      ############## INPUT TAB ##########################
      tabItem(
        tabName = "eingabe",
        fluidRow(
          selectizeInput("name", label = "Name", choices = c(
                                                          "Bitte waehlen",
                                                          "Christian Heymer",
                                                          "Janosch Hüttner",
                                                          "Chris Madeira Noronha",
                                                          "Johannes Reiter",
                                                          "Sophie Mailänder",
                                                          "Miriam Zeltner",
                                                          "Jakob Hehl",
                                                          "Amir",
                                                          "Szofi",
                                                          "Felix Lindicke"
                                                          )
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen'",
            selectInput(
              "in_out",
              label = "Aus- oder Einnahme",
              choices = c("Bitte waehlen", "Einnahme", "Ausgabe")
            )
          ),
          
          # choose category of transaction
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen'",
            selectInput(
              "category",
              label = "Kategorie",
              choices = c("Bitte waehlen",
                          "Party",
                          "Spende",
                          "Miete")
            )
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen'",
            dateInput("billdate", "Datum der Miete / Rechnung", value = "", format = "yyyy-mm-dd", language = "de")
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen' && input.billdate != ''",
            numericInput("value", "Geldbetrag", value = 0)
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen' && input.billdate != 0 && input.value != 0",
            textInput("comment", "Kommentar", NULL),
            # Implement later... for making "Beglichen" immediately
            #checkboxInput("done", "Beglichen", FALSE),
            helpText("Nach der Eingabe einfach auf",
                     "den Abschicken-Knopf druecken."),
            actionButton("submit", "Abschicken", icon = icon("send"))
          ),

        # dialog forcing user to check the input values (triggered by clicking on submit)
        bsModal(
          id = "confirm",
          title = "In Datenbank übernehmen?",
          trigger = "submit",
          #size = "small",
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
        
        # Create a new Row in the UI for selectInputs
        # fluidRow(
        #   column(3,
        #          dateRangeInput("daterange", "Datum")),
        #   column(2,
        #          selectInput("user", "Benutzer", c(
        #            "Alles", unique(as.character(responses$Bewohner))
        #          )))
        #   ),
        
        # Create a new row for the table.
        fluidRow(DT::dataTableOutput("responses")),

        box(title = "Begleichen", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
          fluidRow(#actionButton("delete", "Zeile löschen"), # Implement the delete feature later maybe in an extra box
                  shinyjs::disabled(textInput(inputId = "id_tab", "lfdNr", "")),
                  shinyjs::disabled(textInput(inputId = "name_tab", "Name", "")),
                  shinyjs::disabled(textInput(inputId = "category_tab", "Kategory", "")),
                  shinyjs::disabled(textInput(inputId = "value_tab", "Wert", "")),
                  checkboxInput(inputId = "done_tab", "Beglichen", FALSE),
                  actionButton("update", "Änderung übernehmen", icon = icon("send")) # Was for submitting the change in checkbox input... maybe possible submitting while clicking the checkbox
                   )
          )
        ),
      tabItem(tabName = "ausgabe",
              h2("Ausgabe"))
    ))
  ))

##########################################################################################################
############################ S E R V E R #################################################################
##########################################################################################################

server <- function(input, output, session) {
  
  # Update selectInput "category" after choosing selectInput "in_out"
  # Implement later the "Optionen" for in and outs that are not often used like Wasser, Strom ...
  # observeEvent(input$in_out, {
  #   if (input$in_out == "Ausgabe" && input$check == TRUE) {updateSelectInput(session, "category", choices = c("Bitte waehlen", "Essen", "Fahrtkosten", "Party", "Strom", "Holz", "Gas", "Wasser", "Telefon"))}
  #   else if (input$in_out == "Ausgabe" && input$check == FALSE) {
  #     updateSelectInput(session, "category", choices = c("Bitte waehlen", "Essen", "Fahrtkosten", "Party"))
  #   } else {updateSelectInput(session, "category", choices = c("Bitte waehlen", "Party", "Spende", "Miete"))}
  # })
  
  # Update selectInput "category" after choosing selectInput "in_out"
  observeEvent(input$in_out, {
    if (input$in_out == "Ausgabe"){
      updateSelectInput(session, "category", choices = list(c("Bitte waehlen"), "Allgemein" = c("Essen", "Verbrauchsmaterialien", "Fahrtkosten"), "Verwaltung" = c("Miete", "Rueckbaukonto", "Kontoführungsgebühr", "Party", "Baumaterialien", "Überweisung Foodcoop", "Brennholz", "GEZ", "Strom", "Wasser", "Internet & Telefon", "Gas")))
      #updateSelectInput(session, "category", choices = c("Bitte waehlen", "Essen", "Verbrauchsmaterialien", "Fahrtkosten", "Party", "Baumaterialien", "Überweisung Foodcoop", "Brennholz", "GEZ", "Strom", "Wasser", "Internet & Telefon", "Gas"))
    } else {updateSelectInput(session, "category", choices = c("Bitte waehlen", "Party", "Spende", "Miete"))}
  })
  
  # Update selectInput "category" after choosing selectInput "in_out"
  observeEvent(input$category, {
    if (input$category == "Miete") {updateDateInput(session, "billdate", label = "Miete für Monat?")} else {
      updateDateInput(session, "billdate", label = "Rechnungsdatum")
    }
  })
  
  # input fields are treated as a group
  # formData <- reactive({
  #   sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  # })
  
  # Click submit button and confirm with Yes
  observeEvent(input$BUTyes, {
    toggleModal(session, "confirm", toggle = "close")
    # Save values to database...
    newdata <-
      c(
        format(Sys.time(), "%Y.%m.%d %H:%M:%S"),
        input$name,
        input$in_out,
        input$category,
        format(input$billdate, "%Y.%m.%d"),
        as.numeric(input$value),
        input$comment,
        # Populate the done input with default FALSE
        #input$done
        FALSE
      )
    # print input values for debugging...
    #print(newdata)
    saveData(newdata)
    
    # Deactivated emptying the input fields for debugging...
    EmptyInputs(session)
  })
  

  
  #Press "delete row" button
  # observeEvent(input$delete, {
  #   # Get the right ID selected row because selected row must not equal the ID in database
  #   data <- ReadData()[input$responses_rows_selected, ]
  #   print(data["ldfNr"])
  #   saveData(data["lfdNr"], type = "delete")
  #   # Updates the editing fields below datatable
  #   #UpdateInputs(CreateDefaultRecord(), session)
  # }, priority = 1)

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