library(shiny)
library(shinymanager)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
    user = c("BotinstSanta"),
    password = c("BotinstSanta"),
    stringsAsFactors = FALSE
)

ui <- secure_app(head_auth = tags$script(inactivity),
                 fluidPage(
                     textInput("name", "Your Name", value = NULL),
                     textInput("email", "Email address", value = NULL),
                     actionButton('Submit', "Submit"),
                     br(),
                     br(),
                     actionButton("load_pep", "See who signed up"),
                     br(),
                     br(),
                     dataTableOutput('table')
                     ))

server <- function(input, output, session) {
    ##login lock
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })
    
    file.create("user_inputs.csv")
    
    # classic app
    randomVals <- eventReactive(input$load_pep, {
    read.table("user_inputs.csv", header = FALSE, col.names = c("Name", "Email"))
    })
    
    output$table <- renderDataTable({randomVals()})
    
    observeEvent(input$Submit, {
        # Define inputs to save
        inputs_to_save <- c('name', 'email')
        # Declare inputs
        inputs <- NULL
        # Append all inputs before saving to folder
        for(input.i in inputs_to_save){
            inputs <- c(inputs, input[[input.i]])
        }
        # Inputs data.frame
        inputs_data_frame <- rbind.data.frame(inputs)
        colnames(inputs_data_frame) <- c("Name", "Email")
        write.table(inputs_data_frame, file = "user_inputs.csv", row.names = FALSE, append = TRUE, 
                    col.names = FALSE)

    }) 
}

shinyApp(ui = ui, server = server)