library(shiny)
library(shinyjs)
library(DT)
library(lubridate)
library(mailR)
library(tidyr)
library(dplyr)

# UI =======================================================================
# The UI for the Calendar Planner is structured using Shiny's fluid layout, 
# which adapts to different screen sizes. The UI is divided into two main tabs: 
# Task Management and Task Statistics.

ui <- fluidPage(
    titlePanel("Calendar Planner"),  # Main title of the app
    shinyjs::useShinyjs(),  # Enable the use of shinyjs for enhanced UI interactions
    
    # Main layout with tabs
    tabsetPanel(
        
        # First tab: Task Management
        # This tab allows users to add, view, and manage tasks.
        tabPanel("Task Management", 
                 sidebarLayout(
                     sidebarPanel(
                         
                         # Date Range Input
                         # Users can select a start and end date for their tasks.
                         dateRangeInput(inputId = "dateRange", 
                                        label = "Select date range",
                                        start = Sys.Date(), 
                                        end = Sys.Date() + 1),
                         
                         # Time Inputs
                         # Users can specify the start and end times for tasks.
                         textInput("startTime", "Start Time", value = "", placeholder = "8:00"),
                         textInput("endTime", "End Time", value = "", placeholder = "17:00"),
                         
                         # Task Name Input
                         # Users can add a new task name here.
                         textInput("newTaskName", "Add a New Task", 
                                   value = "",
                                   placeholder = "Task name"),
                         
                         # Button to Add New Task
                         # Adds the specified new task to the list.
                         actionButton("addTaskBtn", "Specify a New Task", class = "btn-primary"),
                         
                         # Task Selection Dropdown
                         # Users can select a task from predefined options.
                         selectInput(inputId = "task",
                                     label = "Select a task",
                                     choices = c("Meeting", "Gym", "Appointment", "Shopping", "Classes", "Work"),
                                     selected = "Meeting"),
                         
                         # Comment Input
                         # Users can add comments related to the task.
                         textInput(inputId = "comment", 
                                   label = "Comment", 
                                   value = "", 
                                   placeholder = "Enter your comment"),
                         
                         # Save Task Button
                         # Saves the specified task with details.
                         actionButton(inputId = "saveTask", label = "Save Task", class = "btn-success"),
                         
                         # Delete Task Button
                         # Deletes the selected task from the list.
                         actionButton("deleteTask", label = "Delete Selected Task", class = "btn-danger"),
                         
                         # Email Address Input
                         # Users can enter their email to send the schedule.
                         textInput("emailAddress", "Email your schedule", value = "",placeholder = "your@email.com"),
                         
                         # Send Email Button
                         # Sends the current schedule to the specified email address.
                         actionButton("sendEmail", "Send Email")
                         
                     ),
                     
                     mainPanel(
                         # Displays the selected time range and task.
                         textOutput("selectedTimeRange"),
                         textOutput("selectedTask"),
                         
                         # Table of Planned Tasks
                         # Shows the tasks that have been added with their details.
                         dataTableOutput("taskTable"), 
                         
                         # Download Button for Tasks
                         # Allows users to export their tasks.
                         downloadButton("exportTasks", "Export Tasks")
                     )
                 )
        ),
        # Second tab: Task Statistics
        # This tab allows users to view statistics about their tasks.
        tabPanel("Task Statistics", 
                 fluidRow(
                     column(4, 
                            selectInput("statsView", "Select View", 
                                        choices = c("Weekly", "Monthly", "Yearly", "All-Time"))
                     )
                 ),
                 fluidRow(
                     column(4, 
                            wellPanel(
                                h4("Most Saved Task"),
                                textOutput("mostSavedTask")
                            )
                     ),
                     column(4, 
                            wellPanel(
                                h4("Average Number of Tasks Per Day"),
                                textOutput("avgTasksPerDay")
                            )
                     ),
                     column(4, 
                            wellPanel(
                                h4("Total Saved Tasks"),
                                textOutput("totalSavedTasks")
                            )
                     )
                 ),
                 # Add the download button for exporting statistics
                 downloadButton("downloadStats", "Export Statistics to CSV")
        )
        
    )
)

#STANDALONE FUNCTIONS =======================================================

## Function to generate the file
# This function takes a data frame of tasks and generates a text file with detailed task information organized by date.
generateContent <- function(file, tasks) {
    
    # Expand tasks to a row for each day of the task. This is necessary for tasks that span multiple days.
    tasks <- tasks %>%
        mutate(StartDate = as.Date(StartDate),
               EndDate = as.Date(EndDate)) %>%
        rowwise() %>%
        do({
            data.frame(., Date = seq(.$StartDate, .$EndDate, by = "day"))
        }) %>%
        ungroup()
    
    # Sort and format dates
    tasks <- tasks %>%
        arrange(Date) %>%
        mutate(FormattedDate = format(Date, "%d-%m-%Y"),  # Format the task dates
               FormattedDeadline = format(EndDate, "%d-%m-%Y"))  # Format the end dates as deadlines
    
    # Generate content string
    content <- unlist(lapply(unique(tasks$FormattedDate), function(formatted_date) {
        daily_tasks <- filter(tasks, FormattedDate == formatted_date)
        task_lines <- lapply(1:nrow(daily_tasks), function(i) {
            paste0("* ", daily_tasks$Task[i], " (Deadline: ", daily_tasks$FormattedDeadline[i], ")",
                   " || Start: ", daily_tasks$StartTime[i],
                   " || End: ", daily_tasks$EndTime[i],
                   " || Comment: ", daily_tasks$Comment[i], " ||")
        })
        c(paste0("## ", formatted_date, " ##"), task_lines, "")
    }))
    
    # Write the content to the file
    writeLines(content, con = file)
}

## Function to check if time is in HH:MM format
# This function uses a regular expression to validate time in HH:MM format.
isValidTime <- function(time) {
    grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", time)
}

## Function to get the color based on the task
# This function returns a color code based on the task type. If the task type is not predefined, it assigns a random color.
getTaskColor <- function(task) {
    
    predefined_colors <- c(Meeting = "#78a4ad", Gym = "#a4c991", Appointment = "#fade9d", 
                           Shopping = "#c15b4e", Classes = "#b9a898", Work = "#bf94e4")
    
    if (!is.null(predefined_colors[[task]])) {
        return(predefined_colors[[task]])
    } else {
        # Default color for tasks not in the predefined list
        return("#6f6f6f")  # Grey color, or any color of your choice
    }
}


# Function to Calculate Statistics for CSV Export
# This function computes statistics from a task data frame and returns a data frame suitable for CSV export.
calculate_stats_for_csv <- function(tasks) {
    if (is.null(tasks) || nrow(tasks) == 0) {
        return(data.frame(
            "Most Saved Task" = NA, 
            "Average Number of Tasks Per Day" = NA, 
            "Total Saved Tasks" = NA
        ))
    } else {
        most_common_task <- names(sort(table(tasks$Task), decreasing = TRUE))[1]
        avg_tasks_per_day <- mean(table(tasks$StartDate))
        total_tasks <- nrow(tasks)
        return(data.frame(
            "Most Saved Task" = most_common_task, 
            "Average Number of Tasks Per Day" = avg_tasks_per_day, 
            "Total Saved Tasks" = total_tasks
        ))
    }
}


#SERVER =======================================================================

server <- function(input, output, session) {
    
    # Reactive value indicating whether the file export has been completed
    fileExported <- reactiveVal(FALSE)
    
    # Reactive value holding the data frame of planned tasks. This data frame includes
    # start and end dates, start and end times, task name, comments, and color for each task.
    planned_tasks <- reactiveVal(data.frame(
        StartDate = as.Date(character()),  # Convert character vector to Date
        EndDate = as.Date(character()),    # Convert character vector to Date
        StartTime = character(),           # Store start time as character
        EndTime = character(),             # Store end time as character
        Task = character(),                # Store task name as character
        Comment = character(),             # Store comments as character
        Color = character(),               # Store color associated with task as character
        stringsAsFactors = FALSE           # Avoid conversion to factors for text columns
    ))
    
    # Observer to enable or disable the 'Export Tasks' button based on the number of tasks.
    # If there are no tasks, the button is disabled; otherwise, it is enabled.
    observe({
        taskCount <- nrow(planned_tasks())
        if (taskCount == 0) {
            shinyjs::disable("exportTasks")  # Disable button when there are no tasks
        } else {
            shinyjs::enable("exportTasks")   # Enable button when there are tasks
        }
    })
    
    
    ## Output the table of planned tasks with colors
    output$taskTable <- renderDataTable({
        # Retrieve the current set of planned tasks
        tasks_data <- planned_tasks()
        
        # If there are tasks, update column names to be more user-friendly
        if(is.data.frame(tasks_data) && nrow(tasks_data) > 0) {
            colnames(tasks_data) <- c('Start Date', 'End Date', 'Start Time', 'End Time', 'Task', 'Comment', 'Color')
        } else {
            # Create an empty data frame with the same structure as `tasks_data` for consistency
            tasks_data <- data.frame(
                `Start Date` = character(),
                `End Date` = character(),
                `Start Time` = character(), 
                `End Time` = character(), 
                Task = character(), 
                Comment = character(), 
                Color = character(), 
                stringsAsFactors = FALSE
            )
        }
        
        # Configure and render the DataTable
        datatable(
            tasks_data, 
            options = list(
                language = list(emptyTable = 'No saved tasks'),  # Message displayed when no tasks are available
                columnDefs = list(
                    list(visible = FALSE, targets = 6)  # Hide the 'Color' column from the user
                ),
                rowCallback = JS(
                    "function(row, data, index) {",
                    "  $(row).css('background-color', data[6]);",  # Apply background color to each row based on the 'Color' column
                    "}"
                )
            ),
            selection = 'single',  # Allow selection of single rows
            rownames = FALSE       # Hide rownames in the table
        )
    })
    
    ## Download handler for exporting tasks
    output$exportTasks <- downloadHandler(
        # Define the name of the file to be downloaded
        filename = function() {
            paste("Planned-Tasks-", Sys.Date(), ".txt", sep = "")  # Naming pattern: 'Planned-Tasks-YYYY-MM-DD.txt'
        },
        # Define the content of the file
        content = function(file) {
            generateContent(file, planned_tasks())  # Generate the content based on current planned tasks
            fileExported(TRUE)  # Mark file generation as complete
        }
    )
    
    ## Observer to notify user upon successful file export
    observeEvent(fileExported(), {
        if (fileExported()) {
            # Display a success modal when the file is exported
            showModal(modalDialog(
                title = "Success",
                "File exported successfully!",
                easyClose = TRUE,  # Allow the modal to be closed easily
                footer = NULL  # No footer needed for this modal
            ))
            fileExported(FALSE)  # Reset the file exported status for future exports
        }
    })
    
    
    ## Observer for 'Save Task' button click event
    observeEvent(input$saveTask, {
        
        # Convert input dates to Date objects
        start_date <- as.Date(input$dateRange[1])
        end_date <- as.Date(input$dateRange[2])
        
        # Validate the date range: start date should not be after the end date
        if (start_date > end_date) {
            showModal(modalDialog(
                title = "Invalid Date Range",
                "Start date cannot be later than the end date.",
                easyClose = TRUE,
                footer = NULL
            ))
            return()  # Exit the observer if the date range is invalid
        }
        
        # Validate time format for start and end times
        if (!isValidTime(input$startTime) || !isValidTime(input$endTime)) {
            showModal(modalDialog(
                title = "Invalid Time Format",
                "Please enter time in HH:MM format.",
                easyClose = TRUE,
                footer = NULL
            ))
            return()  # Exit the observer if the time format is invalid
        }
        
        # Create a new task entry with the provided details
        new_task <- data.frame(
            StartDate = start_date,
            EndDate = end_date,
            StartTime = input$startTime,
            EndTime = input$endTime,
            Task = input$task,
            Comment = input$comment,
            Color = getTaskColor(input$task),  # Determine the color based on the task type
            stringsAsFactors = FALSE  # Avoid converting strings to factors
        )
        
        # Update the list of planned tasks by adding the new task
        planned_tasks(rbind(planned_tasks(), new_task))
    })
    
    
    
    ## Reactive expression to track the selected rows in the task table
    selected_rows <- reactive({
        input$taskTable_rows_selected
    })
    
    ## Observer for the 'Delete Task' button
    observeEvent(input$deleteTask, {
        
        # Check if any task is selected for deletion
        if (is.null(selected_rows()) || length(selected_rows()) == 0) {
            # Display a modal dialog if no task is selected
            showModal(modalDialog(
                title = "No Task Selected",
                "Please select a task to delete.",
                easyClose = TRUE,  # Allow closing the modal easily
                footer = NULL      # No additional actions in the modal footer
            ))
        } else {
            # Remove the selected tasks from the list of planned tasks
            updated_tasks <- planned_tasks()[-selected_rows(), ]
            planned_tasks(updated_tasks)  # Update the planned tasks
        }
    })
    
    # Observer to handle past date selection in the date range input
    observe({
        
        # Check if the selected start date is in the past
        if (!is.null(input$dateRange) && as.Date(input$dateRange[1]) < Sys.Date()) {
            # Display a modal dialog for confirmation of the past date selection
            showModal(modalDialog(
                title = "Past Date Selected",
                "Selected Start Date is in the past. Do you wish to select it?",
                footer = tagList(
                    actionButton("cancelDate", "Cancel", class = "btn-default"),
                    actionButton("confirmDate", "Continue", class = "btn-primary")
                )
            ))
        }
    })
    
    
    # Observer for handling the 'Cancel' button in the date selection modal
    observeEvent(input$cancelDate, {
        removeModal()  # Close the modal dialog
        
        # Reset the date range input to the current date and the previously selected end date
        updateDateRangeInput(session, "dateRange", start = Sys.Date(), end = input$dateRange[2])
    })
    
    # Observer for handling the 'Confirm' button in the date selection modal
    observeEvent(input$confirmDate, {
        removeModal()  # Close the modal dialog upon confirmation
        # No further action needed as the user confirms the selection of the past date
    })
    
    # Reactive value to store and manage the list of available tasks
    task_choices <- reactiveVal(c("Meeting", "Gym", "Appointment", "Classes", "Shopping", "Work"))
    
    # Observer for adding a new task through the 'Add Task' button
    observeEvent(input$addTaskBtn, {
        new_task_name <- input$newTaskName  # Capture the entered new task name
        
        # Validate the new task name input
        if (new_task_name == "") {
            showModal(modalDialog(
                title = "Attention",
                "Please enter a name for your task.",
                easyClose = TRUE,
                footer = NULL
            ))
        } else if (new_task_name %in% task_choices()) {
            # Check for duplicate task names
            showModal(modalDialog(
                title = "Task Already Exists",
                paste("The task", sQuote(new_task_name), "already exists. Please enter a different name."),
                easyClose = TRUE,
                footer = NULL
            ))
        } else {
            # Add the new task to the list of choices and clear the input field
            task_choices(c(task_choices(), new_task_name))
            updateTextInput(session, "newTaskName", value = "")
        }
    })
    
    
    # Update the task selection options in the UI based on the current list of task choices
    observe({
        # Update the 'task' selectInput with the latest task choices
        updateSelectInput(session, "task", choices = task_choices())
    })
    
    ## Observer for handling the 'Send Email' event
    observeEvent(input$sendEmail, {
        # Check if the email address field is not empty
        if (input$emailAddress != "") {
            # Validate if there are any tasks to send
            if (nrow(planned_tasks()) == 0) {
                # Show a modal dialog if no tasks are available to send
                showModal(modalDialog(
                    title = "No Tasks to Send",
                    "There are no tasks to include in the email.",
                    easyClose = TRUE,
                    footer = NULL
                ))
            } else {
                # Prepare the file to be sent via email
                file_name <- paste0("Planned-Tasks-", Sys.Date(), ".txt")
                temp_file <- file.path(tempdir(), file_name)
                
                # Generate the content for the email attachment
                generateContent(temp_file, planned_tasks())
                
                # Attempt to send the email and capture the result
                email_send_result <- tryCatch({
                    send.mail(from = "calendarplannerpw@hotmail.com",
                              to = input$emailAddress,
                              subject = "Calendar Planner - Planned Tasks",
                              body = "See attached for your planned tasks.",
                              smtp = list(host.name = "smtp.office365.com", port = 587,
                                          user.name = "calendarplannerpw@hotmail.com", passwd = "uazhleughejhnkhi", tls = TRUE),
                              authenticate = TRUE,
                              send = TRUE,
                              attach.files = temp_file)
                    TRUE
                }, error = function(e) {
                    FALSE
                })
                
                # Clean up by deleting the temporary file
                unlink(temp_file)
                
                # Display the result of the email sending attempt
                if (email_send_result) {
                    showModal(modalDialog(
                        title = "Success",
                        "Email sent successfully!",
                        easyClose = TRUE,
                        footer = NULL
                    ))
                } else {
                    showModal(modalDialog(
                        title = "Error",
                        "Failed to send email. Make sure to provide a valid email address.",
                        easyClose = TRUE,
                        footer = NULL
                    ))
                }
            }
        } else {
            # Show a modal dialog if the email address is missing
            showModal(modalDialog(
                title = "Missing Email Address",
                "Please enter an email address to send the file.",
                easyClose = TRUE,
                footer = NULL
            ))
        }
    })
    
    
    # Reactive expression to store tasks filtered according to the selected time view
    selected_tasks <- reactive({
        # Retrieve the current list of planned tasks
        tasks <- planned_tasks()
        
        # Determine the current date
        today <- Sys.Date()
        
        # Filter tasks based on the user's selection in 'statsView'
        switch(input$statsView,
               "Weekly" = { 
                   # Define the start and end of the current week
                   startOfWeek <- floor_date(today, "week")
                   endOfWeek <- ceiling_date(today, "week") - days(1)
                   tasks[tasks$StartDate >= startOfWeek & tasks$StartDate <= endOfWeek, ]
               },
               "Monthly" = { 
                   # Define the start and end of the current month
                   startOfMonth <- floor_date(today, "month")
                   endOfMonth <- ceiling_date(today, "month") - days(1)
                   tasks[tasks$StartDate >= startOfMonth & tasks$StartDate <= endOfMonth, ]
               },
               "Yearly" = { 
                   # Define the start and end of the current year
                   startOfYear <- floor_date(today, "year")
                   endOfYear <- ceiling_date(today, "year") - days(1)
                   tasks[tasks$StartDate >= startOfYear & tasks$StartDate <= endOfYear, ]
               },
               "All-Time" = { 
                   # Select all tasks for the 'All-Time' view
                   tasks
               },
               # Default to show all tasks if no specific view is selected
               tasks
        )
    })
    
    # Observer to update task statistics based on the selected stats view
    observeEvent(input$statsView, {
        # Recalculate and display the most saved task when the stats view changes
        output$mostSavedTask <- renderText({
            tasks <- selected_tasks()  # Access the filtered list of tasks
            if (is.null(tasks) || nrow(tasks) == 0) {
                "No tasks available for this period"
            } else {
                # Determine the most frequently saved task
                most_common_task <- names(sort(table(tasks$Task), decreasing = TRUE))[1]
                paste("Most saved task: ", most_common_task)
            }
        })
    })
    
    
    # Update task statistics based on the selected view
    output$avgTasksPerDay <- renderText({
        tasks <- selected_tasks()  # Access filtered tasks based on selected view
        if (is.null(tasks) || nrow(tasks) == 0) {
            "No tasks available for this period"
        } else {
            # Calculate and display the average number of tasks per day
            unique_days <- length(unique(tasks$StartDate))
            avg_tasks_per_day <- nrow(tasks) / unique_days
            paste("Average tasks per day: ", format(avg_tasks_per_day, digits = 2))
        }
    })
    
    output$totalSavedTasks <- renderText({
        tasks <- selected_tasks()  # Access filtered tasks based on selected view
        if (is.null(tasks) || nrow(tasks) == 0) {
            "No tasks available for this period"
        } else {
            # Calculate and display the total number of saved tasks
            total_tasks <- nrow(tasks)
            paste("Total saved tasks: ", total_tasks)
        }
    })
    
    # Download handler for exporting task statistics
    output$downloadStats <- downloadHandler(
        filename = function() {
            paste("task-statistics-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            # Access the filtered tasks for statistics
            tasks <- selected_tasks()
            
            # Prepare the statistics data frame for export
            if (is.null(tasks) || nrow(tasks) == 0) {
                stats <- data.frame(
                    "Most Saved Task" = NA, 
                    "Average Number of Tasks Per Day" = NA, 
                    "Total Saved Tasks" = NA
                )
            } else {
                most_common_task <- names(sort(table(tasks$Task), decreasing = TRUE))[1]
                unique_days <- length(unique(tasks$StartDate))
                avg_tasks_per_day <- nrow(tasks) / unique_days
                total_tasks <- nrow(tasks)
                stats <- data.frame(
                    "Most Saved Task" = most_common_task, 
                    "Average Number of Tasks Per Day" = avg_tasks_per_day, 
                    "Total Saved Tasks" = total_tasks
                )
            }
            
            # Replace dots with spaces in column names
            names(stats) <- gsub("\\.", " ", names(stats))
            
            # Write the statistics to the specified CSV file
            write.csv(stats, file, row.names = FALSE, quote = TRUE)
        }
    )
    
}
# Run the application
shinyApp(ui = ui, server = server)
