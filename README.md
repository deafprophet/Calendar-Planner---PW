# Calendar Planner Shiny Application

## Overview
The Calendar Planner is a web application built using R Shiny. It allows users to manage and view tasks on a calendar, offering features such as adding, viewing, managing tasks, and viewing task statistics.

## Dependencies
- `shiny`: For building the interactive application.
- `shinyjs`: For enhanced UI interactions.
- `DT`: For displaying tables of tasks.
- `lubridate`: For handling date and time.
- `mailR`: For sending email notifications.
- `tidyr`: For data tidying.
- `dplyr`: For data manipulation.

## Application Structure
The application is structured into two main components: the UI and the server logic.

### UI
The UI is divided into two tabs: "Task Management" and "Task Statistics".

#### Task Management Tab
This tab allows users to:
- Select date ranges for tasks.
- Input start and end times for tasks.
- Add new task names and comments.
- Select tasks from predefined options.
- Save or delete tasks.
- Send the schedule via email.
- View and export a table of planned tasks.

#### Task Statistics Tab
This tab provides:
- Options to view statistics (Weekly, Monthly, Yearly, All-Time).
- Display of most saved tasks, average tasks per day, and total saved tasks.
- Functionality to export these statistics to CSV.

### Server Logic
The server-side of the application handles:
- Task management (addition, deletion, and saving of tasks).
- Validation of dates and times.
- Email sending functionality.
- Task statistics calculations and updates.
- File generation for tasks and statistics exporting.

## Key Functions

### `generateContent`
Generates a text file with detailed task information organized by date.

### `isValidTime`
Validates if a given time is in the HH:MM format.

### `getTaskColor`
Returns a color code based on the task type.

### `calculate_stats_for_csv`
Computes statistics from a task data frame for CSV export.

### Observers and Reactive Expressions
Multiple observers and reactive expressions are used to dynamically update the UI, respond to user interactions, and manage the state of the application.

## Running the Application
To run the application, load the required libraries and execute the `shinyApp(ui = ui, server = server)` command.
