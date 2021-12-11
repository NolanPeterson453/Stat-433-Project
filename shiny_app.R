library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# cleaned_data
shiny_df <- read_csv(file = "https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/cleaned_data_433.csv") %>% 
  rename(job_title = 'ACS Occupational Title')

# Create User interface with fluid page layout
ui <- fluidPage(
  
  # Title 
  titlePanel("Occupational Satistics"),
  
  # Left sidebar layout (input controls)
  sidebarPanel(
    selectizeInput('job', 'Job Title', choices = shiny_df$job_title)
  ),
  
  # Main panel (output)
  mainPanel(
    dataTableOutput(outputId="dataTable"),
    plotOutput('densityPlot'),
  )
)


# Create server
server <- function(input, output){
  

  
  # Create density Plot shows avg age density for all occps and plots avg age of 
  # chosen occupation 
  output$densityPlot <- renderPlot(
    {
      ggplot(shiny_df, aes(x = mean_age_2016)) +
        geom_density() +
        geom_vline(aes(xintercept = 
                         mean_age_2016[shiny_df$job_title == input$job],
                         color = shortage[shiny_df$job_title == input$job]),
                   size = 2) +
        scale_color_manual(values = c("Shortage" = "red", "No Shortage" = "seagreen"))+
        labs(title = "Density of Occupational Age Averages",
             x = "Age",
             y = "Density",
             color = "Shortage")
    }
  )
  
  
  output$dataTable <- renderDataTable({

    filter(shiny_df,
           job_title  == input$job) %>%
      dplyr::select(mean_annual_2019,emp_growth, wage_growth, unemployed_rate_2019, shortage) %>%
      transmute(Mean_Annual_Salary = round(mean_annual_2019, 2),
                Employment_Growth_Rate = round(emp_growth * 100, 2),
                Wage_Growth_Rate = round(wage_growth * 100, 2),
                Unemployment_Rate = round(unemployed_rate_2019, 2),
                Shortage = shortage)})
}

shinyApp(ui = ui, server = server)


