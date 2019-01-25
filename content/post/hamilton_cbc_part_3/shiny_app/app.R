
# runApp(here("content", "post", "hamilton_cbc_part_3", "shiny_app"))

library(shiny)
library(dplyr)
library(naniar)
library(ggplot2)
library(readr)
library(janitor)
library(scales)
library(plotly)

# Change font sizes
# Offer to divide by count hours

min_max <- function(vector){
  min_max <- c(min(vector), max(vector))
  return(min_max)
}


hamilton_cbc <- read_rds("hamilton_cbc_output.rds")


species_list <- hamilton_cbc %>%
  distinct(species) %>%
  rename(Species = species) %>%
  arrange(Species)

years_list <- hamilton_cbc %>%
  distinct(year) %>%
  rename(Year = year) %>%
  arrange(-Year)

year_min_max <- min_max(years_list)



ui <- navbarPage("Hamilton CBC App",
                 tabPanel(
                   
                   # App title ----
                   titlePanel("Birds counted over several years"),
                   
                   # Sidebar layout with input and output definitions ----
                   sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                       
                       helpText("Note: This data does not include birds counted only during count week"),
                  
                       
                       # Input: which species ----
                       selectizeInput("species_picked",
                                   multiple = TRUE,
                                   selected = c("American Robin", "Mourning Dove", "Northern Cardinal"),
                                   label = "Choose which species you would like to compare (up to five):",
                                   choices = species_list,
                                   options = list(maxItems = 5)),
                       
                       
                       # Input: Slider for the number of years ----
                       sliderInput("years_picked",
                                   label = "Number of years you would like to view:",
                                   sep = "",
                                   min = year_min_max[1],
                                   max = year_min_max[2],
                                   value = c(1955, year_min_max[2])),
                       helpText("Note: In 1955 the boundaries of the Hamilton CBC changed and previous to that there is more missing data so I would recommend only looking at data from 1955 at the earliest. However, the previous years' data have been included for completeness."),
                       
                       checkboxInput("log_picked",
                                     "Would you like the y-axis displayed on a log scale?",
                                     value = FALSE),
                       
                       checkboxInput("count_per_hour_picked",
                                     "Would you like the number of birds counted in a year to be divided by the number of hours of that year's count? (There are some years where this was not recorded, so there will be more missing data.)",
                                     value = FALSE)
                       
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                       
                       # Output: Line graph ----
                       plotOutput(outputId = "time_series_plot")
                       
                     )
                   )
                 ),
                 
                 
                 tabPanel(
                   
                   titlePanel("Birds seen in a particular year"),
                   
                   # Sidebar layout with input and output definitions ----
                   sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                       
                       # Input: which species ----
                       selectInput("individual_year_picked",
                                   label = "What year would you like to look at the CBC for:",
                                   choices = years_list)
                       
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                       
                       # Output: Data table ---- 
                       # can also do dataTableOutput
                       tableOutput(outputId = "count_table")
                       
                     )
                   )                         
                   
                   
                   
                   
                 )
)

# Define server logic required to draw a plot and table ----
server <- function(input, output) {

  # First navbar output ----
  data_input <- reactive({
    hamilton_cbc %>% 
      filter(year >= input$years_picked[1],
             year <= input$years_picked[2],
             species == input$species_picked) 
    
    
  })

  output$time_series_plot <- renderPlot({
    
    
    plotting_function <- function(input_for_plot){

      input_for_plot %>%
        ggplot(aes(x = year, y = how_many_counted, color = species)) +
        geom_line(size = 1) +
        xlab("Year") +
        ylab("Number counted") +
        labs(color = "Species") +
        theme_minimal() +
        theme(axis.text.x = element_text((size = 14))) +
        geom_miss_point()
      
    }
    
    if(input$log_picked){
      
      plotting_function(data_input()) +
        scale_y_log10()
      
    }
    
    else {
      
      plotting_function(data_input())
      
    }
    

    
  })
  
  
  # Second navpage output ----
  
  output$count_table <- renderTable({
    
    hamilton_cbc %>%
      filter(year == input$individual_year_picked) %>%
      count(species, how_many_counted) %>%
      select(-n) %>%
      arrange(-how_many_counted) %>%
      filter(!is.na(how_many_counted)) %>%
      mutate(how_many_counted = how_many_counted %>%
               scales::number(big.mark = ",")) %>%
      rename(Species = species, `How many were counted` = how_many_counted)


    
  },
  
  align = "lr")
}

shinyApp(ui = ui, server = server)
