
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(janitor)
library(scales)

min_max <- function(vector){
  min_max <- c(min(vector), max(vector))
  return(min_max)
}


hamilton_cbc <- read_rds("hamilton_cbc_output_part_2.rds")

species_list <- hamilton_cbc %>%
  distinct(species) %>%
  rename(Species = species) %>%
  arrange(Species)

years_list <- hamilton_cbc %>%
  distinct(year) %>%
  rename(Year = year) %>%
  arrange(-Year)

year_min_max <- min_max(years_list)


ui <- navbarPage(tags$h3("Hamilton Christmas Bird Count app"),
                
                 tabPanel(
                   
                   # App title ----
                   titlePanel(tags$h4("Birds counted over multiple years")),
                   
                   # Sidebar layout with input and output definitions ----
                   sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                       
                       # Input: which species ----
                       selectizeInput("species_picked",
                                      multiple = TRUE,
                                      selected = c("American Robin", "Mourning Dove", "Northern Cardinal"),
                                      label = "Choose which species you would like to compare (up to six):",
                                      choices = species_list,
                                      options = list(maxItems = 6)),
                       
                       
                       # Input: Slider for the number of years ----
                       sliderInput("years_picked",
                                   label = "Number of years you would like to view:",
                                   sep = "",
                                   min = year_min_max[1],
                                   max = year_min_max[2],
                                   value = c(1955, year_min_max[2])),
                       
                       helpText(tags$ol(
                         tags$li("This data does not include birds counted only during count week"),
                         
                         tags$li("This data does not include hybrids or birds that were only identified to the \"sp.\" level"),
                         
                         tags$li("In 1955, the boundaries of the Hamilton Christmas Bird Count changed. I recommend only looking at data from 1955 onwards. However, the previous years' data have been included for completeness.")
                       )
                       )
                       
                     ),
                     
                     # Main panel for displaying outputs ----
                     mainPanel(
                       
                       # Output: Line graph ----
                       plotOutput(outputId = "time_series_plot")
                       
                     )
                   )
                   ),
                 
                 
                 tabPanel(
                   
                   titlePanel(tags$h4("Birds counted in a particular year")),
                   
                   # Sidebar layout with input and output definitions ----
                   sidebarLayout(
                     
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                       
                       # Input: which year ----
                       selectInput("individual_year_picked",
                                   label = "What year would you like to look at the Hamilton Christmas Bird Count data for:",
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
      filter(year >= req(input$years_picked[1]),
             year <= req(input$years_picked[2]),
             species %in% req(input$species_picked))
    
  })
  
  output$time_series_plot <- renderPlot({
    
    
    plotting_function <- function(input_for_plot){
      
      input_for_plot %>%
        ggplot(aes(x = year, y = how_many_counted, color = species)) +
        geom_line(size = 1) +
        xlab("Year") +
        ylab("Number counted") +
        theme_minimal() +
        theme(text = element_text(size = 18),
              legend.position = "none",
              plot.margin = margin(2, 20, 2, 2)) +
        facet_wrap(vars(species),
                   scales = "free",
                   dir = "v") +
        scale_y_continuous(labels = comma)
      
    }
    
    plotting_function(data_input())
    
    
    
    
  })
  
  
  # Second navbar output ----
  
  output$count_table <- renderTable({
    
    hamilton_cbc %>%
      filter(year == input$individual_year_picked) %>%
      count(species, how_many_counted) %>%
      select(-n) %>%
      arrange(-how_many_counted) %>%
      filter(how_many_counted > 0) %>%
      mutate(how_many_counted = how_many_counted %>%
               scales::number(big.mark = ",")) %>%
      rename(Species = species, `How many were counted` = how_many_counted)
    

  },
  
  align = "lr")
}

shinyApp(ui = ui, server = server)
