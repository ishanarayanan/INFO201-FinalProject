library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)

#img(src = "covid.jpg")
covid_data <- read.csv("worldometer_data.csv")

covid <- covid_data %>%
  group_by(Country.Region)%>%
  mutate(sum=sum(TotalTests, na.rm= T))

map_info <- rename(covid_data, region = Country.Region) %>%
  mutate(CaseRate = (TotalCases / Population) * 100) %>%
  select(region, ActiveCases, TotalCases, CaseRate, TotalDeaths, TotalRecovered, TotalTests)
map_shape <- map_data("world") %>%
  left_join(map_info, by = "region")

map_measure <- select(map_shape, ActiveCases, TotalCases, CaseRate, TotalDeaths, TotalRecovered, TotalTests)

Title_page <- tabPanel(
  "Introduction",
  titlePanel("Introduction"),
  p("The Covid-19 pandemic is one of the most prominent, large-scale international issues of the 21st century. With covid cases and deaths rising everyday, it is hard to look past the impact that this pandemic has had on our world today. We have analyzed a dataset that highlights each country's cases, deaths, COVID tests, population, and recoveries. COVID 19 data can reveal more about a country or continent than just cases. We can find out where in the world people are most affected by this sort of global issue, and what countries are able to effectively minimize the impact of COVID-19. "),
  h2("Exploration"),
  p("We approached the analysis of this dataset by asking questions about each continent's covid distribution and impact. We used a bar chart to analyze the countries within a continent that have administered the most coronavirus tests. The bar chart displays the counts of tested civilians for the top 6 populated countries within a continent. This will allow us to see what countries within a continent take more precautionary measures, or what countries have the resources to do so."), 
  p("Next, we displayed a choropleth map of the world which displays various COVID-19 metrics. The dropdown feature allows users to look at the distribution of COVID-19 cases, tests, recovered cases, and deaths across the world. With this interactive feature, the user will be able to quickly compare different aspects of the pandemic visually. "), 
  p("Finally, a line graph organized by continent will show the user the total tests given compared to the total recovered cases. This goal is to visually analyze the covid response of each region. The slope of the line of the ratio of the tests administered to the recovered population could illuminate which countries have effective responses to positive tests. "), 
  HTML('<center><img src="COVIDD.png" width="400"></center>')
)

page_one <- tabPanel(
  "Bar Chart", 
  titlePanel("Understanding Location versus Total Tests"),
  
  selectInput(
    inputId = "countries",
    label = "Select a Continent:",
    choices = unique(covid_data$Continent), 
    selected = "North America" 
  ), 
  plotlyOutput("Continent")
)

page_two <- tabPanel(
  "Dot map",
  titlePanel("Global Covid Cases"),
  radioButtons(
    inputId = "measurement",
    label = "Select a measurement",
    choices = colnames(map_measure),
    select = "TotalCases"
  ),
  plotlyOutput(outputId = "map")
)

page_three <- tabPanel(
  "Line Graph", 
  titlePanel("Understanding Tests versus Recovered Population"),
  
  selectInput(
    inputId = "c",
    label = "Select a Continent:",
    choices = unique(covid_data$Continent), 
    selected = "North America" 
  ), 
  plotlyOutput("C")
)

Summary<- tabPanel(
  "Summary", 
  titlePanel("Key Takeaways"), 
  p(" After completing our graphs and our analysis, we were able to begin to picture the differences in the impact of COVID 19 around the world. We visualized information about tests, and what countries were most affected by the pandemic. Through our visualizations, we were surprised about the distribution of covid numbers, highlighted in the following takeaways. "), 
  p("	In the bar chart, one key takeaway was that in North America, the United States of America had a staggeringly higher number of total coronavirus tests than all the other countries. For example the difference between total tests administered in the USA and Canada, which had the second highest number of tests administered, is 58820433 tests. This points to an equity and test distribution issue in North America. Furthermore, comparing the bar chart continent to continent, shows that the United States of America is the most tested country in the world. I also find it interesting that rather populous countries in each continent have relatively high testing numbers, such as the United States of America, Australia, India, Russia, and Brazil. "), 
  p("	The choropleth map illuminated information about which countries have the highest level of active cases, total cases, deaths, recovered cases, and tests. We explored the relationship between deaths and cases across the world; we saw that there was much more variety in the number of cases, while the number of deaths chart looked more equally distributed globally. We noticed that some regions, such as South America and Eastern Europe, had low cases but a high number of deaths; this might indicate healthcare or resource issues. We also found it interesting that the United States has the highest numbers in every category."), 
  p("	Our line graph compared the recovered population to the number of tests administered by each continent. We were able to see that Africa had a very low number of recovered cases to the amount of tests administered. The data for Europe had some gaps, but the other countries' graphs rendered nicely. We were also surprised at how linear the data was for Australia and Oceania. This could mean that their covid response was strong as they had a good ratio of tests to recovered people. ")
)

library(shinythemes)

my_ui <- navbarPage(
  theme = shinytheme("slate"), 
  
  "Global COVID-19 Trends",
  Title_page,
  page_one,
  page_two,
  page_three, 
  Summary
)

my_server <- function(input, output){
  output$C<- renderPlotly({
    covid_clean <- covid_data %>%
      filter(Continent %in% input$c) %>%
      group_by(Continent)
    
    
    ggplot(data = covid_clean)+
      geom_line(aes(x= TotalTests,y=TotalRecovered))+
      
      labs(title ="Total Recovered population vs. Total Tests organized by Continent", 
           x = "Total Tests given",
           y = "Total Recovered Population")
  })
  
  output$map <- renderPlotly({
    #This line is trying to force input data to new column
    map_refined <- mutate(map_shape, measure = unlist(map_shape[input$measurement]))
    ggplot(data = map_refined) +
      geom_polygon(
        #This line needs to have the specific column assigned by input referenced in fill.
        mapping = aes(x = long, y = lat, group = group, fill = measure),
        color = "white",
        size = .1
      ) +
      coord_map() +
      scale_fill_continuous(low = "white", high = "red") +
      labs(
        title = "Global Covid Cases",
        x = "longitude",
        y = "latitude"
      )
  })
  
  output$Continent<- renderPlotly({
    covid_clean <- covid %>%
      group_by(Continent) %>%
      filter(Continent == input$countries) %>%
      head(arrange(desc(sum)), n =6)
    
    ggplot(data = covid_clean)+
      geom_bar(aes(x=Country.Region, y=sum), stat = "identity")+
      
      labs(title ="Total Tests organized by the top 6 populated Countries within a Continent", 
           x = "Countries",
           y = "Total Tests Given")
  })  
}
  
shinyApp(ui = my_ui, server = my_server)