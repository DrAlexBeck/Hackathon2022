### Shiny Dashboard
library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)

model <- readRDS("ML_LM.rds")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "County-level Health Outcomes in the United States"),
  
  dashboardSidebar(
    menuItem(
      "Years of Potential Life Lost",
      tabName = "YPLL_tab")
    ),
  dashboardBody(
    tabItem(
      tabName = "YPLL_tab",
      box(valueBoxOutput("YPLL_prediction")),
      box(sliderInput("HPSA.Score.Primary", label = "HPSA Score",
                      min = 0, max = 26, value = 13)),      
      box(sliderInput("HPSA.Shortage.Primary", label = "Primary Care Physician Rate (per 100,000)",
                      min = 0, max = 600, value = 300)),
      box(sliderInput("Primary.Care.Physicians.Rate", label = "Primary Care Physician Rate",
                      min = 0, max = 600, value = 250)),
      box(sliderInput("percent.Fair.or.Poor.Health", label = "% Fair or Poor Health",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Smokers", label = "% Smokers",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Adults.with.Obesity", label = "% Adult Obesity",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("Food.Environment.Index", label = "Food Environment Index",
                      min = 0, max = 10, value = 5)),
      box(sliderInput("percent.With.Access.to.Exercise.Opportunities", label = "% Exercise Opportunities",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Excessive.Drinking", label = "% Excessive Drinking",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("Chlamydia.Rate", label = "Chlamydia Rate",
                      min = 0, max = 4000, value = 2000)),
      box(sliderInput("Teen.Birth.Rate", label = "Teen Birth Rate",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Uninsured.x", label = "% Uninsured",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.With.Annual.Mammogram", label = "% Annual Mammogram",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Vaccinated", label = "% Vaccinated",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Completed.High.School", label = "% Completed High School",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Unemployed", label = "% Unemployed",
                      min = 0, max = 100, value = 50)),      
      box(sliderInput("percent.Children.in.Poverty", label = "% Children in Poverty",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("Social.Association.Rate", label = "Social Association Rate",
                      min = 0, max = 100, value = 25)),
      box(sliderInput("Violent.Crime.Rate", label = "Violent Crime Rate",
                      min = 0, max = 2000, value = 1000)),
      box(sliderInput("Average.Daily.PM2.5", label = "Average Daily Particulate Matter (2.5PM)",
                      min = 0, max = 25, value = 12)),
      box(sliderInput("percent.Severe.Housing.Problems", label = "% Severe Housing Problems",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Drive.Alone.to.Work", label = "% Drive Alone to Work",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Adults.with.Diabetes", label = "% Adult Diabetes",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("HIV.Prevalence.Rate", label = "HIV Prevalence Rate",
                      min = 0, max = 3500, value = 1750)),
      box(sliderInput("percent.Insufficient.Sleep", label = "% Insufficient Sleep",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Disconnected.Youth", label = "% Disconnected Youth",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("Average.Grade.Performance", label = "Average Grade Performance",
                      min = 0, max = 4, value = 2)),
      box(sliderInput("Segregation.index", label = "Segregation Index",
                      min = 0, max = 1, value = 0.25)),
      box(sliderInput("Spending.per.pupil", label = "Spending per Pupil",
                      min = 5000, max = 45000, value = 25000)),
      box(sliderInput("Gender.Pay.Gap", label = "Gender Pay Gap",
                      min = 0, max = 2, value = 1)),
      box(sliderInput("Median.Household.Income", label = "Median Household Income",
                      min = 20000, max = 180000, value = 90000)),
      box(sliderInput("percent.Homeowners", label = "% Homeowners",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Less.Than.18.Years.of.Age", label = "% Under 18",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.65.and.Over", label = "% Over 65",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.Not.Proficient.in.English", label = "% Not Proficient in English",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.female", label = "% Female",
                      min = 0, max = 100, value = 50)),
      box(sliderInput("percent.rural", label = "% Rural",
                      min = 0, max = 100, value = 50))
    )
  )
)

 
server <- function(input, output) { 
  
  output$YPLL_prediction <- renderValueBox({
    
    prediction <- predict(
      model,
      tibble("percent.Fair.or.Poor.Health" = input$percent.Fair.or.Poor.Health,
             "percent.Smokers" = input$percent.Smokers,
             "percent.Adults.with.Obesity" = input$percent.Adults.with.Obesity,
             "Food.Environment.Index" = input$Food.Environment.Index,
             "percent.With.Access.to.Exercise.Opportunities" = input$percent.With.Access.to.Exercise.Opportunities,
             "percent.Excessive.Drinking" = input$percent.Excessive.Drinking,
             "Chlamydia.Rate" = input$Chlamydia.Rate,
             "Teen.Birth.Rate" = input$Teen.Birth.Rate,
             "percent.Uninsured.x" = input$percent.Uninsured.x,
             "Primary.Care.Physicians.Rate" = input$Primary.Care.Physicians.Rate,
             "percent.With.Annual.Mammogram" = input$percent.With.Annual.Mammogram,
             "percent.Vaccinated" = input$percent.Vaccinated,
             "percent.Completed.High.School" = input$percent.Completed.High.School,
             "percent.Unemployed" = input$percent.Unemployed,
             "percent.Children.in.Poverty" = input$percent.Children.in.Poverty,
             "Social.Association.Rate" = input$Social.Association.Rate,
             "Violent.Crime.Rate" = input$Violent.Crime.Rate,
             "Average.Daily.PM2.5" = input$Average.Daily.PM2.5,
             "percent.Severe.Housing.Problems" = input$percent.Severe.Housing.Problems,
             "percent.Drive.Alone.to.Work" = input$percent.Drive.Alone.to.Work,
             "percent.Adults.with.Diabetes" = input$percent.Adults.with.Diabetes,
             "HIV.Prevalence.Rate" = input$HIV.Prevalence.Rate,
             "percent.Insufficient.Sleep" = input$percent.Insufficient.Sleep,
             "percent.Disconnected.Youth" = input$percent.Disconnected.Youth,
             "Average.Grade.Performance" = input$Average.Grade.Performance,
             "Segregation.index" = input$Segregation.index,
             "Spending.per.pupil" = input$Spending.per.pupil,
             "Gender.Pay.Gap" = input$Gender.Pay.Gap,
             "Median.Household.Income" = input$Median.Household.Income,
             "percent.Homeowners" = input$percent.Homeowners,
             "percent.Less.Than.18.Years.of.Age" = input$percent.Less.Than.18.Years.of.Age,
             "percent.65.and.Over" = input$percent.65.and.Over,
             "percent.Not.Proficient.in.English" = input$percent.Not.Proficient.in.English,
             "percent.female" = input$percent.female,
             "percent.rural" = input$percent.rural,
             "HPSA.Score.Primary" = input$HPSA.Score.Primary,
             "HPSA.Shortage.Primary" = input$HPSA.Shortage.Primary)
    )
    
    prediction_prob <- predict(
      model,
      tibble("percent.Fair.or.Poor.Health" = input$percent.Fair.or.Poor.Health,
             "percent.Smokers" = input$percent.Smokers,
             "percent.Adults.with.Obesity" = input$percent.Adults.with.Obesity,
             "Food.Environment.Index" = input$Food.Environment.Index,
             "percent.With.Access.to.Exercise.Opportunities" = input$percent.With.Access.to.Exercise.Opportunities,
             "percent.Excessive.Drinking" = input$percent.Excessive.Drinking,
             "Chlamydia.Rate" = input$Chlamydia.Rate,
             "Teen.Birth.Rate" = input$Teen.Birth.Rate,
             "percent.Uninsured.x" = input$percent.Uninsured.x,
             "Primary.Care.Physicians.Rate" = input$Primary.Care.Physicians.Rate,
             "percent.With.Annual.Mammogram" = input$percent.With.Annual.Mammogram,
             "percent.Vaccinated" = input$percent.Vaccinated,
             "percent.Completed.High.School" = input$percent.Completed.High.School,
             "percent.Unemployed" = input$percent.Unemployed,
             "percent.Children.in.Poverty" = input$percent.Children.in.Poverty,
             "Social.Association.Rate" = input$Social.Association.Rate,
             "Violent.Crime.Rate" = input$Violent.Crime.Rate,
             "Average.Daily.PM2.5" = input$Average.Daily.PM2.5,
             "percent.Severe.Housing.Problems" = input$percent.Severe.Housing.Problems,
             "percent.Drive.Alone.to.Work" = input$percent.Drive.Alone.to.Work,
             "percent.Adults.with.Diabetes" = input$percent.Adults.with.Diabetes,
             "HIV.Prevalence.Rate" = input$HIV.Prevalence.Rate,
             "percent.Insufficient.Sleep" = input$percent.Insufficient.Sleep,
             "percent.Disconnected.Youth" = input$percent.Disconnected.Youth,
             "Average.Grade.Performance" = input$Average.Grade.Performance,
             "Segregation.index" = input$Segregation.index,
             "Spending.per.pupil" = input$Spending.per.pupil,
             "Gender.Pay.Gap" = input$Gender.Pay.Gap,
             "Median.Household.Income" = input$Median.Household.Income,
             "percent.Homeowners" = input$percent.Homeowners,
             "percent.Less.Than.18.Years.of.Age" = input$percent.Less.Than.18.Years.of.Age,
             "percent.65.and.Over" = input$percent.65.and.Over,
             "percent.Not.Proficient.in.English" = input$percent.Not.Proficient.in.English,
             "percent.female" = input$percent.female,
             "percent.rural" = input$percent.rural,
             "HPSA.Score.Primary" = input$HPSA.Score.Primary,
             "HPSA.Shortage.Primary" = input$HPSA.Shortage.Primary)
    ) %>% 
      gather() %>% 
      arrange(desc(value)) %>% 
      slice(1) %>% 
      select(value)
    
    
        
    
    valueBox(
      value = paste0(round(prediction_prob$value, 0)),
      subtitle = paste0("Years of Potential Life Lost: ", prediction$.pred_class)
    )
    
  })
  
}

shinyApp(ui, server)
