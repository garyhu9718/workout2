library(shiny)
library(ggplot2)
library(reshape2)
#ui
ui = fluidPage(
  titlePanel("The saving balance demonstration"),
  fluidRow(
    column(4,
           sliderInput("amount", "Initial Amount", max = 100000, min = 0, step = 500, value = 1000)),
    column(4,
           sliderInput("return", "Return Rate (in %)", max = 20 , min = 0, step = 0.1, value = 5)),
    column(4,
           sliderInput("years", "Years", max = 50, min = 0, step = 1, value = 20))
    ),
  
  fluidRow(
    column(4,
           sliderInput("contribution", "Annual Contribution", min = 0, max = 50000, step = 500, value = 2000)),
    column(4,
           sliderInput("growth", "Growth Rate", min = 0, max = 20, step = 0.1, value = 2)),
    column(4,
          selectInput("facet", "Facet?", choices = c("Yes", "No"), selected = "No"))
  ),

  fluidRow(
    column(12,
           h4("Timelines"),
           plotOutput("line_plot"))
  ),
  
 fluidRow(
   column(12,h4("Balance"),tableOutput(("table")))
 )
)
#server
server = function(input, output){
   dat = reactive({
       future_value = function(anount = 100, rate = 0.05, years = 1){ 
       return(anount * (1+rate)^years)
       }
       
       annuity = function(contrib = 100, rate = 0.05, years = 10){
         return(contrib * ((1+rate)^years -1) / rate)
       }
       
       growing_annuity = function(contrib = 100, rate = 0.05, growth = 0.03, years = 1){
         return(contrib * ((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth))
       }
       
       no_contrib = rep(0,input$years+1)
       fixed_contrib = rep(0,input$years+1)
       growing_contrib = rep(0,input$years+1)
       years = 0:input$years
       for(i in 1:input$years+1){
         no_contrib[i] = future_value(input$amount, input$return / 100,i-1)
         fixed_contrib[i] = future_value(input$amount,input$return / 100,i-1) + annuity(input$contribution,input$return / 100,i-1)
         growing_contrib[i] = future_value(input$amount,input$return / 100,i-1) + growing_annuity(input$contribution,input$return / 100,input$growth / 100,i-1)
       }
       no_contrib[1] = input$amount
       fixed_contrib[1] = input$amount
       growing_contrib[1] = input$amount
       
       data = data.frame(years,no_contrib,fixed_contrib,growing_contrib)
      
      data
   })
   output$table = renderTable({dat()})
   output$line_plot = renderPlot({
     data2 = melt(dat(), measure.vars = c("no_contrib", "fixed_contrib", "growing_contrib"),id.vars = "years")
     if(input$facet == "Yes"){
       ggplot(data2, aes(x = years, y = value))+
         geom_area(aes(color = variable, fill = variable), alpha = 0.2)+
         geom_point(aes(color = variable))+
         ylab("future values")+
         ggtitle("The future values of different modalities")+
         facet_grid(~ variable)
     }
     else{
       ggplot(data2, aes(x = years, y = value))+
         geom_line(aes(color = variable))+
         ylab("future values")+
         ggtitle("The future values of different modalities")
     }
   })
}

shinyApp(server = server, ui = ui)



