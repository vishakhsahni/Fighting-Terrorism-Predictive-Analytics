library(shiny)

 fluidPage(    
  
  titlePanel("Number of Attacks Predicted by Country"),
  
  sidebarLayout(      
    
    sidebarPanel(
      selectInput("Country", "Select Country From the Drop Down Menu:",
                  choices = mydata$Country),
      hr(),
      helpText(strong("In Class AIT 582 Final Project!")),
      helpText(strong("Guided by Dr. Setareh Rafatirad")),
      helpText(strong("George Mason University")),
      helpText(strong("Presented by Vishakh Sahni")),
      helpText(strong("Email: vsahni@gmu.edu")),
      helpText(em("Note: Data is Extracted from START Organization, University Of Maryland")) 
      ),

    mainPanel(h4(strong("Visualization of Attacks Predicted")),
    img(src="terr2.png", hieght =300, width = 300), 
    img(src="terr.png", hieght=300, width = 300),
      plotOutput("bar")  
    )
    
  )
)

 