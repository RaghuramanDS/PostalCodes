

# A shiny app on postal codes to experiment the features of R packages
library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(shinythemes)
library(stringr)


ui <- fluidPage(
  theme = shinytheme("readable"),
  
  titlePanel("Postal Code Information in India"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        div(
          style = "text-align:justify",
          h5(
            "Please use the filter or Search facility at the top of the table in the PO Details tab to find the place you are looking for. Copy and paste the name of the place in the text box below  to display location in the map"
          )
        ),
        
        h5("-------------------------------------")
        
      ),
      
      fluidRow(textInput(
        "placeString", "Please type the name of the place ( without B.O, S.O or H.O )"
      )),
      
      fluidRow(div(
        style = "text-align:justify",
        
        h5(
          "Please note, the database with Latitude and Longitude is not exhaustive and reflects data as available in data.gov.in as on 18 Sep 2019"
        ),
        h5(
          " Some Pin codes are missing and a few data items appear to be incorrect, you may report them to the Chief Data Officer."
        ),
        h5("PO Details tab contains a more exhaustive list without Latitude and Longitude "),
        tags$a(
          href = "https://data.gov.in/catalog/all-india-pincode-directory?filters
                       %5Bfield_catalog_reference
                       %5D=85840&format=json&offset=0&
                       limit=6&sort
                       %5Bcreated%5D=desc",
          "Click to go to site  data.gov.in !"
        ),
        h6(
          " Disclaimer: This application is only meant to showcase the features of R and the author is not responsible for any action arising out of this application."
        )
      )
      ),
      fluidRow(column(
        width = 1, tags$img(src = 'contact officer.png', width = "1800px")
      ))
      
    ),
    
    
    
    # Show a map with chosen postal code's location
    mainPanel(tabsetPanel(
      type = "tab",
      
      tabPanel("PO Details",
               dataTableOutput("podetails")
      ),
      tabPanel(
        "PO Location",
        fluidRow(dataTableOutput("postaldata")),
        fluidRow(leafletOutput("postalcode")),
        fluidRow(
           div ( style = "text-align:justify", 
           h4(" 1. Only 1300 Post offices have Latitude and Longitude                   details out of 1,54,798 POs in the PO details Table"),
           h4(" 2. Place name in the database and the place name in the map               may not match !")
           )
         )
       ),
       tabPanel("Credits",
               plotOutput("Credits"))
       
    ) 
   )
  )
)


# Define server logic required to show a map with postal code
server <- function(input, output) {
  
  
  
  pscode <- read_tsv("India_Geo codes.txt")
  
  poAll <- read_csv("AllPOList.csv")
  
  pscode <- pscode[, -c(6:7)]
  pscode <- as.data.frame(pscode)
  pscode$Latitude <- as.numeric(pscode$Latitude)
  pscode$Longitude <- as.numeric(pscode$Longitude)
  # Create a new column with name "PinCode"
  pscode$PinCode <- pscode$`PIN code`
  pscode$PinCode <- as.character(pscode$PinCode)
  # let us drop the "Pin Code" column
  pscode <- pscode[, -5]
  
  output$postalcode <- renderLeaflet({
    pscodeRequired <-
      pscode[which(pscode$Location == input$placeString), ]
    pscodeRequired %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(
        lng = pscodeRequired$Longitude,
        lat = pscodeRequired$Latitude,
        popup = ~ paste(pscodeRequired$Location, pscodeRequired$State)
      ) %>%
      setView(lat = pscodeRequired$Latitude,
              lng = pscodeRequired$Longitude,
              zoom = 10)
    
  })
  
  output$postaldata <- renderDataTable({
    pscodeRequired <-
      pscode[which(pscode$Location == input$placeString),]
    #display only table and nothing else by specifying dom element
    datatable(
        pscodeRequired,
        
        options = list(
        dom = 't',
        paging = FALSE,
        language = list(zeroRecords = "Couldn't choose / find a record yet !"),
        
        editable = FALSE
        )
   )
  })
  
  output$podetails <- renderDataTable({
    psdata2 <-
      poAll[which(str_detect(poAll$officename , input$placeString)),]
    pretty_headers <- colnames(psdata2) %>% str_to_title()
    datatable(
      psdata2,
      rownames = FALSE,
      colnames = pretty_headers,
      
      filter = list(position = "top"),
      extensions = c("Responsive", "Buttons"),
      options = list(
        pageLength = 10,
        buttons = I("colvis"),
        dom = "Bfp"
        ),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        htmltools::strong(em('Post Offices in India with Pin code and related Sub O/ Head Offices'))
      ),
      editable = FALSE
      
    )
    
  })
 
 
  output$Credits <- renderImage ({
    filename <- normalizePath(file.path('creditpagenew.png'))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
 
}


# Run the application
shinyApp(ui = ui, server = server
)
