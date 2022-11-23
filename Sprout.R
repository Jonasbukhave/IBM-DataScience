library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(tidyverse)
library(scales)
library(wordcloud)
library(gtrendsR)
library(ggthemes)
######################################################
#Load data
companies <- read.csv("Company_Cleaned_Non_Uni.csv")
investment <- read.csv("Investment_Cleaned_Non_Uni.csv")
unicorns <- read.csv("Unicorn_Cleaned.csv")
world_population <-read.csv("world_population.csv")


############
#Globeplot data prep (plot 5)
############

#Group data for globeplot
grouped_globeplot <- companies %>% drop_na(country) %>% filter(country != 'Bermuda') %>% group_by(country, country_code) %>% 
  summarize(n_companies =n(), median_funding=median(funding_total_usd), mean_funding=mean(funding_total_usd))
#Globeplot styles:

#Set country boundaries as light grey
l <- list(color = toRGB("#d1d1d1"), width = 0.4)
#Specify map projection and options
g <- list(showframe = F, showcoastlines = F,
          projection = list(type = 'orthographic'),
          resolution = '100', showcountries = T, countrycolor = '#d1d1d1',
          showocean = T, oceancolor = '#c9d2e0', showlakes = T,
          lakecolor = '#99c0db', showrivers = T, rivercolor = '#99c0db')


############
#Plot 6 data prep (two plots)
############

#Grouping unicorns by country
unicorns_country <- unicorns %>% group_by(Country) %>%
  summarise(total_unicorns = n(),
            .groups = 'drop')
#Merging with world_population dataframe
unicorns_country<-merge(x=unicorns_country,y=world_population,by="Country",all.x=TRUE)
#Creating column showing unicorns per 1 million inhabitants of a country
unicorns_country$unicorns_per_1m<-unicorns_country$total_unicorns/unicorns_country$X2022.Population*10^6


############
#Plot 7 data prep
############

#Grouping unicorns by industry and year
unicorns_industry_year <- unicorns %>% group_by(Industry,Year.Joined) %>%
  summarise(unicorns_n = n(),
            .groups = 'drop')

############
#Plot 8 data prep
############

#Perhaps this and plot 7 data can easily be combined by adding the valuation summarize to above.
byValuation <- unicorns %>% group_by(Industry, Year.Joined) %>% summarise(Valuation = Valuation...B.)
byValuation$Year <- as.factor(byValuation$Year.Joined)



############
#Plot 9 data prep
############

# Unicorn Per industry 
uni_byIndustry <- unicorns %>% group_by(Industry) %>% summarise(Count = n())
uni_byIndustry <- uni_byIndustry %>% mutate(Perc = Count / sum(Count)) %>% mutate(Labels = scales::percent(Perc)) 


############
#Plot 10 data prep
############

#Unicorn data prep
uni_byInvestors <- unicorns %>% select(Company, Country, Industry, Investor.1:Investor.4)
uni_byInvestors <- uni_byInvestors %>% gather("Investor_Num", "Investor", Investor.1:Investor.4) 
uni_byInvestors <- uni_byInvestors %>% mutate_all(na_if,"")
uni_byInvestors <- uni_byInvestors[complete.cases(uni_byInvestors), ] #Dropping all with no known investor
uni_byInvestors$Investor <- as.factor(uni_byInvestors$Investor)
uni_byInvestors <- uni_byInvestors %>% group_by(Company) %>% summarise(Company, Country, Industry, Investor)

#Non unicorn data prep
nonuni_byInvestors <- left_join(companies, investment, by=c("name"="company_name"))
nonuni_byInvestors <- nonuni_byInvestors[complete.cases(nonuni_byInvestors),]
nonuni_byInvestors <- nonuni_byInvestors %>% group_by(name) %>% summarise(name, market, country.y, investor_name)
colnames(nonuni_byInvestors) <- c("Company", "Industry", "Country", "Investor")



################################################################################
#UI 
ui <- fluidPage(theme = shinytheme("cerulean"), #cerulean arbitrary theme
                navbarPage(
                  title= "Sprout",
                  tabPanel(title="Gtrend",
                           sidebarLayout(
                             sidebarPanel(
                               #Text boxes
                               h6(" Search Term(s)",style="text-align:center;color:#FFA319;font-size:150%"),
                               helpText("Give one or more terms that you want R to retrieve data from the Google Trends API.
                                         Use comma to separate terms. Lastly Click the 'Submit' Button.", style="text-align:center"),
                               br(),
                               helpText("Note: Wordcloud only working with 1 searchterm", style="text-align:center"),
                               
                               #widget
                               textInput(inputId = "gtrend_input", label=h3("Write industry or technology"), placeholder="Type here....", value= "Tech"), #textInput
                               selectInput(inputId = "gtrend_input_timeline", label="Choose Timehorizon", choice=list("Last day"="now 1-d", "Last seven days"="now 7-d", "Past 30 days"="today 1-m", "Past 12 months"= "today 12-m","Last five years" = "today+5-y")),
                               submitButton("Submit")
                               
                             ), #sidebarPanel
                             
                             mainPanel(
                               shinycssloaders::withSpinner(
                                 plotOutput(outputId="plot1_output")
                               ),#Spinner
                               
                               br(),
                               
                               shinycssloaders::withSpinner(
                                 plotOutput(outputId="plot2_output")
                               ), #Spinner
                               
                               br(),
                               
                               shinycssloaders::withSpinner(
                                 plotOutput(outputId="plot3_output")
                               ), #Spinner
                               shinycssloaders::withSpinner(
                                 plotOutput(outputId="plot55_output")
                               ), #Spinner
                             ) #mainPanel
                           ) #sidebarLayout
                           
                  ), #TabPanel1
                  
                  
                  
                  tabPanel(title="Plot6", #Storing all UI when pressing first tabPanel
                           
                           #All code here in main should be moved to Unicorn companies and change the layout of this one. For now i leave it here.
                           sidebarLayout(
                             sidebarPanel(
                               #Widgets
                               radioButtons(inputId = "plot6_input", label = h3("Select the plot"), choices = list("Show ratio"= 1, "show total"=2), selected=1),
                               sliderInput(inputId = "plot6_n_input",label = h3("Select number of countries to show"),  min=1, max = 15, value = 10)
                               
                             ),#SidebarPanel
                             
                             #mainPanel for displaying output
                             mainPanel(
                               plotOutput(outputId = "plot6_output")
                             ) #mainPanel
                           ) #sidebar layout
                           
                  ), #Tabpanel1.  
                  
                  tabPanel(title="Unicorn companies",
                           #Sidebar layout
                           sidebarLayout(
                             
                             #Sidebarpanel
                             sidebarPanel(
                               
                               #Input: Dropdown-list
                               selectizeInput(
                                 inputId = "plot7_input", 
                                 label = "Select one or multiple industries", 
                                 choices = sort(unique(unicorns$Industry)), 
                                 selected = "Artificial intelligence",
                                 multiple = T
                               ) #selecticeInput
                             ), #sidebarpanel
                             
                             #Main-panel for displaying output 
                             mainPanel(
                               
                               #Output 
                               plotlyOutput(outputId = "plot7_output"),
                               plotOutput(outputId = "plot8_output")
                               
                               
                             )#Mainpanel
                             
                           )#sidebarlayout  
                           
                           
                  ), #Tabpanel2.
                  
                  #Tabpanel for Non-unicorn companies data
                  tabPanel(title="Non-unicorn companies",
                           
                           #Sidebar layout
                           sidebarLayout(
                             
                             #Sidebarpanel
                             sidebarPanel(
                               
                               #Input: Radiobuttons
                               radioButtons(inputId ="globeplot_input", label = h4("Choose information for funding"),
                                            choices = list("Median" = 1, "Mean" = 2, "Number of companies" = 3, "All" = 4), 
                                            selected = 1)
                               
                             ), #sidebarpanel
                             
                             #Main-panel for displaying output 
                             mainPanel(
                               
                               #Output globeplot
                               plotlyOutput(outputId = "globeplot")
                             )#Mainpanel
                             
                           )#sidebarlayout  
                           
                           
                  ), #Tabpanel3.
                  
                  
                  #Tabpanel 4 for plot 9
                  tabPanel(title = "Plot 9&10 (combine later)",
                           
                           #sidebarLayout
                           sidebarLayout(
                             
                             #sidebarPanel
                             sidebarPanel(
                               wellPanel(
                                 #Input: Radiobuttons
                                 radioButtons(inputId ="plot9_input", label = h4("Choose plot format"),
                                              choices = list("Pie Chart" = 1, "Bar Chart" = 2), 
                                              selected = 1)
                                 
                               ), #wellpanel
                               wellPanel(), #wellpanel 2
                               wellPanel(
                                 
                                 #Widgets
                                 checkboxInput(inputId = "plot10_input","Show for Unicorns", value=TRUE),
                                 selectInput(inputId="plot10_select", label=h3("Select Industry"), choices= NULL),
                                 sliderInput(inputId = "plot10_n_input",label = h3("Select number top investors to show"),  min=1, max = 15, value = 5)
                                 
                                 
                               ), #wellpanel 3 
                             ), #sidebarpanel
                             
                             #mainPanel
                             mainPanel(
                               plotOutput(outputId="plot9_output" ), #plotOutput plot9
                               
                               #Need to create some sort of spacer here:
                               
                               plotOutput(outputId= "plot10_output") #plotOutput plot10
                               
                             ) #mainPanel
                           ) #SidebarLayout
                  ) #tabPanel 4.
                  
                  
                ) #Navbar
) #Fluidpage



################################################################################
# Server

server <- function(input, output, session) {
  
  #Reactive function for keyword splitting
  keyword_splitter <- reactive({
    if (length(input$gtrend_input) > 0) {
      unlist(strsplit(input$gtrend_input, ","))
    }
  }) #keyword_splitter
  
  #Reactive function for gtrend search
  searcher <- reactive({
    if (length(input$gtrend_input != 0))
      req(input$gtrend_input)
    {
      gtrends(keyword = keyword_splitter(),
              time = input$gtrend_input_timeline,
              gprop=c("web", "news", "images", "froogle", "youtube"))
      }
  }) #searcher
  
  
  #Output Plot 1
  output$plot1_output <- renderPlot({
    trends = searcher()
    trendsiot <- trends$interest_over_time
    ggplot(data=trendsiot) + geom_line(aes(x=date,y=hits, group=keyword, color = keyword))
    
  }) #Output Plot1
  
  
  # plot 2
  output$plot2_output <- renderPlot({
    
    trends = searcher()
    # Processing of dataset from gtrends by country
    iot <- trends$interest_by_country
    iotnew <- iot %>% mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>% # replace '<1' with 0.5
      mutate_at("hits", ~as.numeric(.)) # convert to numeric
    
    # Filter countries with more than 25 'hits'
    iotfilter <- iotnew %>% filter(hits > 25)
    
    # Plot barchart for countries
    ggplot(data = iotfilter) + 
      geom_bar(aes(x = reorder(location, hits),y = hits), stat = 'identity') + 
      theme_economist() +
      labs(
        x = "Countries",
        y = "Hit rate",
        title = "Trends by top countries",
        caption = "Courtesy: gtrendsR package") + 
      theme(
        title = element_text(face = "bold", color = "black"),
        axis.title = element_text(face = "bold", color = "black"),
        panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  
  
  #Plot 3 wordcloud
  output$plot3_output <- renderPlot({
    
    # Processing of dataset from gtrends for related topics
    trends = searcher()
    testb <- trends$related_topics
    testb$subject[testb$subject == 'Breakout'] <- '95'
    testb$subject <- as.numeric(testb$subject)
    testb <- testb[!is.na(testb$subject), ]
    
    # Plot wordcloud
    wordcloud(words = testb$value, freq = testb$subject, 
              min.freq = 10, max.words=200, random.order=FALSE, 
              rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(1.5,0.7)
    ) #word cloud
    
    
  }) #Output plot 3 wordcloud
  
  
  
  
  #Plot for globe (plot 5).
  output$globeplot <- renderPlotly({
    #Creating if statements depending on the result of the radiobuttons.
    #median
    if (input$globeplot_input == 1) {
      #Text for hovering
      grouped_globeplot$hover <-
        with(grouped_globeplot,
             paste(country, '<br>', "Number of companies:", n_companies))
      #Choosing color and target for globeplot.
      grouped_globeplot$target <- grouped_globeplot$median_funding
      text.target <- 'Median funding, $USD'
    }
    #mean
    else if (input$globeplot_input == 2) {
      grouped_globeplot$hover <-
        with(grouped_globeplot,
             paste(country, '<br>', "Number of companies:", n_companies))
      grouped_globeplot$target <- grouped_globeplot$mean_funding
      text.target <- 'Mean funding, $USD'
    }
    #n_companies
    else if (input$globeplot_input == 3) {
      grouped_globeplot$hover <-
        with(grouped_globeplot, paste(country, ""))
      grouped_globeplot$target <- grouped_globeplot$n_companies
      text.target <- 'number of companies'
      
    }
    #all
    else if (input$globeplot_input == 4) {
      #Creating the text for hovering:
      grouped_globeplot$hover <-
        with(
          grouped_globeplot,
          paste(
            country,
            '<br>',
            "Number of companies:",
            n_companies,
            '<br>',
            "Mean_funding, usd$:",
            label_number(scale_cut = cut_short_scale())(round(mean_funding, digits =
                                                                2))
          )
        )#For readability
      grouped_globeplot$target <- grouped_globeplot$median_funding
      text.target <- 'Median funding, $USD'
    }
    p <- plot_geo(grouped_globeplot) %>%
      add_trace(
        z = ~ target,
        color = ~ target,
        colors = 'Reds',
        text = ~ hover,
        locations = ~ country_code,
        marker = list(line = l)
      ) %>%
      colorbar(title = text.target) %>%
      layout(title = paste(''), geo = g)
    ggplotly(p, height = 200, width = 100)
  }) #Output globeplot (plot 5)
  
  
  #Plot 5.5 
  output$plot55_output <- renderPlot({
    
    # Get top 4 countries from gtrends into non-unicorn dataset
    trends = searcher()
    # Processing of dataset from gtrends by country
    iot <- trends$interest_by_country
    iotnew <- iot %>% mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>% # replace '<1' with 0.5
      mutate_at("hits", ~as.numeric(.)) # convert to numeric
    
    # Filter countries with more than 25 'hits'
    iotfilter <- iotnew %>% filter(hits > 25) 
    top4 <- iotfilter$location[1:4]
    companiesf <- companies %>% drop_na(country) %>% filter(country != 'Bermuda') %>% filter(funding_total_usd > 0) %>% subset(country %in% top4)
    
    #Plot boxplot with log y scale
    ggplot(companiesf, aes(x=country,y=funding_total_usd)) + geom_boxplot() + scale_y_log10() + theme_economist() +
      labs(
        x = "Countries",
        y = "Total funding (log)",
        title = "Total funding for companies in top countries") +
      theme(
        title = element_text(face = "bold", color = "black"),
        axis.title = element_text(face = "bold", color = "black"),
        panel.grid.major.x = element_blank()
      )
    
  })
  
  
  
  
  
  #Plot 6 Unicorn data
  output$plot6_output <- renderPlot({
    
    if (input$plot6_input == 1){
      #Sorting top n countries based on relative values
      top_n_countries <- top_n(unicorns_country, input$plot6_n_input, unicorns_per_1m)
      top_n_countries$target <- top_n_countries$unicorns_per_1m
      text_ylabel <- "Number of unicorns per. 1 million inhabitants"
    } # If == ratio
    
    else if (input$plot6_input == 2){
      #Sorting top n countries based on absolute values
      top_n_countries <-top_n(unicorns_country, input$plot6_n_input, total_unicorns)
      top_n_countries$target <- top_n_countries$total_unicorns
      text_ylabel <- "Number of unicorns"
    } # If == total
    
    #Creating a bar chart to show countries with most unicorns
    top_n_countries %>%
      ggplot( aes(x=reorder(Country, target), y=target)) +
      geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
      coord_flip() +
      xlab("Country") +
      ylab(text_ylabel) +
      ggtitle("Number of unicorns, country")+
      theme_bw()
    
  }) #Output Plot6
  
  
  #Plot 7 Unicorn data
  output$plot7_output <- renderPlotly({
    p <-
      unicorns_industry_year %>% filter(Industry %in% input$plot7_input) %>%
      group_by(Industry) %>%
      plot_ly(
        x =  ~ Year.Joined,
        y =  ~ unicorns_n,
        color =  ~ Industry
      ) %>% #plot_ly
      add_lines() %>%
      layout(
        title = 'Development of Unicorns Over Time',
        plot_bgcolor = "#e5ecf6",
        yaxis = list(title = "Number of unicorns pr. Industry"),
        xaxis = list(title = "Year"),
        legend = list(title = list(text = (
          '<b> Industry </b>'
        )))
      ) #layout
    
    ggplotly(p, width = 200, height = 200)
  }) #output Plot7
  
  
  #Plot 8 Unicorn data
  output$plot8_output <- renderPlot({
    
    ggplot(byValuation %>% filter(Industry %in% input$plot7_input), #using same input as plot7 (Name should be changed)
           aes(x=Year, y=Valuation, fill=Year)) + geom_boxplot() + ylab("Valuation in Billions") + ggtitle(paste0(input$plot7_input,collapse=" / ")) + facet_wrap(~Industry, scale='free') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))
  }) #output Plot8
  
  
  output$plot9_output <- renderPlot({
    #This plot should switch between doing the bar chart or doing a pie chart 
    
    #Missing the color code_line here.
    color <- colorRampPalette(brewer.pal(9, "PuBu"))(length(unique(uni_byIndustry$Industry)))
    
    
    if (input$plot9_input == 1){ #pie chart
      
      pp <- ggplot(uni_byIndustry, aes(x = "", y = Perc, fill = Industry)) + 
        geom_col() + 
        coord_polar(theta = "y") + 
        theme_void() + ggtitle("Percentage of Unicorns by Industry") + 
        scale_fill_manual(values = color)
    } #if 
    
    else if (input$plot9_input == 2){ #bar chart
      
      pp <- ggplot(uni_byIndustry, aes(x = Industry, y = Count)) + 
        geom_bar(stat='identity') + 
        coord_flip() + 
        geom_label(aes(label=Labels), vjust = 0.5, size=2) + 
        ggtitle("Unicorns Per Industry") + 
        theme_light()
    } #else if 
    
    print(pp)
  }) #output Plot9
  
  
  output$plot10_output <- renderPlot({
    
    #Three inputs: 
    #plot10_n_input == slider 
    #plot10_input  == (True/False)
    #plot10_select == String with name of industry
    
    if (input$plot10_input == T){ #Unicorn data
      # Creating plot
      p10 <- ggplot(uni_byInvestors %>% filter(Industry %in% input$plot10_select) 
                    %>% group_by(Investor) %>% summarise(Count = n()) 
                    %>% slice_max(Count, n=input$plot10_n_input), aes(x=Investor, y=Count)) + 
        geom_bar(stat='identity') + 
        ggtitle(paste0("Top ", input$plot10_n_input, " Investors in ", input$plot10_select)) + 
        coord_flip()
    } #if
    
    else if (input$plot10_input == F){ #Non-unicorn data
      # Creating plot
      p10 <- ggplot(nonuni_byInvestors %>% filter(Industry %in% input$plot10_select) 
                    %>% group_by(Investor) %>% summarise(Count = n()) 
                    %>% slice_max(Count, n=input$plot10_n_input), aes(x=Investor, y=Count)) + 
        geom_bar(stat='identity') + 
        ggtitle(paste0("Top ", input$plot10_n_input, " Investors in ", input$plot10_select)) + 
        coord_flip()
    } #else if
    print(p10)
  }) #output Plot10
  
  observe({ 
    #Observe function for plot10 to create a dynamic select list depending on chosen dataset
    if (input$plot10_input == TRUE){ #if unicorn data
      select_list <- unique(uni_byInvestors$Industry)
    } #if
    else if (input$plot10_input == FALSE){
      select_list <- unique(nonuni_byInvestors$Industry)
    } #else if
    #Update plot 10 input selection
    select_list <- relist(sort(unlist(select_list)), select_list)
    updateSelectInput(session, inputId = "plot10_select", choices = select_list)
  }) #observe for plot10
  
  
  
  
} # server

################################################################################
# Create shiny object
shinyApp(ui = ui, server = server)
