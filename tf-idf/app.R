#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(forcats)
library(dplyr)
library(ggplot2)
library(bslib)
library(shinyjs)
library(purrr)

remove_list = c("Afghanistan", "Albania", "Algeria", "ASM", "ASM", "Andorra", "Angola", "Anguilla", "ATG", "ATG", "ATG", 
                "Argentina", "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
                "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "BIH", 
                "BIH", "Botswana", "Brazil", "BRN", "BRN", "Bulgaria", "BFA", "BFA", "Burundi", "CPV", "CPV", "Cambodia", 
                "Cameroon", "Canada", "CYM", "CAF", "CAF", "Chad", "Chile", "China", "HKG", "Macau", "Colombia", "Comoros", 
                "cod", "cod", "cod", "cog", "cog", "cog", "COK", "COK", "CRI", "CRI", "Croatia", "CIV", "CIV", "CIV", "CIV", 
                "CIV", "CIV", "Cuba", "CUW", "Cyprus", "Czechia", "PRK", "PRK", "Denmark", "Djibouti", "DOM", "DOM", 
                "Ecuador", "Egypt", "SLV", "SLV", "GNQ", "GNQ", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "FRO", "FRO", 
                "Fiji", "Finland", "France", "PYF", "FSM", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", 
                "Greece", "Greenland", "Grenada", "Guam", "Guatemala", "Guinea", "GNB", "GNB", "Guyana", "Haiti", "Honduras", 
                "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", 
                "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Laus", "Latvia", 
                "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", 
                "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "MHL", "MHL", "Mauritania", "Mauritius", 
                "Mexico", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", 
                "Nauru", "Nepal", "Netherlands", "NCL", "NZL", "NZL", "Nicaragua", "Niger", "Nigeria", "Niue", "MKD", 
                "Norway", "Oman", "Pakistan", "Palau", "Panama", "PNG", "PNG", "Paraguay", "Peru", "Philippines", "Poland", 
                "Portugal", "Qatar", "KOR", "Moldova", "Romania", "Russia", "Russia", "Rwanda", "SHN", "KNA", "KNA", "LCA", 
                "SPM", "SPM", "SPM", "VCT", "Samoa", "SMR", "SMR", "STP", "STP", "SAU", "Saudi Arabia", "Senegal", "Serbia", 
                "Seychelles", "SLE", "SLE", "Singapore", "Slovakia", "Slovenia", "SLB", "SLB", "Somalia", "ZAF", "ZAF", 
                "SSD", "SSD", "Spain", "LKA", "LKA", "Palestine", "Palestine", "Sudan", "Suriname", "Sweden", "Switzerland", 
                "Syria", "Syria", "Tajikistan", "Thailand", "TLS", "TLS", "Togo", "Tokelau", "Tonga", "TTO", "TTO", "TTO", 
                "Tunisia", "Türkiye", "Turkmenistan", "TCA", "TCA", "Tuvalu", "Uganda", "Ukraine", "ARE", "ARE", "GBR", "gbr", 
                "gbr", "gbr", "GBR", "TZA", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "VNM", "VNM", "WLF", "ESH", "ESH", 
                "Yemen", "Zambia", "Zimbabwe", "taiwan", "macedonian", "bosnia", "bissau", "antigua", "herzegovina", "congo",
                "pse", "ven", "dominica", "dominican", "tobago", "trinidad", "tanzania", "vincent", "grenadines") %>% tolower()

# JavaScript code for toggling fullscreen
jsToggleFS <- '
shinyjs.toggleFullScreen = function() {
    var element = document.documentElement,
      enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
      exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
    if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
      enterFS.call(element);
    } else {
      exitFS.call(document);
    }
  }
'

# Load data
speeches_tf_idf <- readRDS(file = "speeches_tf_idf.RDS")
speeches_tf_idf$ISO2[speeches_tf_idf$ISO3=="NAM"]
speeches_tf_idf = speeches_tf_idf %>% 
  filter(!c(word %in% remove_list))

unique_iso <- speeches_tf_idf %>% select(ISO3, ISO2, Country) %>% distinct() 
unique_iso$ISO2 = unique_iso$ISO2 %>% tolower()
unique_iso$img_urls <- paste0(
  'https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/',
  unique_iso$ISO2, '.svg'
)
unique_iso$group = "group"
unique_iso$group[1:10] = "group2"


# Define UI for app
ui <- page_sidebar(
  useShinyjs(),  # Initialize shinyjs
  extendShinyjs(text = jsToggleFS, functions = c("toggleFullScreen")),  
  
  titlePanel("TF-IDF for GA Speeches (2024)"),
  
  sidebar = sidebar(
    width = 340,
    actionButton("fullscreen_btn", "Fullscreen"),
    multiInput(
      inputId = "countries", 
      label = "Add and remove countries:",
      choices = NULL,
      choiceNames = unique(unique_iso$group) %>%
        map(function(group) {
          which(unique_iso$group == group) %>%
            map(function(i) {
              tagList(
                tags$img(src = unique_iso$img_urls[i], width = 20, height = 15),
                unique_iso$Country[i]
              )
            })
        }) %>%
        flatten() ,
      choiceValues = unique_iso$ISO3,
      selected = c("USA", "LEB", "CRI", "ZAF", "UKR", "CHN","PSE")  #"ISR", 
    ),
    # Explainer box for TF-IDF
    wellPanel(
      h3("What is TF-IDF?"),
      tags$ul(
        tags$li(tags$b(tags$a(href="https://en.wikipedia.org/wiki/Tf%E2%80%93idf", "TF-IDF")), " measures a word's importance in a document."),
        tags$li(tags$b("TF"), " (Term Frequency) counts how often the word appears in one speech."),
        tags$li(tags$b("IDF"), " (Inverse Document Frequency) measures how rare the word is across all speeches."),
        tags$li("Combined they show the words used a lot in one speech but not used much in other speeches.")
      )
    ),
    hidden(
      numericInput(inputId = "window_width", value = 600, label="window width")
    )
  ),
    plotOutput("plot"),
  
  # JavaScript to update window width on resize
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var width = $(window).width();
      Shiny.setInputValue('window_width', width);
    });
    $(window).resize(function() {
      var width = $(window).width();
      Shiny.setInputValue('window_width', width);
    });
  "))
  
)

# Define server logic
server <- function(input, output, session) {
  col_function = function(){
    max((input$window_width/400) %>% floor(), 1)
  }
  
  height_function = function() {
    (length(input$countries) / col_function()) %>% ceiling() * 400 # in the case of ncol=col_function
  }
  

  
  output$plot <- renderPlot({
    ISO3s = input$countries
    df <- speeches_tf_idf %>%
      filter(ISO3 %in% ISO3s) %>% 
      group_by(ISO3) %>%
      slice_max(tf_idf, n = 15, with_ties=FALSE) %>%
      mutate(word_ISO3 = interaction(word, ISO3, drop = TRUE)) %>%
      group_by(ISO3) %>%
      mutate(word_order = fct_reorder(word_ISO3, tf_idf)) %>%
      ungroup()
    df = df %>% 
      mutate(word_ISO3 = factor(word_ISO3, levels=rev(word_ISO3)))
    
    # Plotting
    ggplot(df, aes(x = tf_idf, y = word_ISO3, fill = Country)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~Country, ncol = col_function(), scales = "free") +
      labs(x = "tf-idf", y = NULL) +
      theme_minimal() +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 2)) +
      theme(axis.text.x = element_text(size = 8)) +
      scale_y_discrete(labels = function(labels) gsub("\\..*", "", labels))
    
  }, res = 140, height = height_function)
  
  observeEvent(input$fullscreen_btn, {
    js$toggleFullScreen()
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
