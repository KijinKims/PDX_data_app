library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(DT)
library(shinythemes)
library(shinycssloaders)

PrimarySite = c("Breast", "Gastric", "GIST", "Colon")
Tumortype = c("Primary", "Metastatic", "Undefined")
Subtype = c("TNBC", "Luminal", "HR+ HER2+", "HR- HER2+", "Unclassified")
tagList(
  navbarPage("Korean PDX",
             theme = shinytheme("yeti"),
             id = "tabs",
             tags$head(tags$link(rel = "stylesheet",
                                 type = "text/css",
                                 href = "my.css"),
                       tags$style(HTML(
                         '
                         .modal-header .modal-title {
                         font-size: 35px;
                         }
                         '
                       ))),
             tags$style("#mstable tfoot {display:none;}"),
             tags$style("#mgtable tfoot {display:none;}"),
             selected = "samplelist",
             tabPanel("Sample List",
                      value = "samplelist",
                      icon = icon("list"),
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            width = 4,
                            div(
                              id = "form",
                              pickerInput(
                                inputId = "primarysite",
                                label = "Primary Site",
                                choices = PrimarySite,
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = paste0("count > ", length(PrimarySite) - 1),
                                  `count-selected-text` = "All"
                                ),
                                multiple = TRUE
                              ),#pickerInput
                              pickerInput(
                                inputId = "tumortype",
                                label = "Tumor Type",
                                choices = Tumortype,
                                selected = Tumortype,
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = paste0("count > ", length(Tumortype) -
                                                                    1),
                                  `count-selected-text` = "All"
                                ),
                                multiple = TRUE
                              ),#pickerInput
                              pickerInput(
                                inputId = "donorsex",
                                label = "Donor Sex",
                                choices = c("Female", "Male"),
                                selected = c("Female", "Male"),
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count",
                                  `count-selected-text` = "All"
                                ),
                                multiple = TRUE
                              ),#pickerInput
                              sliderInput(
                                inputId = "donorage",
                                label = "Donor Age",
                                min = 0,
                                max = 100,
                                value = c(0, 100),
                                dragRange = TRUE
                              ),#sliderInput
                              pickerInput(
                                inputId = "patientsubtype",
                                label = "Breast Cancer Patient Subtype",
                                choices = Subtype,
                                selected = Subtype,
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = paste0("count > ", length(Subtype) -
                                                                    1),
                                  `count-selected-text` = "All"
                                ),
                                multiple = TRUE
                              ),#pickerInput
                              pickerInput(
                                inputId = "PDXsubtype",
                                label = "Breast Cancer PDX Subtype",
                                choices = Subtype,
                                selected = Subtype,
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = paste0("count > ", length(Subtype) - 1),
                                  `count-selected-text` = "All"
                                ),
                                multiple = TRUE
                              ),#pickerInput
                              pickerInput(
                                inputId = "lymphomagen",
                                label = "Lymphoma Genesis",
                                choices = c("Y", "N"),
                                selected = c("Y", "N"),
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count",
                                  `count-selected-text` = "All"
                                ),
                                multiple = TRUE
                              )#pickerInput
                            ),#div
                            div(actionButton("resetAll", "Reset"), style = "float:left")
                          ),#sidebarPanel
                          mainPanel(
                            useShinyjs(),
                            includeCSS("www/animate.min.css"),
                            
                            #js function to reset a button, variableName is the button name whose value we want to reset
                            tags$script(
                              "Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                              Shiny.onInputChange(variableName, null);
                              });
                              "),
                            div(style = 'overflow-x: scroll', DT::dataTableOutput("sampletable")),
                            div(id = "loading-content",
                                style = "height:500px;text-align:center;padding-top: 200px;",
                                class = "loading-content",
                                h2(class = "animated infinite pulse", "Loading data...")),
                            uiOutput("modal")
                          )#mainPanel
                        )#sidebarLayout
                      )#fluidPage
             ),#tabPanel-Samplelist
             tabPanel("Sample Search",
                      value = "samplesearch",
                      icon = icon("search"),
                      align = "center",
                      searchInput(
                        inputId = "sampleid",
                        label = "Enter your Primary ID ",
                        placeholder = "Enter Primary ID... ex) X_192",
                        btnSearch = icon("search"),
                        btnReset = icon("remove"),
                        width = "50%"
                      ),#searchInput
                      tabsetPanel(id = "SAMPLE",
                                  tabPanel(title = "Sample Information",
                                           div(class = "col-sm-12",
                                               h1(textOutput("pdxname"))),
                                           br(),
                                           br(),
                                           fluidRow(
                                             column(width = 12,
                                                    fluidRow(
                                                      tagList(singleton(tags$head(
                                                        tags$style(type = "text/css", "tfoot{display:none;}")
                                                      ))),
                                                      h3("Patient")
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Age")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(textOutput(
                                                                   "age"
                                                                 )))),
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Sex")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(textOutput(
                                                                   "sex"
                                                                 ))))
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Tumor type")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("tumortype")
                                                                 ))),
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Patient subtype")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("patientsubtype")
                                                                 )))
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Primary Site")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("primarysite")
                                                                 ))),
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Tumor site")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("tumorsite")
                                                                 )))
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Available data")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 9,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("patientseqdata")
                                                                 )))
                                                    )
                                                    )
                                           ),
                                           fluidRow(
                                             column(width = 12,
                                                    h3("Xenograft"),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Date")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(textOutput(
                                                                   "date"
                                                                 )))),
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Mouse ID")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 3,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("mouseid")
                                                                 )))
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("PDX pathology")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 9,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("pdxpathology")
                                                                 )))
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("PDX subtype")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 9,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("pdxsubtype")
                                                                 )))
                                                    ),
                                                    br(),
                                                    fluidRow(
                                                      column(width = 1),
                                                      column(
                                                        width = 2,
                                                        div(class = "col-sm-12",
                                                            h5("Available data")),
                                                        style = "background-color:#eeedf2;"
                                                      ),
                                                      column(width = 9,
                                                             div(class = "col-sm-12",
                                                                 h5(
                                                                   textOutput("xenoseqdata")
                                                                 )))
                                                    )
                                                    )
                                           )
                                  ), #tabPanel-sampleinfo
                                  tabPanel(title = "Mutation",
                                           fluidRow(tagList(singleton(tags$head(
                                             tags$style(type = "text/css", "tfoot{display:none;}")
                                           ))),
                                           div(style = 'overflow-x: scroll',
                                               br(),
                                               DT::dataTableOutput("mstable")))
                                           ),#tabPanel-samplemutation
                                  tabPanel(title = "CNV",
                                           br(),
                                           plotlyOutput('cnvsplot') %>% withSpinner(color="#0dc5c1")
                                  ),#tabPanel-samplemutation
                                  tabPanel(title = "Expression",
                                           br(),
                                           plotlyOutput('rnasplot') %>% withSpinner(color="#0dc5c1")
                                  )#tabPanel-samplemutation
                      )#tabsetPanel
             ),#tabPanel-samplesearch
             tabPanel("Gene Search",
                      value = "genesearch",
                      icon = icon("search"),
                      align = "center",
                      searchInput(
                        inputId = "geneid",
                        label = "Enter your Gene symbol ",
                        placeholder = "Enter Gene symbol... ex) BRAF",
                        btnSearch = icon("search"),
                        btnReset = icon("remove"),
                        width = "50%"
                      ),
                      tabsetPanel(
                        id = "GENE",
                        tabPanel(title = "Mutation",
                                 fluidRow(
                                   tagList(singleton(tags$head(
                                     tags$style(type = "text/css", "tfoot{display:none;}")
                                   ))),
                                   div(style = 'overflow-x: scroll',
                                       br(),
                                       DT::dataTableOutput("mgtable"))
                                 )),
                        #tabPanel-samplemutation
                        tabPanel(title = "CNV",
                                 br(),
                                 plotlyOutput('cnvgplot') %>% withSpinner(color="#0dc5c1")),
                        #tabPanel-samplemutation
                        tabPanel(title = "Expression",
                                 br(),
                                 plotlyOutput('rnagplot') %>% withSpinner(color="#0dc5c1"))#tabPanel-samplemutation
                      )
             )
  )#navbarPage
)