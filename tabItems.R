
tab_upload <- 
          
          fluidPage(
            fileInput(inputId = "file1", 
                      label = HTML("<center>Upload your Excel (.xlsx) file:</center>"),
                      accept = c(".xlsx"), multiple = F
                      ),
            
            
            )

tab_scatter1 <- 
  tabItem(
    {
    # tag style used to control the padding of the boxes
    # tags$head(tags$style(
    #   type = "text/css",
    #   '
    #                              .box { margin-bottom: 1; }
    #                              [class*="col-lg-"],[class*="col-md-"],
    #                              [class*="col-sm-"],[class*="col-xs-"]
    #                              {padding-right:1 !important;
    #                              padding-left:1 !important;}
    #                              '
    # )),
    },
        tabName = "scatter1",
    
    fluidRow(
      uiOutput("var_ui"),
      # box(plotOutput("contents"), width =  9)
      conditionalPanel(
        # Uses a Javascript formatted condition
        # e.g.: condition="input.yvalue2 !== ' ' | input.goterm !== ' '",
        condition = "input.facet_var_row !== ' '",
        # condition="input.genes_or_GO !== ' '",
        uiOutput("placeholder1"),
        htmlOutput("URLS_scatter1")
        # box(
        #   
        #   HTML("<b>NEW: </b><i>Genemania combination and Genecard info for selected genes</i>"),
        #   solidHeader = T, status = "primary",
        #   htmlOutput("URLS_scatter1"), width = 3
        # ),
      ),
      )
    )


tab_scatter2 <- 
  tabItem(
    {
    # tag style used to control the padding of the boxes
    # tags$head(tags$style(
    #   type = "text/css",
    #   '
    #                              .box { margin-bottom: 1; }
    #                              [class*="col-lg-"],[class*="col-md-"],
    #                              [class*="col-sm-"],[class*="col-xs-"]
    #                              {padding-right:1 !important;
    #                              padding-left:1 !important;}
    #                              '
    # )),
    },
        tabName = "scatter2",
    
    fluidRow(
      # https://groups.google.com/g/shiny-discuss/c/FyMGa2R_Mgs # remove all error messages
      # tags$style(type="text/css",
      #            ".shiny-output-error { visibility: hidden; }",
      #            ".shiny-output-error:before { visibility: hidden; }"
      # ),
      # uiOutput("var_ui2"), # NOT NEEDED ANYMORE
      # box(plotOutput("contents"), width =  9)
      conditionalPanel(
        # Uses a Javascript formatted condition
        # e.g.: condition="input.yvalue2 !== ' ' | input.goterm !== ' '",
        condition = "input.x_var_sel !== '' ",
        # condition = "input.facet_var_row_sel !== ' ' | input.facet_var_col_sel !== ' ' | 
        # input.x_var_sel !== ' ' | input.x_var_fil !== ' '",
        # condition="input.genes_or_GO !== ' '",
        
        
        
        
        box(title = "Parameters", width = 3, 
            selectInput("x_var_sel", "Select x variable", choices = character(0)),
            selectInput("x_var_fil", NULL, 
                        choices = character(0), selected = character(0), multiple = T),
            selectInput('color_var_sel', 'Select color variable', 
                        choices = character(0), selected = character(0)),
            selectInput("facet_var_col_sel", "Select facet column variable", 
                        choices = character(0), selected = character(0)),
            selectInput("facet_var_col_fil", NULL, 
                        choices = character(0), selected = character(0), multiple = T),
            selectInput("facet_var_row_sel", "Select facet row variable", 
                        choices = character(0), selected = "No facet row"),
            selectInput("facet_var_row_fil", NULL, 
                        choices = character(0), selected = character(0), multiple = T),
            
            
        ),
        
        
        
        uiOutput("placeholder2"),
        # tags$div(class = 'rightAlign',
          # style = "float:right;width=100%",
        # column(align = "left", width = 12, offset = 0,
        # box(
        #   width = 3,
        #   
        #     HTML("<b>NEW: </b><i>Genemania combination and Genecard info for selected genes</i>"),
        #   solidHeader = T, status = "primary",
        #   htmlOutput("URLS_scatter2"))
        htmlOutput("URLS_scatter2"),
        box(width = 7, div(actionButton("actbutt", label = "Click here", 
                                    ), align = "center"),
            div(HTML("<br>Click the button to get gene ontology (GO) terms of the genes.<br>Please only click one time and wait until a table appears below.<br>It might take a while to load and the app is unresponsive during this process."), 
                align = "center")),
        box(selectInput("GO_scatter2_select", label = 'Search genes displayed in graph by GO term after clicking above button - delete the entries if you again want to use only the above filters for genes in "Parameters"', choices = NULL, multiple = T), width = 12),
        box(title = "Search table by gene or GO term",
            dataTableOutput("GO_scatter2"), width = 12),
        # textOutput("testingthings")
        # dataTableOutput("testingthings")
        
      )
      ),
      # )
    )