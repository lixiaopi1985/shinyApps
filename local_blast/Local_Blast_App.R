library(shiny)
library(Biostrings)
library(DT)
library(XML)
library(plyr)
library(stringr)

options(repos = BiocInstaller::biocinstallRepos())
getOption("repos")



# checkFasta <- function(fa){
#   fasta = readLines(fa)
#   if(!startsWith(fasta[1], '>') & !setequal(unique(unlist(str_split(toupper(fasta[2]), ""))), c("A", "T", "C", "G", "N"))){
#     return('Not a Valid Fasta File')
#   } else {
#     return(NULL)
#   }
# }

ui <- fluidPage(
  
  # tag 
  tagList(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "style.css"),
      tags$script(type = "text/javascript",
                  src = "busy.js")
    )),
  

  # mainPanel (((((((((((((((((((((((((
  div(
    class="main",
    mainPanel(
      tags$img(src='banner.png', height = 90, width=800),
      #tags$p("Contains Oregon Plant Species trnL database and Starky Plant Species ITS2 database"),
      br(),br(),
    
      textAreaInput(
      inputId = 'seq',
      label = 'Input FASTA Format Sequence here:',
      value = "",
      placeholder = paste("fasta format. e.g.                                                                                                                >Query                                                                                                                                                     AATTCCGGTT"),
      width = "600px",
      height = "200px"),
      
      fileInput(
        inputId = 'upload',
        label = 'Upload Query file',
        accept = "fasta",
        multiple = FALSE,
        placeholder = "format: .fa .fasta"
        
      ),
    
    # INLINE program, database and evalue
    div(
      class = "program",
      selectInput(
        inputId = 'program', 
        label = 'Program',
        choices = c('blastn', 'tblastn'), 
        width = "80px",
        selected = 'blastn',
        selectize = FALSE
      )),
      
    div(
      class = "DB",
      selectInput(inputId = "db",
                  label = "Database:",
                  choices = c("Oregon Local Plant trnL", "Starky Plant ITS2", "Phytophthora ITS"),
                  selected = "",
                  selectize = FALSE,
                  width = "250px"
                  )
      ),
      
    div(
      class = "eval",
      selectInput(
        inputId = "eval", 
        label = "e-value:", 
        #  E = m x n  / 2bit-score
        #m - query sequence length
        #n - total database length (sum of all sequences)
        choices = c(1, 0.001, 1e-4, 1e-5, 1e-10), 
        width = "80px", 
        selected = 1e-4, 
        selectize = FALSE)
    ),
    
    div(
      radioButtons(
        inputId = 'file_type',
        label = 'Save result type:',
        choices = c('csv', 'table'),
        selected = 'csv'), class = 'radiobutton'
    ),
    
    textInput(
      inputId = 'saveName',
      label = 'Save file Name As:',
      value = '',
      placeholder = 'blastoutput'
    ),
    
    div(
      actionButton(inputId = "blastButton",
                   label = "Blast!",
                   class = 'butt'),
      downloadButton(outputId = 'download',
                     label = 'Download Results',
                     class = 'butt'),
      downloadButton(outputId = 'downloadDB',
                     label = 'Download Database',
                     class = 'butt'),
      class = "Btn"
      )
    )
  ),
    
#))))))))))))))))))))))))))))))))))))))))))))))))))))

      
    
      #style = 'display:inline-block',
      # selectInput(
      #   inputId = 'outfmt',
      #   label = 'Output Format:',
      #   choices = c("xml", "tabular"),
      #   selected = "xml",
      #   selectize = FALSE,
      #   width = "80px"
      # ),

    

      

  
  # this snippet generates a 'busy' indicator for long Blasts
  div(class = 'busy',
      p('calculation in progress..'),
      img(src='LoadingBasketContents.gif'), height = 50, width = 50, align = 'center'),
  
  br(),br(),
  ############ results panels ###########################
  div(
    mainPanel(
      hr(class="h-line"),
      h4('Blast Results:'),
      DT::dataTableOutput("Blast_Results"),
      width = 6,
      br(),br(),
    class = "result_block")),
      
    div(
      mainPanel(        
        h4("Alignment:"), 
        tableOutput('clicked'),
        verbatimTextOutput('alignment'), 
        width = 6)
      ),
      
    br(),br(),br(),
    div(
      class = 'footer',
      HTML("Author: Xiaoping Li&emsp;lixiaopi@oregonstate.edu&emsp;2018-6-11")
      )
)

#|
#|///////////////////////// Server //////////////////////////
#|           
server <- function(input, output, session){
  

  
  # databaseInput <- reactive({
  #   switch (input$db,
  #           "Oregon Local Plant trnL" = trnl,
  #           "Starky Plant ITS2" = starky,
  #           "Phytophthora ITS" = phyto
  #   )
  # })
  

  
  
  # button blastButton
  blastresults <- eventReactive(
    
    # acitvate the blast button
    req(input$blastButton), {

     # if else chooses the right data base
     # here you could add more database
     if(input$db == "Oregon Local Plant trnL"){
        db = c("database/blast_db/OR_trnl_DB/OR_plant_trnl")
        remote = c("")
     } else if(input$db == "Starky Plant ITS2"){
        db = c("database/blast_db/Starky_ITS2_DB/Starky_plant_ITS2")
        remote = c("")
      } else if(input$db == "Phytopthora ITS"){
        db = c("database/blast_db/pytophora/phyto_ITS")
        remote = c("")
      } else {
        db = c("nr")
        remote = c("-remote")
      }
      
      


      # gather input and set up temp file
      tmp = tempfile(fileext = ".fa")
  
      
      if(nchar(input$seq) == 0){
        
          file1 = input$upload
          if(is.null(file1)){
            return(NULL)
          }
          
          line = readLines(file1$datapath)
          
          if(startsWith(line[1], ">")){
            writeLines(line, tmp)
          } else {
            writeLines(paste0(">Query\n", line), tmp)
          }
        

        
      } else if(nchar(input$seq > 0)){
      
        query = input$seq
        

        if(startsWith(query, ">")){
          writeLines(query, tmp)
        } else {
          writeLines(paste0(">Query\n", query), tmp)
        }
      }
      
      
      # calls the blast
      # set permission error anwser here:
      # https://groups.google.com/forum/#!topic/shinyapps-users/sHSm3Of_3Og
      # fmt = reactive({
      #   if(input$outfmt == 'xml'){
      #     return('5')
      #   } else if(input$outfmt == 'tabular'){
      #     return('6')
      #   }
      #   })
      
      
      system("chmod -R 777 ncbi-blast-2.7.1+/bin/")
      # check fasta input

      datas = system(paste0("ncbi-blast-2.7.1+/bin/", input$program, " -query ", tmp, " -db ", db, " -dust no -evalue ", input$eval, " -outfmt 5", " -max_hsps 1 -max_target_seqs 10 ", remote), intern = T)

      
      xmlParse(datas)
      # if(input$outfmt == 'xml'){ 
      #   
      # } else if(input$outfmt == 'tabular'){ 
      #   datas
      # }
 
}, ignoreNULL = T)

  
  
  
  # parse results to parseResults as xml
  parseResults = reactive({
    
    if(is.null(blastresults())){
        cat("No results")
    } else {
      
            #if(input$outfmt == 'xml'){
            #xmltop = xmlRoot(blastresults())

            #extract elements from xml results
            query_ID = getNodeSet(blastresults(), '//Hit//Hit_def') %>% sapply(., xmlValue)
            hit_IDs = getNodeSet(blastresults(), '//Hit//Hit_id') %>% sapply(., xmlValue)
            hit_length = getNodeSet(blastresults(), '//Hit//Hit_len') %>% sapply(., xmlValue)
            bitscore = getNodeSet(blastresults(), '//Hit//Hit_hsps//Hsp_bit-score') %>% sapply(., xmlValue)
            eval = getNodeSet(blastresults(), '//Hit//Hit_hsps//Hsp//Hsp_evalue') %>% sapply(., xmlValue)
            accessions = gsub(" .*$", "", query_ID)

            results = data.frame(accessions, query_ID, hit_IDs, hit_length, eval, bitscore, stringsAsFactors = F)
          
            #results <-  rbind.fill(lapply(results,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
            names(results) = c('accession', 'qseqid', 'sseqid', 'length', 'eval', 'bitscore')
          
            results # data frame
            
            #} else if(input$outfmt == 'tabular'){
        
             #   row_list = lapply(blastresults(), function(x){
              #    unlist(str_split(x, "\t"))
               #   })
                
                #df = data.frame(t(sapply(row_list, c)), stringsAsFactors = F)
                
                #colnames(df) = c('qseqid', 'sseqid', 'pident','length', 'mismatch', 'gapopen', 'qstart', 'qend', 'sstart', 'send', 'eval', 'bitscore')
                
                #df
            #}        
    }
  })

  # output results to data table
  output$Blast_Results = renderDataTable({
    if(is.null(parseResults())){
      cat('No Results')
    } else {
      parseResults()
    }
  })



  # show results
  # place holder for alignments
  #   
  output$clicked <- renderTable({
    
    reset <- reactiveValues(sel = "") # select single rows
    
    if(is.null(input$Blast_Results_rows_selected)){
    } else {
      clicks = input$Blast_Results_rows_selected
      
      # make alignment
      Hsp_qseq = getNodeSet(blastresults(), '//Hit//Hit_hsps//Hsp//Hsp_qseq') %>% sapply(., xmlValue)
      Hsp_midline = getNodeSet(blastresults(), '//Hit//Hit_hsps//Hsp//Hsp_midline') %>% sapply(., xmlValue)
      Hsp_hseq = getNodeSet(blastresults(), '//Hit//Hit_hsps//Hsp//Hsp_hseq') %>% sapply(., xmlValue)
      align = rbind(Hsp_qseq, Hsp_midline, Hsp_hseq)
      
      # wrap the alignment, 60 characters \\1 selects by first group
      splits <- strsplit(gsub("(.{60})", "\\1,", align[1:3,clicks]),",")
      # paste them together
      

      splits_out <- lapply(1:length(splits[[1]]), function(i){
         
        
        top <- splits[[1]][i]
        midline <-  splits[[2]][i]
        bottom <- splits[[3]][i]
        midline <- gsub("(\\|)", "\\1 ", gsub(" ", "      ", midline))
        
        rbind(top, midline, bottom)
       })
       
       as.data.frame(unlist(splits_out))
    }
  }, server = TRUE) # TRUE if data is big
  
    ################ download output #############
    
    # download results
  output$download <- downloadHandler(
      
      filename = function(){
        paste(input$saveName, input$file_type, sep = ".")
},
    
      content = function(file){
      
        if(input$file_type == 'csv'){
          write.table(parseResults(), file, sep = ',', row.names = F, quote = F)
        } else if (input$file_type == 'table'){
          write.table(parseResults(), file, row.names = F, quote = F)
        }
      } 
  )

  


    output$downloadDB <- downloadHandler(
      filename = function(){
        paste0(str_replace_all(input$db, ' ', '_'), '.fasta')
      },
      
      content = function(file){
        if(input$db == 'Oregon Local Plant trnL'){
          file.copy("database/blast_db/OR_trnl_DB/trnL_OR_plantsp.fasta", file)
        } else if (input$db == "Starky Plant ITS2"){
          file.copy("database/blast_db/Starky_ITS2_DB/ITS2_SparkySpecies.fasta", file)
        } else if (input$db == "Phytophthora ITS"){
          file.copy("database/blast_db/pytophora/Phytopthora-ID-ITS.fasta", file)
          }
      }
      
    )
}

shinyApp(ui, server)