#library(readr)
#cnv_all <- read_tsv("data/cnv_all.tsv", na = character()) #118MB
#cnv_gene <- read_tsv("data/cnv_gene.tsv", na = character()) #540MB
#exp_gene <- read_tsv("data/exp_gene.tsv", na = character()) #253MB
#exp_gene_type <- read_tsv("data/exp_gene_type.tsv", na = character())
#expDF <- read_tsv("data/expDF.tsv", na = character())
#Mutlist <- read_tsv("data/Mutlist.tsv", na = character())#57MB
#Sinfo <- read_tsv("data/Sinfo.tsv", na = character())

library(shiny)
library(ggplot2)
library(plotly)
library(shinyBS)
library(DT)
library(data.table)
library(shinythemes)
library(shinyjs)

chrlens<-c(249250621,243199373,198022430,191154276,180915260,171115067,159138663,
           146364022,141213431,135534747,135006516,133851895,115169878,107349540,
           102531392,90354753,81195210,78077248,59128983,63025520,48129895,51304566,
           155270560,59373566)

function(input,output, session){
  
  show("loading-content")
  load("data/PDXDB_new.RData")
  hide("loading-content")
  
  shinyInput <- function(FUN, data, id, ...) {
    len <- nrow(data)
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label = data[i,1], ...))
    }
    inputs
  }
  
  presented_data <- reactive({
    cond_data_00 <- Sinfo[Sinfo$`Primary Site` != "Breast" | Sinfo$`Patient subtype` %in% input$patientsubtype,]
    cond_data_01 <- cond_data_00[cond_data_00$`Primary Site` != "Breast" | cond_data_00$`PDX subtype` %in% input$PDXsubtype,]
    cond_data_02 <- cond_data_01[cond_data_01$`Lymphoma genesis` %in% input$lymphomagen,]
    cond_data_1 <- cond_data_02[cond_data_02$`Primary Site` %in% input$primarysite,]
    cond_data_2 <- cond_data_1[cond_data_1$Sex %in% input$donorsex,]
    cond_data_3 <- cond_data_2[cond_data_2$`Tumor type` %in% input$tumortype,]
    if(input$donorage[[1]] == "0" & input$donorage[[2]] == "100"){
      cond_data_4 <- cond_data_3
      }
    else{
      cond_data_3 <- cond_data_3[cond_data_3$Age!="-",]
      cond_data_4 <- cond_data_3[as.numeric(cond_data_3$Age) >= input$donorage[[1]] & as.numeric(cond_data_3$Age) <= input$donorage[[2]],]
    }
    selected_data <- cond_data_4[,c("PDX ID", "Primary ID", "Primary Site", "Tumor type", "Tumor Site", "Patient subtype", "Age", "Sex", "Lymphoma genesis", "PDX status", "PDX subtype",  
                                    "Mouse ID", "Date", "Patient ID", "Patient tumor")]
    
    
    cbind(
      `PDX ID` = shinyInput(actionButton, 
                        selected_data,
                        'button_',
                        onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
      selected_data)
  })
  
  output$sampletable <- DT::renderDataTable(
    presented_data(), options = list(searching = TRUE, columnDefs = list(list(visible=FALSE, targets=c(1, 10, 14, 15)))),server = FALSE, escape = FALSE, selection = 'single', rownames=FALSE
  )
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
  observeEvent(input$resetAll2, {
    reset("form2")
  })
  
  observeEvent(input$select_button, {
    cond_data_00 <- Sinfo[Sinfo$`Primary Site` != "Breast" | Sinfo$`Patient subtype` %in% input$patientsubtype,]
    cond_data_01 <- cond_data_00[cond_data_00$`Primary Site` != "Breast" | cond_data_00$`PDX subtype` %in% input$PDXsubtype,]
    cond_data_02 <- cond_data_01[cond_data_01$`Lymphoma genesis` %in% input$lymphomagen,]
    cond_data_1 <- cond_data_02[cond_data_02$`Primary Site` %in% input$primarysite,]
    cond_data_2 <- cond_data_1[cond_data_1$Sex %in% input$donorsex,]
    cond_data_3 <- cond_data_2[cond_data_2$`Tumor type` %in% input$tumortype,]
    if(input$donorage[[1]] == "0" & input$donorage[[2]] == "100"){
      selected_data <- cond_data_3
    }
    else{
      cond_data_3 <- cond_data_3[cond_data_3$Age!="-",]
      selected_data <- cond_data_3[as.numeric(cond_data_3$Age) >= input$donorage[[1]] & as.numeric(cond_data_3$Age) <= input$donorage[[2]],]
    }
    s <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    output$ngsidtable <- DT::renderDataTable({datatable(rbind(
      cbind(selected_data[s,c("PDX ID", "Primary Site", "Patient tumor", "Primary ID")], `Data type`="WES"),
      setNames(cbind(selected_data[s,c("PDX ID", "Primary Site", "Patient tumor__1", "Xenograft tumor__1")], `Data type`="RNA"), c("PDX ID", "Primary Site", "Patient tumor", "Primary ID", "Data type"))
      ), options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, paging=FALSE), selection = list(mode = 'single', target = 'cell')
      )})
    
    showModal(modalDialog(
      title = selected_data$`PDX ID`[s],
      dataTableOutput("ngsidtable"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))

    modelnum <- s
    ##Reset the select_button
    session$sendCustomMessage(type = 'resetInputValue', message =  "select_button")
  })
  
  observeEvent(input$ngsidtable_cell_clicked, {
    info = input$ngsidtable_cell_clicked
    if (is.null(info$value) || (info$col != 4)) return()
    else {
      removeModal()
      updateTextInput(session, "sampleid", value = info$value)
      updateTabItems(session, "tabs", "samplesearch")

    }
    })
  
  observeEvent(input$sampleid_search, {
    if(!(toupper(input$sampleid) %in% Sinfo$`Primary ID`)){
      showModal(modalDialog(
        "No such Primary ID",
        size = "s",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      )
      )
    }
  })
  
  output$pdxname <- renderText({
    if(toupper(input$sampleid) != ""){
      if(!(toupper(input$sampleid) %in% Sinfo$`Primary ID`)){""}
      else{paste(Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"PDX ID"][[1]], " (", toupper(input$sampleid), ")")}
    }
    else{
      ""
    }
  })
  
  output$age <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Age"][[1]]
  })
  
  output$sex <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Sex"][[1]]
  })
  
  output$tumortype <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Tumor type"][[1]]
  })
  
  output$patientsubtype <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Patient subtype"][[1]]
  })
  
  output$primarysite <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Primary Site"][[1]]
  })
  
  output$tumorsite <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Tumor Site"][[1]]
  })
  
  output$patientseqdata <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Patient available"][[1]]
  })
  
  output$date <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Date"][[1]]
  })
  
  output$mouseid <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"Mouse ID"][[1]]
  })
  
  output$pdxpathology <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"PDX Pathology"][[1]]
  })
  
  output$pdxsubtype <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"PDX subtype"][[1]]
  })
  
  output$pdxid <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"PDX ID"][[1]]
  })
  
  output$xenoseqdata <- renderText({
    Sinfo[Sinfo$`Primary ID`==toupper(input$sampleid),"PDX available"][[1]]
  })

  output$mstable<-DT::renderDataTable({
    if(toupper(input$sampleid) != ""){
      Mutlist_dt <- Mutlist[Mutlist$Sample==toupper(input$sampleid),]
      datatable(data = Mutlist_dt[c("Primary Site", "Sample", "Gene", "Variant type", "AA change", "Total depth", "AF", "COSMIC", "TCGA", "OncoKB",  "Matched", "Chr", "Position", "Ref", "Alt")],
                                             rownames = FALSE,
                                             options = 
                                               list(
                                                 autoWidth = TRUE,
                                                 columnDefs = list(
                                                   list(
                                                   targets = c(13,14),
                                                   render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 6 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                                     "}")
                                                 ),
                                                 list(className = 'dt-center', targets = "_all"), 
                                                 list(visible=FALSE, targets=c(0, 1)))
                                               ), callback = JS('table.page(3).draw(false);'))
      
    }
    else{
      return()
    }
  })
  
  output$cnvsplot<-renderPlotly({
    cnv <- cnv_all[cnv_all$sample==toupper(input$sampleid),]
    if(nrow(cnv) == 0){
      p <- plotly_empty() %>% layout(title = "No data")
      p
    }
    else{
      vline <- function(x = 0, color = "gray") {
        list(
          type = "line", 
          y0 = -4, 
          y1 = 4, 
          yref = "y",
          x0 = x, 
          x1 = x, 
          line = list(color = color)
        )
      }
      chrlens<-c(249250621,243199373,198022430,191154276,180915260,171115067,159138663,
                 146364022,141213431,135534747,135006516,133851895,115169878,107349540,
                 102531392,90354753,81195210,78077248,59128983,63025520,48129895,51304566,
                 155270560,59373566)
      chromosomes <-paste("chr", c(1:22,"X","Y"), sep = '')
      offsets<-c(0,cumsum(as.numeric(chrlens)))
      names(offsets)=c(0,seq(1,24))
      
      
      vertical_lines <- list(vline(offsets["1"]), vline(offsets["2"]), vline(offsets["3"]), vline(offsets["4"]),
                             vline(offsets["5"]), vline(offsets["6"]), vline(offsets["7"]), vline(offsets["8"]),
                             vline(offsets["9"]), vline(offsets["10"]), vline(offsets["11"]), vline(offsets["12"]),
                             vline(offsets["13"]), vline(offsets["14"]), vline(offsets["15"]), vline(offsets["16"]),
                             vline(offsets["17"]), vline(offsets["18"]), vline(offsets["19"]), vline(offsets["20"]),
                             vline(offsets["21"]), vline(offsets["22"]), vline(offsets["23"]), vline(offsets["24"]) 
      )
      
      chr2coordinates <- function(chr,coord){
        if(chr == "X"){
          chr = "23"
        }
        else if(chr == "Y"){
          chr = "24"
        }
        converted <- offsets[as.character(as.numeric(chr) - 1)] + as.numeric(coord)
        converted
      }
      
      
      for (i in 1:nrow(cnv)) {
        chr <- cnv[i,"chr"][[1]]
        cnv[i,"start"] <- as.numeric(chr2coordinates(chr,cnv[i,"start"]))
        cnv[i,"end"] <- as.numeric(chr2coordinates(chr,cnv[i,"end"]))
        cnv[i,"seg.mean"][[1]] <- as.numeric(cnv[i,"seg.mean"][[1]])
      }
      cnv$mid_pos <- (cnv$start + cnv$end) / 2
      cnv$size <- cnv$end - cnv$start
      
      p <- plot_ly(data = cnv, x = ~mid_pos, y = ~seg.mean, 
                   hoverinfo = 'text', text = ~paste('</br>Chromosome:', chr,
                                                     '</br>Position:', start, '-', end,
                                                     '</br>Segment size: ', size,' bp',
                                                     '</br>Gene: ', gene,
                                                     '</br>',seg.mean
                   ), color = ~ampdel, colors = c("amp" = "blue", "del" = "red"),
                   type = "scatter", mode = "markers") %>%
        layout(title = "Segment mean of Z-score of log2(FPKM)", yaxis = list(range = c(-4, 4)), xaxis = list(title = "position", range = c(min(offsets), max(offsets)), tickvals = offsets[2:length(offsets)], ticktext=chromosomes), shapes = vertical_lines)

      p
    }
  })
  
  output$rnasplot<-renderPlotly({
    xeno <- toupper(input$sampleid)
    if(!(xeno %in% expDF$Sample)){
      p <- plotly_empty() %>% layout(title = "No data")
      p
    }
    else{
      
      p <- plot_ly(width = 1000, height = 800) %>%
        add_trace(
          x = expDF$PC1, 
          y = expDF$PC2, 
          z = expDF$PC4, 
          color = expDF$cluster, 
          hovertext = ~paste(expDF$Sample,expDF$cluster),
          hoverinfo = "text",
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 3, line = list(color='black',width = 1))
        ) %>% 
        layout(
          showlegend = TRUE,
          scene = list(
            xaxis = list(title = 'PC1', showticklabels=FALSE),
            yaxis = list(title = 'PC2', showticklabels=FALSE),
            zaxis = list(title = 'PC4', showticklabels=FALSE),
            aspectratio = list(
              x = 1,
              y = 1,
              z = 1
            ),
            camera = list(
              center = list(
                x = 0,
                y = 0,
                z = 0
              ),
              eye = list(
                x = 1.3,
                y = 1.3,
                z = 1.3
              ),
              up = list(
                x = 0,
                y = 0,
                z = 1
              )
            ),
            annotations = list(list(
              x = expDF[input$sampleid,"PC1"][[1]],
              y = expDF[input$sampleid,"PC2"][[1]],
              z = expDF[input$sampleid,"PC4"][[1]],
              xref = "x",
              yref = "y",
              zref = "z",
              text = input$sampleid,
              textangle = 0,
              ax = 200,
              ay = 30,
              font = list(
                color = "red",
                size = 30
              ),
              arrowcolor = "red",
              arrowsize = 1,
              arrowwidth = 3,
              arrowhead = 1
            )
            ))
        )
      p
    }
  })
    
  output$mgtable<-DT::renderDataTable({
    if(toupper(input$geneid) != ""){
      Mutlist_dt <- Mutlist[Mutlist$Gene==toupper(input$geneid),]
      datatable(data = Mutlist_dt[c("Primary Site", "Sample", "Gene", "Variant type", "AA change", "Total depth", "AF", "COSMIC", "TCGA", "OncoKB",  "Matched", "Chr", "Position", "Ref", "Alt")],
                                             rownames = FALSE,
                                             options = 
                                               list(
                                                 autoWidth = TRUE,
                                                 columnDefs = list(list(
                                                   targets = c(13,14),
                                                   render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 6 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                                                     "}")
                                                 ),
                                                 list(className = 'dt-center', targets = "_all"))
                                               ), callback = JS('table.page(3).draw(false);'))
    }
    else{
      return()
    }
  })
  
  output$cnvgplot<-renderPlotly({

    cnvg <- cnv_gene[cnv_gene$gene==toupper(input$geneid),]
    if(nrow(cnvg) == 0){p <- plotly_empty() %>% layout(title = "No data")}
    else{
      p <- plot_ly(cnvg, y = ~seg.mean, color = ~type, type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8,
                   hoverinfo = 'text', text = ~paste('</br>Sample: ', sample,
                                                     '</br>',seg.mean
                   ))
      
      p
    }
  })

  output$rnagplot<-renderPlotly({
    
    expg <- exp_gene[exp_gene$NAME==toupper(input$geneid),]
    if(nrow(expg) == 0){p <- plotly_empty() %>% layout(title = "No data")}
    else{
      t_expg <- transpose(expg)
      colnames(t_expg) <- rownames(expg)
      rownames(t_expg) <- colnames(expg)
      plot_expg <- data.frame(matrix(ncol = 1, nrow = ncol(expg) - 1))
      plot_expg[,1] <- rownames(t_expg)[2:nrow(t_expg)]
      colnames(plot_expg) <- c("name")
      plot_expg$FPKM <- as.numeric(t_expg[2:nrow(t_expg),1])
      plot_expg$type <- exp_gene_type$X__2
      
      p <- plot_ly(plot_expg, y = ~FPKM, color = ~type, type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8,
                   hoverinfo = 'text', text = ~paste('</br>Sample: ', name,
                                                     '</br>',FPKM
                   )) %>%
        layout(yaxis = list(showticklabels =FALSE))
      
      
      p
    }
  })

}