## nametagger
## R shinyapp to generate nametags
## 2020 Roy Mathew Francis

source("functions.R")

# UI ---------------------------------------------------------------------------

ui <- fluidPage(theme=shinytheme("flatly"),
tags$head(tags$link(rel="stylesheet",type="text/css",href="styles.css")),
  fixedRow(
      column(12,style="margin:15px;",
             fluidRow(style="margin-bottom:10px;",
                      span(tags$img(src='nbis-lime.png',style="height:18px;"),style="vertical-align:top;display:inline-block;"),
                      span(tags$h4("•",style="margin:0px;margin-left:6px;margin-right:6px;"),style="vertical-align:top;display:inline-block;"),
                      span(tags$h4(strong("Nametagger"),style="margin:0px;"),style="vertical-align:middle;display:inline-block;")
             ),
    fixedRow(
    column(3,style="max-width:300px;background:#ebedef;padding-top:15px;padding-bottom:15px;border-radius:4px;",
      fluidRow(
        column(6,style=list("padding-right:5px;"),
               selectInput("in_input","Input method",choices=c("Upload file","Paste text"),
                           selected="Paste text",multiple=FALSE)),
        column(6,style=list("padding-left: 5px;"),
               selectInput("in_data_format","File format",choices=c("tsv","csv","csv2"),selected="csv",multiple=FALSE))
        ),
      uiOutput("ui_input"),
      fluidRow(
        column(12,
               style="padding-top:5px;padding-bottom:10px",
                      HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Input must contain column names. label1 is a mandatory column. Optional columns are label2 and label3.</div>')
               )
        ),
      fluidRow(
        column(6,style=list("padding-right: 5px;"),
               selectInput("in_logo_left",label="Logo left",choices=c("None","NBIS Lime","NBIS Aqua","SciLifeLab Lime","SciLifeLab Aqua","Elixir"),selected="None",multiple=FALSE)
        ),
        column(6,style=list("padding-left: 5px;"),
               selectInput("in_logo_right",label="Logo right",c("None","NBIS Lime","NBIS Aqua","SciLifeLab Lime","SciLifeLab Aqua","Elixir"),selected="None",multiple=FALSE)
        )
      ),
      #selectInput("in_family",label="Font family",choices=c("Default",sysfonts::font_families_google()),selected="Lato",multiple=FALSE,selectize=T),
      # fluidRow(
      #   column(12,
      #          style="padding-top:5px;padding-bottom:10px",
      #                 icon("info-circle",class="fa-lg"),
      #                 helpText("Check https://fonts.google.com/ for google fonts.",style="display:inline;")
      #          )
      #   ),
      checkboxInput("in_settings","Settings",value=FALSE),
      div(style="margin-top:25px;margin-bottom:20px;",downloadButton("btn_download","Download")),
      div(style="font-size:0.8em;",paste0(format(Sys.time(),'%Y'),' • Roy Francis • Version: ',fn_version()))
    ),
    column(6,style="max-width:450px;min-width:400px;padding-top:15px;padding-bottom:15px;border-radius:4px;",
      sliderInput("in_scale","Image preview scale",min=0.1,max=1.5,step=0.10,value=0.6,width="100%"),
      fluidRow(
        column(12,style="padding-top:5px;padding-bottom:10px",
               HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Scale controls preview below and does not affect download. Only page 1 is displayed below.</div>')
        )
      ),
      textOutput("out_pagecount"),
      tags$br(),
      div(class="img-output",
          imageOutput("out_plot",width="auto",height="auto")
      )
      #verbatimTextOutput("out_display")
    ),
    uiOutput("ui_settings")
    )
  )
)
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## get temporary directory
  store <- reactiveValues(epath=tempdir())
  
  ## UI: ui_input --------------------------------------------------------------
  ## ui to select input type: upload or paste
  output$ui_input <- renderUI({
    validate(fn_validate(input$in_input))

    if(input$in_input=="Upload file") {
      fileInput("in_data","Upload a text file.",multiple=FALSE)
    }else{
      shinyAce::aceEditor("in_data","label1,label2\nJohn Doe,Uppsala University\nMary Jane,Stockholm University",mode="text",theme="textmate",readOnly=FALSE,height="150px",fontSize=12)
    }
  })

  ## UI: ui_settings -----------------------------------------------------------
  ## ui to display settings
  output$ui_settings <- renderUI({
    validate(fn_validate(input$in_settings))

    if(input$in_settings) {
      column(3,style="max-width:300px;border-radius:4px;background:#ebedef;",
        h3("Settings"),
        div(
          tags$b("Label 1"),
          fluidRow(
            column(4,style=list("padding-right: 3px;"),
                   numericInput("in_label1_size",label="Size",value=8,min=0,max=20,step=0.5),
                   shinyBS::bsTooltip(id="in_label1_size",title="Size of label 1. A value between 0 and 20.0.",placement="top",trigger="hover")
            ),
            column(4,style=list("padding-right: 3px; padding-left: 3px;"),
                   numericInput("in_label1_x",label="Hor pos",value=0.5,min=0,max=1,step=0.02),
                   shinyBS::bsTooltip(id="in_label1_x",title="Horizontal position of label 1. A value between 0.00 and 1.00 denoting left to right.",placement="center",trigger="hover")
            ),
            column(4,style=list("padding-left: 3px;"),
                   numericInput("in_label1_y",label="Ver pos",value=0.54,min=0,max=1,step=0.02),
                   shinyBS::bsTooltip(id="in_label1_y",title="Vertical position of label 1. A value between 0.00 and 1.00 denoting bottom to top.",placement="right",trigger="hover")
            )
          ),
          tags$b("Label 2"),
          fluidRow(
            column(4,style=list("padding-right: 3px;"),
                   numericInput("in_label2_size",label="Size",value=6.5,min=0,max=20,step=0.5),
                   shinyBS::bsTooltip(id="in_label2_size",title="Size of label 2. A value between 0 and 20.0.",placement="top",trigger="hover")
            ),
            column(4,style=list("padding-right: 3px; padding-left: 3px;"),
                   numericInput("in_label2_x",label="Hor pos",value=0.5,min=0,max=1,step=0.02),
                   shinyBS::bsTooltip(id="in_label2_x",title="Horizontal position of label 2. A value between 0.00 and 1.00 denoting left to right.",placement="center",trigger="hover")
            ),
            column(4,style=list("padding-left: 3px;"),
                   numericInput("in_label2_y",label="Ver pos",value=0.37,min=0,max=1,step=0.02),
                   shinyBS::bsTooltip(id="in_label2_y",title="Vertical position of label 2. A value between 0.00 and 1.00 denoting bottom to top.",placement="right",trigger="hover")
            )
          ),
          tags$b("Label 3"),
          fluidRow(
            column(4,style=list("padding-right: 3px;"),
                   numericInput("in_label3_size",label="Size",value=6,min=0,max=20,step=0.5),
                   shinyBS::bsTooltip(id="in_label3_size",title="Size of label 3. A value between 0 and 20.0.",placement="top",trigger="hover")
            ),
            column(4,style=list("padding-right: 3px; padding-left: 3px;"),
                   numericInput("in_label3_x",label="Hor pos",value=0.5,min=0,max=1,step=0.02),
                   shinyBS::bsTooltip(id="in_label3_x",title="Horizontal position of label 3. A value between 0.00 and 1.00 denoting left to right.",placement="center",trigger="hover")
            ),
            column(4,style=list("padding-left: 3px;"),
                   numericInput("in_label3_y",label="Ver pos",value=0.22,min=0,max=1,step=0.02),
                   shinyBS::bsTooltip(id="in_label3_y",title="Vertical position of label 3. A value between 0.00 and 1.00 denoting bottom to top.",placement="right",trigger="hover")
            )
          ),
          tags$b("Logo left"),
          fluidRow(
            column(6,style=list("padding-right: 3px;"),
                   numericInput("in_logo_left_offset",label="Offset",value=0.02,min=0,max=0.35,step=0.01),
                   shinyBS::bsTooltip(id="in_logo_left_offset",title="Distance of left logo from left edge and top edge. A value between 0.0 and 0.35.",placement="left",trigger="hover")
            ),
            column(6,style=list("padding-left: 3px;"),
                   numericInput("in_logo_left_scale",label="Size",value=0.2,min=0.1,max=0.6,step=0.01),
                   shinyBS::bsTooltip(id="in_logo_left_scale",title="Size of left logo. A value between 0.1 and 1.0.",placement="right",trigger="hover")
            )
          ),
          tags$b("Logo right"),
          fluidRow(style="margin-bottom:10px;",
            column(6,style=list("padding-right: 3px;"),
                   numericInput("in_logo_right_offset",label="Offset",value=0.02,min=0,max=0.35,step=0.01),
                   shinyBS::bsTooltip(id="in_logo_right_offset",title="Distance of right logo from right edge and top edge. A value between 0.0 and 0.35.",placement="left",trigger="hover")
            ),
            column(6,style=list("padding-left: 3px;"),
                   numericInput("in_logo_right_scale",label="Size",value=0.2,min=0.1,max=0.6,step=0.01),
                   shinyBS::bsTooltip(id="in_logo_right_scale",title="Size of right logo. A value between 0.1 and 1.0.",placement="right",trigger="hover")
            )
          )
        )
      )
    }

  })

  ## FN: fn_input --------------------------------------------------------
  ## function to get input data

  fn_input <- reactive({
    validate(fn_validate(input$in_input))
    validate(fn_validate(try(input$in_data),message1="Upload a file or paste text."))
    validate(fn_validate(input$in_data_format))
    fr <- ifelse(input$in_data_format=="tsv","\t",ifelse(input$in_data_format=="csv",",",";"))

    if(input$in_input=="Upload file") {
      dfr <- read.delim(input$in_data$datapath,header=TRUE,sep=fr,stringsAsFactors=F)
    }
    if(input$in_input=="Paste text") {
      df1 <- as.data.frame(strsplit(as.character(unlist(strsplit(input$in_data,"\n"))),fr))
      cnames <- as.character(df1[,1])
      df1[,1] <- NULL
      dfr <- as.data.frame(t(df1),stringsAsFactors=F)
      colnames(dfr) <- cnames
      rownames(dfr) <- 1:nrow(dfr)
    }

    colnames(dfr) <- tolower(colnames(dfr))
    return(dfr)
  })

  ## FN: fn_params ------------------------------------------------------------
  ## function to get plot params

  fn_params <- reactive({

    validate(fn_validate(fn_input()))
    validate(fn_validate(input$in_settings))
    validate(fn_validate(input$in_logo_left))
    validate(fn_validate(input$in_logo_right))
    #req(input$in_family)
    
    if("label1" %in% colnames(fn_input())) {l1 <- TRUE}else{l1 <- FALSE}
    if("label2" %in% colnames(fn_input())) {l2 <- TRUE}else{l2 <- FALSE}
    if("label3" %in% colnames(fn_input())) {l3 <- TRUE}else{l3 <- FALSE}
    
    # if values are available, use them, else use defaults
    if(is.null(input$in_label1_size)){l1s <- 8}else{l1s <- input$in_label1_size}
    if(is.null(input$in_label1_x)){l1x <- 0.5}else{l1x <- input$in_label1_x}
    if(is.null(input$in_label1_y)){l1y <- 0.54}else{l1y <- input$in_label1_y}
    if(is.null(input$in_label2_size)){l2s <- 6.5}else{l2s <- input$in_label2_size}
    if(is.null(input$in_label2_x)){l2x <- 0.5}else{l2x <- input$in_label2_x}
    if(is.null(input$in_label2_y)){l2y <- 0.37}else{l2y <- input$in_label2_y}
    if(is.null(input$in_label3_size)){l3s <- 6}else{l3s <- input$in_label3_size}
    if(is.null(input$in_label3_x)){l3x <- 0.5}else{l3x <- input$in_label3_x}
    if(is.null(input$in_label3_y)){l3y <- 0.22}else{l3y <- input$in_label3_y}
    if(is.null(input$in_logo_left_offset)){llo <- 0.04}else{llo <- input$in_logo_left_offset}
    if(is.null(input$in_logo_right_offset)){lro <- 0.04}else{lro <- input$in_logo_right_offset}
    if(is.null(input$in_logo_left_scale)){lls <- 0.2}else{lls <- input$in_logo_left_scale}
    if(is.null(input$in_logo_right_scale)){lrs <- 0.2}else{lrs <- input$in_logo_right_scale}

    # validation ---------------------------------------------------------------
    fn_val <- function(x) {
      dp <- deparse(substitute(x))
      dp <- switch(dp,"l1x"="Label 1 Hor pos","l2x"="Label 2 Hor pos",
                   "l3x"="Label 3 Hor pos","l1y"="Label 1 Ver pos",
                   "l2y"="Label 2 Ver pos","l3y"="Label 3 Ver pos",
                   "lls"="Logo left scale","lrs"="Logo right scale")
      if(x<0 | x>1) paste0("Input '",dp,"' must be between 0 and 1.")
    }
    validate(fn_val(l1x))
    validate(fn_val(l2x))
    validate(fn_val(l3x))
    validate(fn_val(l1y))
    validate(fn_val(l2y))
    validate(fn_val(l3y))
    validate(fn_val(lls))
    validate(fn_val(lrs))
    
    fn_val_offset <- function(x) {
      dp <- deparse(substitute(x))
      dp <- switch(dp,"llo"="Logo left Offset","lro"="Logo right Offset")
      if(x<0 | x>0.35) paste0("Input '",dp,"' must be between 0 and 0.35.")
    }
    validate(fn_val_offset(llo))
    validate(fn_val_offset(lro))
    
    fn_val_size <- function(x) {
      dp <- deparse(substitute(x))
      dp <- switch(dp,"l1s"="Label 1 Size","l2s"="Label 2 Size","l3s"="Label 3 Size")
      if(x<0 | x>20) paste0("Input '",dp,"' must be between 0.0 and 20.0.")
    }
    validate(fn_val_size(l1s))
    validate(fn_val_size(l2s))
    validate(fn_val_size(l3s))
    
    # logos --------------------------------------------------------------------
    lr = switch(
      input$in_logo_right,
      "None"=NULL,
      "NBIS Lime"="./www/nbis-lime.png",
      "NBIS Aqua"="./www/nbis-aqua.png",
      "SciLifeLab Lime"="./www/scilifelab-lime.png",
      "SciLifeLab Aqua"="./www/scilifelab-aqua.png",
      "Elixir"="./www/elixir_200.png"
    )
    if(!is.null(lr)) {lri <- readPNG(lr)}else{lri <- NULL}

    ll = switch(
      input$in_logo_left,
      "None"=NULL,
      "NBIS Lime"="./www/nbis-lime.png",
      "NBIS Aqua"="./www/nbis-aqua.png",
      "SciLifeLab Lime"="./www/scilifelab-lime.png",
      "SciLifeLab Aqua"="./www/scilifelab-aqua.png",
      "Elixir"="./www/elixir_200.png"
    )
    if(!is.null(ll)) {lli <- readPNG(ll)}else{lli <- NULL}

    # custom label y positions, offset and scaling for logos
    if(!input$in_settings) {
      if(is.null(lr) & is.null(ll)) {
        if(l1) {l1y <- 0.52}
        if(l1 & l2) {l1y <- 0.62; l2y <- 0.42}
        if(l1 & l2 & l3) {l1y <- 0.68; l2y <- 0.48; l3y <- 0.3}
      }
      
      if(!is.null(lr)) {
        if(grepl("elixir",lr)) {lro <- 0.02; lrs <- 0.2}
        if(grepl("scilifelab",lr)) {lro <- 0.035; lrs <- 0.32}
        if(grepl("nbis",lr)) {lro <- 0.035; lrs <- 0.2}
      }

      if(!is.null(ll)) {
        if(grepl("elixir",ll)) {llo <- 0.02; lls <- 0.2}
        if(grepl("scilifelab",ll)) {llo <- 0.035; lls <- 0.32}
        if(grepl("nbis",ll)) {llo <- 0.035; lls <- 0.2}
      }
    }

    return(list(l1s=l1s,l1x=l1x,l1y=l1y,l2s=l2s,l2x=l2x,l2y=l2y,l3s=l3s,l3x=l3x,l3y=l3y,
                llo=llo,lls=lls,lro=lro,lrs=lrs,lri=lri,lli=lli))
  })

  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure

  output$out_plot <- renderImage({
    
    validate(fn_validate(fn_input()))
    validate(fn_validate(fn_params()))
    
    progress1 <- shiny::Progress$new()
    progress1$set(message="Capturing settings...", value=8)
    
    p <- fn_params()
    
    progress1$set(message="Generating figure...", value=30)
    nametag(dfr=fn_input(),label1_sz=p$l1s,label1_x=p$l1x,label1_y=p$l1y,
            label2_sz=p$l2s,label2_x=p$l2x,label2_y=p$l2y,
            label3_sz=p$l3s,label3_x=p$l3x,label3_y=p$l3y,
            logo_right=p$lri,logo_right_offset=p$lro,logo_right_scale=p$lrs,
            logo_left=p$lli,logo_left_offset=p$llo,logo_left_scale=p$lls,
            family="gfont",path=store$epath,ftype="png")
    fname <- file.path(store$epath,"nametag_1.png")
    
    progress1$set(message="Completed.", value=100)
    progress1$close()
    
    return(list(src=fname,contentType="image/png",
                width=round(9*2*37.7*input$in_scale,0),
                height=round(5.5*4*37.7*input$in_scale,0),
                alt="nametagger_image"))
  },deleteFile=TRUE)

  ## OUT: out_pagecount -------------------------------------------------------
  ## prints general variables for debugging
  
  output$out_pagecount <- renderText({
    
    req(fn_input())

    npages <- ceiling(nrow(fn_input())/8)
    paste0("Showing 1 of ",npages," pages.")
  })
  
  ## OUT: out_display -------------------------------------------------------
  ## prints general variables for debugging

  output$out_display <- renderPrint({
    cat(paste0(
      "Input type: ",input$in_input,"\n",
      "Settings: ",input$in_settings,"\n",
      "Label 1 size: ",input$in_label1_size,"\n",
      "Label 2 size: ",input$in_label2_size,"\n",
      "Label 3 size: ",input$in_label3_size,"\n",
      "Label 1 x: ",input$in_label1_x,"\n",
      "Label 2 x: ",input$in_label2_x,"\n",
      "Label 3 x: ",input$in_label3_x,"\n",
      "Label 1 y: ",input$in_label1_y,"\n",
      "Label 2 y: ",input$in_label2_y,"\n",
      "Label 3 y: ",input$in_label3_y,"\n",
      "Logo left: ",input$in_logo_left,"\n",
      "Logo left offset: ",input$in_logo_left_offset,"\n",
      "Logo left scale: ",input$in_logo_left_scale,"\n",
      "Logo right: ",input$in_logo_right,"\n",
      "Logo right offset: ",input$in_logo_right_offset,"\n",
      "Logo right scale: ",input$in_logo_right_scale,"\n"
    ))
  })
  
  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images

  fn_download <- function(){

    validate(fn_validate(fn_input()))
    validate(fn_validate(fn_params()))

    p <- fn_params()
    nametag(dfr=fn_input(),label1_sz=p$l1s,label1_x=p$l1x,label1_y=p$l1y,
            label2_sz=p$l2s,label2_x=p$l2x,label2_y=p$l2y,
            label3_sz=p$l3s,label3_x=p$l3x,label3_y=p$l3y,
            logo_right=p$lri,logo_right_offset=p$lro,logo_right_scale=p$lrs,
            logo_left=p$lli,logo_left_offset=p$llo,logo_left_scale=p$lls,
            path=store$epath,family="gfont",ftype="pdf")

    epathn <- file.path(store$epath,"nametagger.zip")
    if(exists(epathn)) unlink(epathn)
    
    zip(epathn,files=list.files(path=store$epath,pattern="pdf",full.names=TRUE))
    unlink(list.files(path=store$epath,pattern="pdf",full.names=TRUE))
  }

  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file

  output$btn_download <- downloadHandler(
    filename="nametagger.zip",
    content=function(file) {
      
      progress <- shiny::Progress$new()
      progress$set(message="Generating PDFs...", value=52)
      fn_download()
      
      progress$set(message="Downloading zipped file...", value=90)
      file.copy(file.path(store$epath,"nametagger.zip"),file,overwrite=T)
      
      progress$set(message="Completed.",value=100)
      progress$close()
    }
  )
}

shinyApp(ui=ui, server=server)
