# nametagger
# functions

library(Cairo)
library(curl)
library(ggplot2)
library(png)
library(shiny)
library(shinyBS)
library(shinythemes)
library(showtext)

if(!"gfont" %in% sysfonts::font_families()) font_add_google("Lato","gfont")
showtext_opts(dpi=300)

# fn_version
fn_version <- function() {
  return("v1.1.3")
}

# validation
fn_validate <- function(input,message1,message2,message3)
{

  if(missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing",message1))
  if(gcheck == 1)
  {
    m1 <- sub("Argument ","",message1)
    m1 <- sub(" missing.","",m1)
  }

  if (all(is.null(input))) {
    if(missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if(all(is.na(input)))
    {
      if(missing(message2))
      {
        if(gcheck==1) message2 <- paste0("Argument ",m1," is NA.",sep="")
        if(gcheck!=1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if(all(nchar(input) == 0))
    {
      if(missing(message3))
      {
        if(gcheck==1) message3 <- paste0("Argument ",m1," is empty.",sep="")
        if(gcheck!=1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

# nametag_plot_page ------------------------------------------------------------

#' @title nametag_plot_page
#' @description Creates a page with 1-8 nametags.
#' @param dfr A data.frame. See details.
#' @param logo_right A raster logo to be placed on the right.
#' @param logo_right_scale A scale value. Typically 0.1-0.4.
#' @param logo_right_offset Logo offset from the edge. Around 0.01-0.1.
#' @param logo_left A raster logo to be placed on the left.
#' @param logo_left_scale A scale value. Typically 0.1-0.4.
#' @param logo_left_offset Logo offset from the edge. Around 0.01-0.1.
#' @param col_border Colour of clipping border. Defaults to "grey75".
#' @param family A character denoting font family.
#' @param ret Logical indicating if the ggplot object must be returned.
#' @param height Height of nametag in cm. Defaults to 5.5.
#' @param width Width of nametag in cm. Defaults to 9.
#' @details Argument 'dfr' is a data.frame that must have columns
#' label1, label1_x, label1_y, page, row and col.
#' @export
#'
nametag_plot_page <- function(dfr,logo_right=NULL,logo_right_scale,logo_right_offset,logo_left=NULL,logo_left_scale,logo_left_offset,col_border="grey75",family="",height=5.5,width=9)
{
  if(missing(dfr)) stop("Input argument 'dfr' missing.")

  # check columns
  req_cols <- c("label1","label1_sz","label1_x","label1_y","page","row","col")
  chk_cols <- req_cols %in% colnames(dfr)
  if(any(!chk_cols)) stop(paste0("Input data is missing columns: ",paste0(req_cols[!chk_cols],collapse=","),"."))

  # add labels if any has length >0
  p <- ggplot(dfr)
  if(any(nchar(dfr$label1)!=0)) p <- p + geom_text(aes(x=label1_x,y=label1_y,label=label1),size=dfr$label1_sz[1],fontface="bold",family=family)

  if("label2" %in% colnames(dfr)) {
    req_cols <- c("label2_sz","label2_x","label2_y")
    chk_cols <- req_cols %in% colnames(dfr)
    if(any(!chk_cols)) stop(paste0("Input data contains column 'label2' but missing columns: ",paste0(req_cols[!chk_cols],collapse=","),"."))

    if(any(nchar(dfr$label2)!=0)) p <- p + geom_text(aes(x=label2_x,y=label2_y,label=label2),size=dfr$label2_sz[1],family=family)
  }

  if("label3" %in% colnames(dfr)) {
    req_cols <- c("label3_sz","label3_x","label3_y")
    chk_cols <- req_cols %in% colnames(dfr)
    if(any(!chk_cols)) stop(paste0("Input data contains column 'label2' but missing columns: ",paste0(req_cols[!chk_cols],collapse=","),"."))

    if(any(nchar(dfr$label3)!=0)) p <- p + geom_text(aes(x=label3_x,y=label3_y,label=label3),size=dfr$label3_sz[1],family=family)
  }

  rm(req_cols,chk_cols)

  p <- p + scale_x_continuous(limits=c(0,1),expand=c(0,0))+
    scale_y_continuous(limits=c(0,1),expand=c(0,0))+
    labs(x=NULL,y=NULL)

  w_scaler <- width/height

  # check and add right logo
  if(!is.null(logo_right)) {

    # height scaling multiplier 1.6
    logo_right_height <- ((logo_right_scale*nrow(logo_right))/ncol(logo_right))*w_scaler

    # create logo positions
    logo_right_x2 <- 1-logo_right_offset
    logo_right_x1 <- logo_right_x2-logo_right_scale
    logo_right_y2 <- 1-(logo_right_offset+(logo_right_offset*w_scaler))
    logo_right_y1 <- round(logo_right_y2-logo_right_height,3)

    # add right logo to plot
    p <- p + annotation_raster(logo_right,xmin=logo_right_x1,xmax=logo_right_x2,ymin=logo_right_y1,ymax=logo_right_y2)
  }


  # check and add left logo
  if(!is.null(logo_left)) {
    logo_left_height <- ((logo_left_scale*nrow(logo_left))/ncol(logo_left))*w_scaler

    # create logo positions
    logo_left_x1 <- logo_left_offset
    logo_left_x2 <- logo_left_x1+logo_left_scale
    logo_left_y2 <- 1-(logo_left_offset+(logo_left_offset*w_scaler))
    logo_left_y1 <- round(logo_left_y2-logo_left_height,3)

    # add left logo to plot
    p <- p + annotation_raster(logo_left,xmin=logo_left_x1,xmax=logo_left_x2,ymin=logo_left_y1,ymax=logo_left_y2)
  }

  p <- p+
    facet_grid(row~col)+
    #theme_minimal()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0,"lines"),
          strip.background=element_blank(),
          strip.text=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_rect(colour=col_border,fill="white",size=0.4,linetype="25"),
          plot.background=element_blank(),
          plot.margin=margin(0.1,0.1,0.1,0.1),
          axis.ticks.length=unit(0,"pt"))

  return(p)
}

# nametag ----------------------------------------------------------------------

#' @title nametag
#' @description Creates an A4 paper with 1-8 name tags.
#' @param dfr A data.frame with column 'label1'. Optionally 'label2' and 'label3'.
#' @param label1_sz Size of label on line 1.
#' @param label1_x X-axis coordinate for the label on line 1.
#' @param label1_y Y-axis coordinate for the label on line 1.
#' @param label2_sz Size of label on line 2.
#' @param label2_x X-axis coordinate for the label on line 2.
#' @param label2_y Y-axis coordinate for the label on line 2.
#' @param label3_sz Size of label on line 3.
#' @param label3_x X-axis coordinate for the label on line 3.
#' @param label3_y Y-axis coordinate for the label on line 3.
#' @param logo_right A raster logo for the right side.
#' @param logo_right_offset Logo offset from the edge. Around 0.01-0.1.
#' @param logo_right_scale A scale value. Typically 0.1-0.4.
#' @param logo_left A raster logo for the right side.
#' @param logo_left_offset Logo offset from the edge. Around 0.01-0.1.
#' @param logo_left_scale A scale value. Typically 0.1-0.4.
#' @param family A character denoting font family.
#' @param export Logical indicating if the plot must be exported.
#' @param filename A character denoting filename (prefix) of exported files.Defaults to 'nametag_' followed by page number and '.png'.
#' @param path A character path to the directory where file(s) are to be exported. Do not add / at the end of the path.
#' @param ftype Export file type. 'png' or 'pdf'. Defaults to 'png'.
#' @param height Height of nametag in cm. Defaults to 5.5.
#' @param width Width of nametag in cm. Defaults to 9.
#' @param verbose If progress messages should be printed to console.
#' @details A data.frame with one column 'label1' is the only mandatory input for this function.
#' @export
#'
nametag <- function(dfr,label1_sz=8,label1_x=0.5,label1_y=0.54,
                    label2_sz=6.5,label2_x=0.5,label2_y=0.37,
                    label3_sz=6,label3_x=0.5,label3_y=0.22,
                    logo_right=NULL,logo_right_offset=0.04,logo_right_scale=0.2,
                    logo_left=NULL,logo_left_offset=0.04,logo_left_scale=0.2,
                    family="",export=TRUE,filename="nametag_",path=".",ftype="png",height=5.5,width=9,verbose=FALSE)
{
  if(missing(dfr)) stop("Input argument 'dfr' is missing.")
  if(!is.data.frame(dfr)) stop("Input argument 'dfr' must be a data.frame.")
  if(nrow(dfr)<1) stop("Input data must have at least 1 row.")
  if(!("label1"  %in% colnames(dfr))) stop("Input data must contain a column named 'label1'.")
  if("label3"  %in% colnames(dfr)) {if(!("label2"  %in% colnames(dfr))) stop("Column 'label3' is present, but 'label2' is missing. If 'label3' is used, 'label2' must be present." )}
  if(is.null(filename)) filename <- "nametag_"
  
  # compute tags and pages
  n <- nrow(dfr)
  npages <- ceiling(n/8)
  ntags <- npages*8
  filler <- ntags-n
  l1 <- c(dfr$label1,rep("",filler))
  if("label2"  %in% colnames(dfr)) l2 <- c(dfr$label2,rep("",filler))
  if("label3"  %in% colnames(dfr)) l3 <- c(dfr$label3,rep("",filler))
  nn <- length(l1)

  # create working df
  dfw <- data.frame(label1=l1,label1_sz=label1_sz,
                    label1_x=rep(label1_x,nn),label1_y=rep(label1_y,nn),
                    stringsAsFactors=F)

  if("label2"  %in% colnames(dfr)) {
    dfw$label2 <- l2
    dfw$label2_sz <- label2_sz
    dfw$label2_x <- rep(label2_x,nn)
    dfw$label2_y <- rep(label2_y,nn)
  }

  if("label3"  %in% colnames(dfr)) {
    dfw$label3 <- l3
    dfw$label3_sz <- label3_sz
    dfw$label3_x <- rep(label3_x,nn)
    dfw$label3_y <- rep(label3_y,nn)
  }

  dfw$page <- rep(1:npages,each=8)
  dfw$row <- rep(rep(1:4,each=2),npages)
  dfw$col <- rep(rep(c(1,2),4),npages)

  dflist <- split(dfw,dfw$page)
  ids <- names(dflist)

  # creates plots and saves ggplot objects to a list
  plist <- lapply(dflist,nametag_plot_page,logo_right,logo_right_scale,logo_right_offset,logo_left,logo_left_scale,logo_left_offset,family=family,height=height,width=width)
  #sapply(dflist,plot_page,logo,logo_scale,logo_offset,simplify=F,USE.NAMES=TRUE)
  #mapply(plot_page,dflist,ids,logo,logo_scale,logo_offset)
  
  # function to export images
  efun <- function(p,id,height,width,filename,ftype,verbose) {
    if(export) {
      
      if(ftype=="pdf") {
        fname <- file.path(path,paste0(filename,id,".pdf"))
        grDevices::cairo_pdf(file=fname,height=height*4*0.3937,width=width*2*0.3937)
        showtext::showtext_begin()
        print(p)
        showtext::showtext_end()
        dev.off()
        if(verbose) cat(paste0("\n",fname," exported\n."))
      }
      
      if(ftype=="png") {
        fname <- file.path(path,paste0(filename,id,".png"))
        grDevices::png(file=fname,width=width*2,height=height*4,units="cm",res=300,type="cairo")
        showtext::showtext_begin()
        print(p)
        showtext::showtext_end()
        dev.off()
        if(verbose) cat(paste0("\n",fname," exported\n."))
      }
      
    }
  }
  
  # exports images
  mapply(function(p,id,height,width,filename,ftype,verbose) efun(p,id,height,width,filename,ftype,verbose),plist,ids,height,width,filename,ftype,verbose)
}

# get vector of logos
logos <- c("none",list.files("www/logos",full.names = T))
names(logos) <- gsub(".png$|.jpg$","",basename(logos))
