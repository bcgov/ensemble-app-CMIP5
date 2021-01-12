library(shiny)
library(RColorBrewer)
library(DT)

# setwd("C:\\Colin\\Projects\\2020_ScenarioGuidance\\EnsembleApp")

## Load the input data

ecoprov.hist <- read.csv("data/ecoprov.ref.100pts.mean.csv")
ecoprov.proj <- read.csv("data/ecoprov.proj.100pts.mean.csv")
ecoprov.6190 <- read.csv("data/ecoprov.6190.100pts.mean.csv")
ecoprov.0119 <- read.csv("data/ecoprov.0119.100pts.mean.csv")
ecoprov.1119 <- read.csv("data/ecoprov.1119.100pts.mean.csv")
ecoprov.1719 <- read.csv("data/ecoprov.1719.100pts.mean.csv")
ecoprov.ts <- read.csv("data/ecoprov.ts.100pts.mean.csv")
modelMetadata <- read.csv("data/ModelList.csv")
# str(ecoprov.climate)

ecoprovs <- levels(ecoprov.proj$id1)
ecoprov.names <- c("Boreal Plains", "Central Interior", "Coast and Mountains", "Georgia Depression", "Northern Boreal Mountains", "Southern Alaska", "Sub-Boreal Interior", "Southern Interior Mountains", "Southern Interior", "Taiga Plains")
variables <- names(ecoprov.proj)[-c(1:6)]
variable.names <- read.csv("data/Variables_ClimateBC.csv")
variables.select <- variables[c(grep("_wt|_sp|_sm|_at", variables), 225:247)]
variables.select <- variables.select[-grep("RH|Rad|MAR", variables.select)]

variable.types <- rep(NA, length(variables))
variable.types[grep("PPT|DD|PAS|NFFD|Eref|FFP|CMD|MAP|MSP|AHM|SHM|Rad|MAR", variables)] <- "ratio"
variable.types[grep("Tmax|Tmin|Tave|MAT|MWMT|MCMT|TD|EMT|EXT|bFFP|eFFP", variables)] <- "interval"
variable.types[grep("RH", variables)] <- "pct"

Ystr <- strsplit(as.character(ecoprov.proj[,1]), "_")
scenario <- matrix(unlist(Ystr), ncol=3, byrow=TRUE)
scenario[,3] <- substr(scenario[,3],1,4)
scenario[grep("Ensemble", scenario[,1]),1] <- "Ensemble-mean"

rcps <- sort(unique(scenario[,2]))
proj.years <- unique(scenario[,3])

## calculate change in each climate variable

ecoprov.change <- ecoprov.proj[-c(1:6)]
ecoprov.change[,] <- NA

for(ecoprov in ecoprovs){
  s <- which(ecoprov.proj$id1==ecoprov)
  j <- which(ecoprovs==ecoprov)
  for(variable in variables){
    i <- which(variables==variable)
    ecoprov.change[s,i] <- if(variable.types[i]%in%c("interval", "pct")) ecoprov.proj[s,i+6] - ecoprov.hist[j,i+5] else ecoprov.proj[s,i+6]/ecoprov.hist[j,i+5]
  }
  print(ecoprov)
}

ecoprov.change.0119 <- ecoprov.0119[-c(1:6)]
ecoprov.change.0119[,] <- NA
ecoprov.change.1119 <- ecoprov.change.0119
ecoprov.change.1719 <- ecoprov.change.0119

for(ecoprov in ecoprovs){
  s <- which(ecoprov.6190$id1==ecoprov)
  for(variable in variables){
    i <- which(variables==variable)
    ecoprov.change.0119[s,i] <- if(variable.types[i]%in%c("interval", "pct")) ecoprov.0119[s,i+6] - ecoprov.6190[s,i+6] else ecoprov.0119[s,i+6]/ecoprov.6190[s,i+6]
    ecoprov.change.1119[s,i] <- if(variable.types[i]%in%c("interval", "pct")) ecoprov.1119[s,i+6] - ecoprov.6190[s,i+6] else ecoprov.1119[s,i+6]/ecoprov.6190[s,i+6]
    ecoprov.change.1719[s,i] <- if(variable.types[i]%in%c("interval", "pct")) ecoprov.1719[s,i+6] - ecoprov.6190[s,i+6] else ecoprov.1719[s,i+6]/ecoprov.6190[s,i+6]
  }
  print(ecoprov)
}


variable="MAP"
# runExample("01_hello")

# Define UI ----
ui <- fluidPage(
  navbarPage(title = "Climate model selection", theme = "bcgov.css", 
             tabPanel("2 Variables", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Use this app to select global climate models for your region, scenario, and variables of interest. All data are from ClimateBC. See the 'Model Info' tab for model names"),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          checkboxInput("ensembleOnly", label = "Exclude single-run models", value = FALSE),
                          
                          radioButtons("proj.year",
                                       label = "Choose a future period",
                                       choices = list("2011-2040" = 1, "2041-2070" = 2, "2071-2100" = 3),
                                       selected = 1),
                          
                          radioButtons("rcp",
                                       label = "Choose an emissions scenario",
                                       choices = list("RCP2.6" = 1, "RCP4.5" = 2, "RCP8.5" = 3),
                                       selected = 2),
                          
                          selectInput("var1", 
                                      label = "Choose the x-axis variable",
                                      choices = as.list(variables.select),
                                      selected = "Tave_sm"),
                          
                          selectInput("var2", 
                                      label = "Choose the y-axis variable",
                                      choices = as.list(variables.select),
                                      selected = "PPT_sm"),
                          
                          selectInput("ecoprov.name",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[9]),
                          
                          img(src = "Ecoprovinces_Title.png", height = 1861*1/5, width = 1993*1/5)
                        ),    
                        
                        mainPanel(
                          
                          plotOutput(outputId = "scatterPlot")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             tabPanel("1 Variable",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Climate projections of the 15-model CMIP5 ensemble. The red time series is the observed climate of the selected ecoprovince. All data are from ClimateBC."),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          checkboxInput("ensembleOnly.II", label = "Exclude single-run models", value = FALSE),
                          
                          radioButtons("proj.year.II",
                                      label = "Choose a future period",
                                      choices = list("2011-2040" = 1, "2041-2070" = 2, "2071-2100" = 3),
                                      selected = 1),
                          
                          radioButtons("rcp.II",
                                      label = "Choose an emissions scenario",
                                      choices = list("RCP2.6" = 1, "RCP4.5" = 2, "RCP8.5" = 3),
                                      selected = 2),
                          
                          selectInput("var2.II", 
                                      label = "Choose the y-axis variable",
                                      choices = as.list(variables.select),
                                      selected = "Tmin_at"),
                          
                          selectInput("ecoprov.name.II",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[9]),
                          
                          img(src = "Ecoprovinces_Title.png", height = 1861*1/5, width = 1993*1/5)
                        ),    
                        
                        mainPanel(
                          
                          plotOutput(outputId = "timeSeries")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             tabPanel("Model Info",
                      DT::dataTableOutput("table"),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
                      )
             )
)

# Define server logic ----
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    proj.year <-  proj.years[as.numeric(input$proj.year)]
    rcp <- rcps[as.numeric(input$rcp)]
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name)]
    var1 <- input$var1
    var2 <- input$var2
    variable.type1 <- variable.types[which(variables==var1)]
    variable.type2 <- variable.types[which(variables==var2)]
    
    s <- which(ecoprov.proj$id1==ecoprov & scenario[,2]== rcp & scenario[,3]== proj.year )
    x <- ecoprov.change[s, which(variables==var1)]
    y <- ecoprov.change[s, which(variables==var2)]
    
    # xlim=if(variable.type1=="ratio") range(x) else if(min(x)<0) range(x) else c(0, max(x))
    # ylim=if(variable.type2=="ratio") range(y) else if(min(y)<0) range(y) else c(0, max(y))
    xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
    ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)
    
    par(mar=c(3,4,0,1), mgp=c(1.25, 0.25,0), cex=1.5)
    plot(x,y,col="white", tck=0, xaxt="n", yaxt="n", xlim=xlim, ylim=ylim, ylab="",
         xlab=paste("Change in", variable.names$Variable[which(variable.names$Code==var1)]), 
    )
    par(mgp=c(2.5,0.25, 0))
    title(ylab=paste("Change in", variable.names$Variable[which(variable.names$Code==var2)]))
    lines(if(variable.type1=="ratio") c(1,1) else c(0,0), c(-99,99), lty=2, col="gray")
    lines(c(-99,99), if(variable.type2=="ratio") c(1,1) else c(0,0), lty=2, col="gray")

    s <- which(ecoprov.0119$id1==ecoprov)
    x1 <- ecoprov.change.0119[s, which(variables==var1)]
    y1 <- ecoprov.change.0119[s, which(variables==var2)]
    points(x1,y1, pch=16, col="gray", cex=5)
    text(x1,y1, "2001-2019", cex=1.5, font=2, pos=4, col="gray", offset=1.1)  
 
    # s <- which(ecoprov.1119$id1==ecoprov)
    # x1 <- ecoprov.change.1119[s, which(variables==var1)]
    # y1 <- ecoprov.change.1119[s, which(variables==var2)]
    # points(x1,y1, pch=1, col="red", cex=2)
    # text(x1,y1, "2011-19", col="red", font=2, pos=4)  # for(rcp in rcps[2:3]){
    
    s <- which(ecoprov.proj$id1==ecoprov & scenario[,2]== rcp & scenario[,3]== proj.year )
    x <- ecoprov.change[s, which(variables==var1)]
    y <- ecoprov.change[s, which(variables==var2)]
    models <- unique(scenario[which(scenario[,2]==rcp),1])
    mods <- c("ENS", "ACC", "CAN", "CCSM", "CESM", "CNRM", "CSIR", "GFDL", "GISS", "HAD", "INM", "IPSL", "MIRE", "MIR5", "MPI", "MRI")[which(unique(scenario[,1])%in%models)]
    exclude.singleRun <- which(mods%in%modelMetadata$Code[which(modelMetadata$Runs==1)])
    
    colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
    set.seed(2)
    ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(unique(scenario[,1]))-12))
    ColScheme[11] <- "blue"
    ColScheme <- ColScheme[which(unique(scenario[,1])%in%models)]
    
    for(model in if(input$ensembleOnly==T) models[-exclude.singleRun] else models){
      i=which(models==model)
      points(x[i],y[i], pch=21, bg=ColScheme[i], cex=if(model==models[1]) 5.5 else 4)
      text(x[i],y[i], mods[i], cex=if(model==models[1]) 1 else 0.7, font=2)
    }
    
    if(variable.type1=="ratio"){
      axis(1, at=seq(-99,99,0.1), labels=paste(round(seq(-99,99,0.1)*100-100), "%", sep=""), tck=0)
    } else axis(1, at=seq(round(range(x)[1]-1), round(range(x)[2]+1), round(diff(range(x))/7, if(diff(range(x))<3.5) 1 else 0)), labels=seq(round(range(x)[1]-1),round(range(x)[2]+1), round(diff(range(x))/7, if(diff(range(x))<3.5) 1 else 0)), tck=0)
    if(variable.type2=="ratio"){
      axis(2, at=seq(-99,99,0.1), labels=paste(round(seq(-99,99,0.1)*100-100), "%", sep=""), las=2, tck=0)
    } else axis(2, at=seq(round(range(y)[1]-1), round(range(y)[2]+1), round(diff(range(y))/7, if(diff(range(y))<3.5) 1 else 0)), labels=seq(round(range(y)[1]-1),round(range(y)[2]+1), round(diff(range(y))/7, if(diff(range(y))<3.5) 1 else 0)), las=2, tck=0)
    
    # legend("bottomleft", legend = c("2001-2019", "2011-2019"), title = "Observed change", pch=c(16, 1), pt.cex=2, col="red", bty="n")
    
    },
  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
  output$timeSeries <- renderPlot({
    
    proj.year <-  proj.years[as.numeric(input$proj.year.II)]
    rcp <- rcps[as.numeric(input$rcp.II)]
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name.II)]
    var2 <- input$var2.II
    variable.type2 <- variable.types[which(variables==var2)]
    
    s <- which(ecoprov.proj$id1==ecoprov & scenario[,2]== rcp & scenario[,3]== proj.year )
    x <- ecoprov.proj[s, which(names(ecoprov.proj)==var2)]
    x.ref <- ecoprov.hist[ecoprov.hist$id1==ecoprov, which(names(ecoprov.hist)==var2)]
    ts <- ecoprov.ts[which(ecoprov.ts$Year>1949 & ecoprov.ts$id1==ecoprov), which(names(ecoprov.ts)==var2)]
    ts.ref <- ecoprov.6190[which(ecoprov.6190$id1==ecoprov), which(names(ecoprov.6190)==var2)]
    
    models <- unique(scenario[which(scenario[,2]==rcp),1])
    mods <- c("ENS", "ACC", "CAN", "CCSM", "CESM", "CNRM", "CSIR", "GFDL", "GISS", "HAD", "INM", "IPSL", "MIRE", "MIR5", "MPI", "MRI")[which(unique(scenario[,1])%in%models)]
    colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
    set.seed(2)
    ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(unique(scenario[,1]))-12))
    ColScheme[11] <- "blue"
    ColScheme <- ColScheme[which(unique(scenario[,1])%in%models)]
    
        if(variable.type2=="ratio"){
      ts <- ts*x.ref/ts.ref
    } else ts <- ts+x.ref-ts.ref

    r <- range(c(x, ts))

    par(mar=c(2,4,0.1,0.1), mgp=c(2,0.25,0))
    plot(1950:2105, rep(NA, length(1950:2105)), ylim=r, xaxt="n", xaxs="i", xlab="", tck=0, las=2,
         ylab=paste(variable.names$Variable[which(variable.names$Code==var2)]))

    proj.year.start <- 1981+30*which(proj.years==proj.year)
    proj.year.end <- 2010+30*which(proj.years==proj.year)

    if(input$ensembleOnly.II==T){
      exclude <- which(models%in%modelMetadata$Model[which(modelMetadata$Runs==1)])
      x <- x[-exclude]
      models <- models[-exclude]
      mods <- mods[-exclude]
      ColScheme <- ColScheme[-exclude]
    }
    
    rect(proj.year.start, min(x), proj.year.end, max(x), col="gray", border=F)
    lines(c(0,9999), rep(x.ref,2), col="grey", lwd=2, lty=2)
    lines(c(1961,1990), rep(x.ref,2), col=1, lwd=4)
    lines(1950:2019, ts, col="red", lwd=2)

    axis(1, at=seq(1960, 2100, 20), labels=seq(1960, 2100, 20), tck=0)

    
    models.sort <- models[order(x)]
    x.sort <- x[order(x)]
    for(model in models.sort){
      i <- which(models.sort==model)
      lines(c(proj.year.start, proj.year.end), rep(x.sort[i],2), col=ColScheme[which(models==model)], lwd=2)
      position <- if(proj.year==proj.years[2]) rep(0:2, times=100) else rep(0:4, times=100)
      side <- if(proj.year==proj.years[1]) rep(4,200) else {if(proj.year==proj.years[2]) rep(c(2,4), times=100) else rep(2,200)}
      lines(if(side[i]==4) c(proj.year.end, proj.year.end+position[i]*11) else c(proj.year.start, proj.year.start-position[i]*11) , rep(x.sort[i],2), col=ColScheme[which(models==model)], lty=2)
      # points(if(side[i]==4) {proj.year.end+position[i]*8} else {proj.year.start-position[i]*8}, x.sort[i], pch=21, bg=ColScheme[which(models==model)], cex=if(model==models[1]) 5.5 else 4)
      text(if(side[i]==4) proj.year.end+position[i]*11 else proj.year.start-position[i]*11, x.sort[i], mods[which(models==model)], col=ColScheme[which(models==model)], pos=side[i], font=2, cex=0.9, offset=0.1)
         }
    
  },
  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata, 
                  options = list(pageLength = dim(modelMetadata)[1]), 
                  rownames= FALSE, 
                  caption = 'Model Metadata. Global model statistics are quoted from Forster et al. (2013, Journal of Geophysical Research): TCR is transient climate response (temperature change in response to a 1%/yr increase in CO2 at time, at point of doubling CO2), ECS is equilibrium climate sensitivity (temperature change in response to an instant doubling of CO2), and deltaT is global mean surface temperature change since preindustrial for RCP4.5 in the 2090s. All values are in degrees Celsius.'
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)




# proj.year <- 2055
# rcp <- "rcp45"
# ecoprov <- "SOI"
# var1 <- "MAT"
# var2 <- "MAP"
