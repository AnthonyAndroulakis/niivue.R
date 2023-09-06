# Using NiiVue in R Shiny
[NiiVue](https://github.com/niivue/niivue) is an open-source JavaScript library for WebGL2-based medical image viewing. This R package allows you to use NiiVue from R Shiny.

# Examples
### Tractography (TCK, TRK, TRX, VTK)
Demo for [NiiVue's Tractography](https://niivue.github.io/niivue/features/tracts.html)
![](images/example1.png)
[[code]](examples/tracts.R)

### Time Series Demo
Partial demo for [NiiVue's Time Series](https://niivue.github.io/niivue/features/timeseries.html) (toggle thumbnail doesn't work, but the other features do)
![](images/example2.png)
[[code]](examples/timeseries.R)

# Quickstart
1. Install the devtools library
```
install.packages('devtools')
```
2. Download this repo
```
git clone https://github.com/AnthonyAndroulakis/niivue.R
```
3. Install niivue R library
```
cd niivue.R/niivue
devtools::install()
```
4. Use it (for more examples, see the examples folder)
```R
suppressWarnings(library(niivue))
library(shiny)

ui <- fluidPage(
  fluidRow(
    tags$div(style = "width: 100%; height: 600px;", NiivueWidget$new()$plot)
  )
)

server <- function(input, output, session) {
  volumeList <- list(
    list(url = "https://niivue.github.io/niivue/images/mni152.nii.gz",
         colormap = "gray", visible=TRUE, opacity=1)
  )

  nv <- NiivueWidget$new()
  nv$loadVolumes(volumeList)
}

shinyApp(ui, server)
```

# Docs
- you need a shiny server to run niivue.R
- usage mirrors that of the niivue library (camelcase)
- nv$opts and nv$graph are reactiveValues variables

# Limitations
- any large data arrays (ex: nv.volumes[0].img) are not sent back to R
- nv$thumbnailVisible(TRUE) doesn't work as intended
- only the onLocationChange niivue event handler is implemented
- no extensive documentation (just refer to https://niivue.github.io/niivue/devdocs/index.html)
