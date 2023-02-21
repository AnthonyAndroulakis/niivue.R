# niivue.R
A WebGL2-based NIFTI volume viewer in R

# Installation
```
git clone https://github.com/anthonyandroulakis/niivue.R
```
open R Studio. Then, in R Studio:
```R
setwd("/path/to/niivue.R")
library(niivue)
library(dplyr) #in order to be able to use %>%
```

# Usage
```R
niivue() %>%
  onRender("function(el) { console.log(el); }")
```

# Development
Currently, messaging between R and js does not work. If this messaging can be implemented (as has been implemented in other libraries that use both js and R, for example, see plotly.R), then something like this could be done:
```R
niivue() %>%
  addVolume("https://niivue.github.io/niivue/images/mni152.nii.gz")
```
2 helpful resources are:
- https://shiny.rstudio.com/articles/js-send-message.html
- https://github.com/FrissAnalytics/shinyJsTutorials/blob/master/tutorials/materials3/C3/R/C3.R

# Rebuilding
Open R Studio, and run these:
```R
setwd("path/to/niivue.R")
setwd("./niivue")
devtools::install()
```
It might help to close and reopen R studio. Then, reinstall the niivue library and use the niivue library.