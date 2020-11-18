# shiny-nametagger

[![gh-actions-build-status](https://github.com/royfrancis/shiny-nametagger/workflows/build/badge.svg)](https://github.com/royfrancis/shiny-nametagger/actions?workflow=build) [![docker-hub](https://img.shields.io/docker/image-size/royfrancis/shiny-nametagger?label=dockerhub)](https://hub.docker.com/repository/docker/royfrancis/shiny-nametagger)

This is an R shiny app to create labels for clip-on name badges.

![](preview.png)

## Running the app

### Run online

Click [here](https://roymf.shinyapps.io/nametagger/) to access an online instance of this app. This link may not always be active.

### Run using docker

```
docker run --rm -p 8787:8787 royfrancis/shiny-nametagger:v1.1.1
```

The app should be available through a web browser at `http://0.0.0.0:8787`.

### Run in R

Install the following R packages:

```
install.packages(c("Cairo","curl","ggplot2","ggtext","png","shiny","shinythemes","shinyAce","shinyBS","showtext"))
```

This repo is not an R package. In the root directory of this repo, run app using `shiny::runApp("app.R")`.

2020 • Roy Francis
