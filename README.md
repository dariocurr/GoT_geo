# Game of Thrones geo-stats

[R Shiny app](https://dariocurr.shinyapps.io/GoT-plots/) developed to discover
geo-curious GoT stats

## How to run

In order to run and develop the app locally, build the `dev container` using the
[Docker](https://www.docker.com/) extension of
[VScode](https://code.visualstudio.com/). Once attached, run this command:

```sh
Rscript -e 'library(methods); shiny::runApp("src/", launch.browser = TRUE)'
```
