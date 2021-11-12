# Game of Thrones geo-stats

[R Shiny](https://shiny.rstudio.com/) [application](https://dariocurr.shinyapps.io/GoT-plots/) developed to discover
curious Game of Thrones geo-stats.

<img width="700" alt="Home geo-stats" src="/img/home.png">

For a better understanding, please take a look at the
[report](/doc/final_report.pdf).

## How to run

In order to run and develop the app locally, build the `dev container` using the
[Docker](https://www.docker.com/) extension of
[VScode](https://code.visualstudio.com/). Once attached, run this command:

```sh
Rscript -e 'library(methods); options(shiny.autoreload = TRUE); shiny::runApp("src/", launch.browser = TRUE)'
```
