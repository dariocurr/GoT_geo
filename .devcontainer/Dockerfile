ARG SHINY_VERSION
FROM rocker/shiny:${SHINY_VERSION}

ARG USER

USER root

RUN useradd -ms /bin/bash ${USER}
RUN chown -R ${USER}:${USER} /usr/local/

RUN apt update
RUN apt upgrade -y
RUN apt install git libudunits2-dev libgdal-dev libssl-dev xdg-utils -y

RUN Rscript -e 'install.packages(c("readr", "dplyr", "sf", "ggplot2"))'

USER ${USER}
