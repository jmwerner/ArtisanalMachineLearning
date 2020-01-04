FROM rocker/tidyverse:3.5.3

RUN apt-get update && \
    apt-get install -y libssh-dev && \
    install2.r --error \
    --deps TRUE \
    --skipinstalled \
    remotes  && \
    R -e "remotes::install_cran(c('devtools', 'pak', 'ssh',\
      'desc','dockerfiler', 'testthat', 'vdiffr', 'pkgload',\
      'rcmdcheck','covr', 'pkgbuild', 'pkgdown', 'config', 'viridis',\
      'viridis', 'Rtsne', 'rprojroot', 'knitr', 'rmarkdown'))"