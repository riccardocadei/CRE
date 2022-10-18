FROM rocker/verse:4.1.0

LABEL org.opencontainers.image.authors="nkhoshnevis@g.harvard.edu"

RUN install2.r --error --repo https://mran.microsoft.com/snapshot/2022-05-24 \ 
	MASS \
	stats \
	logger \
    BART \
    bartCause \
    gbm \ 
    randomForest \
    methods \
	xgboost \
    RRF \
	data.table \
    xtable \
    glmnet \
    stabs \
    stringr \
	gnm \
	SuperLearner \
    baggr \
    dplyr \
    magrittr \
	ggplot2 \
    bcf \
    grf \
	covr 


ENV RENV_VERSION 0.15.1
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "remotes::install_github('JingyuHe/XBART')"
RUN R -e "remotes::install_github('johaupt/XBCF')"


# Linux internal commands
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		vim \
		zsh \
		&& apt-get clean

