FROM rocker/r-base

MAINTAINER Jordan Walker jiwalker@usgs.gov

RUN apt-get update \
	&& apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev

RUN install2.r \
  -r "http://cran.rstudio.com" \
  -r "http://owi.usgs.gov/R" \
  data.table \
  devtools \
  readxl \
  yaml \
  dplyr \
  reshape2 \
  tidyr \
  jsonlite \
  sbtools

RUN installGithub.r \
  USGS-R/gsplot \
  richfitz/remake \
  jread-usgs/dinosvg 
  
COPY data /data
COPY images /images
COPY scripts /scripts 
COPY layout /layout
COPY Makefile Makefile
COPY *.yaml /

VOLUME /cache
VOLUME /target

CMD ["./Makefile"]
