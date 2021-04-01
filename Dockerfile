FROM rocker/verse

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update --fix-missing \
  && apt-get install -y --no-install-recommends apt-utils
RUN apt-get install -y --no-install-recommends \
  ed \
  libnlopt-dev \
  clang \
  ccache \
  libxt-dev \
  libxml2 \
  git \
  libv8-dev \
  build-essential \
  curl \
  libssl-dev \
  devscripts

WORKDIR /home/temp/mrbayes
COPY . .
RUN cp -R .R /home/rstudio
RUN R -e "update.packages(ask = FALSE)" \
  && R -e 'devtools::install_dev_deps()' \
  && rm -r /home/temp/mrbayes

RUN apt-get install -y --no-install-recommends r-cran-rjags

ENV DEBIAN_FRONTEND teletype
