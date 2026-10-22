FROM condaforge/miniforge3

# set shell to bash
SHELL ["/bin/bash", "-c"]

ENV SHELL=/bin/bash

ENV CONDA_DIR=/opt/conda

# Put conda in path so we can use conda install
ENV PATH=$CONDA_DIR/bin:$PATH

# get cascade code
COPY requirements.yaml /app/cascade/

# build conda env
RUN conda env create -p /env/cascade-env --file /app/cascade/requirements.yaml

# set workdir to cascade
WORKDIR /app

# add conda env bin to path
ENV PATH=/env/cascade-env/bin:$PATH

# set env variable for docker
ENV IN_DOCKER=true

# add path to python for reticulate
ENV RETICULATE_PYTHON=/env/cascade-env/bin/python

# add port to run carnation as command-line argument
ENV PORT=3838

EXPOSE 3838

# install cascade into env
RUN Rscript -e "setRepositories(ind=1:5); install.packages('shinymanager', repos='https://cloud.r-project.org'); remotes::install_github('NICHD-BSPC/cascadeR', upgrade='never')"

# run carnation
CMD R -e "library(cascadeR); run_cascade(options=list(host='0.0.0.0', port=as.integer(Sys.getenv('PORT'))))"

