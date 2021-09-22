#!/usr/bin/env bash
docker run -d -p 8787:8787 -v $(pwd):/home/rstudio/mrbayes -e ROOT=TRUE -e PASSWORD=pass mrbayes
# in your browser now go to http://localhost:8787/
# the username is: rstudio
