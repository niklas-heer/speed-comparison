FROM niklasheer/arch-python:latest

# Switch user to be able to use yay
USER user

# Get the keys
RUN gpg --recv-keys --keyserver hkp://pgp.mit.edu EF5430F071E1B235 && \
    gpg --recv-keys --keyserver hkp://pgp.mit.edu C2BF0BC433CFC8B3 && \
    gpg --recv-keys --keyserver hkp://pgp.mit.edu 702353E0F7E48EDB

# Install packages from the AUR
RUN yay --noconfirm -S php56
# RUN yay --noconfirm -S swift-bin # doesn't work atm due to faulty icu55 package

# Switch back
USER root

# Programming languages
RUN pacman --noconfirm -S pypy
RUN pacman --noconfirm -S ruby
RUN pacman --noconfirm -S php
RUN pacman --noconfirm -S rust
RUN pacman --noconfirm -S go
RUN pacman --noconfirm -S nodejs
RUN pacman --noconfirm -S lua
RUN pacman --noconfirm -S julia
RUN pacman --noconfirm -S nim
RUN pacman --noconfirm -S crystal
RUN pacman --noconfirm -S kotlin

# Install R
RUN pacman --noconfirm -S r

# Install R packages
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"

COPY requirements.txt /tmp/
RUN pip install -r /tmp/requirements.txt

# Fix the problem with open_basedir restriction while running php56
RUN sed -i '/open_basedir/d' /etc/php56/php.ini

ADD . /usr/src/app
WORKDIR /usr/src/app
ENV PYTHONPATH /usr/src/app

COPY ./comparison.py /
CMD ["/comparison.py"]
