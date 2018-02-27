FROM base/archlinux:latest

# Base installation
RUN pacman -Syu --noconfirm  && \
    pacman --noconfirm -S \
    base-devel

# Install python
RUN pacman --noconfirm -S \
    python \
    python-pip

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

# Install R incl. packages + font for rendering
RUN pacman --noconfirm -S \
    r \
    ttf-roboto && \
    R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"

# Add user, group sudo; switch to user
RUN /usr/sbin/groupadd --system sudo && \
    /usr/sbin/useradd -m --groups sudo user && \
    /usr/sbin/sed -i -e "s/Defaults    requiretty.*/ #Defaults    requiretty/g" /etc/sudoers && \
    /usr/sbin/echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

COPY requirements.txt /tmp/
RUN pip install -r /tmp/requirements.txt

# Install yay - https://github.com/Jguer/yay
ENV yay_version=2.350
ENV yay_folder=yay_${yay_version}_x86_64
RUN cd /tmp && \
    curl -L https://github.com/Jguer/yay/releases/download/v${yay_version}/${yay_folder}.tar.gz | tar zx && \
    install -Dm755 ${yay_folder}/yay /usr/bin/yay && \
    install -Dm644 ${yay_folder}/yay.8 /usr/share/man/man8/yay.8

# Switch user to be able to use yay
USER user

# Get the keys
RUN gpg --recv-keys --keyserver hkp://pgp.mit.edu EF5430F071E1B235 && \
    gpg --recv-keys --keyserver hkp://pgp.mit.edu 702353E0F7E48EDB

# Install packages from the AUR
RUN yay --noconfirm -S swift-bin

USER root
# Set correct locale
RUN echo "LC_ALL=en_US.UTF-8" >> /etc/environment && \
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen && \
    echo "LANG=en_US.UTF-8" > /etc/locale.conf
RUN locale-gen en_US.UTF-8
ENV LC_CTYPE 'en_US.UTF-8'

ADD . /usr/src/app
WORKDIR /usr/src/app
ENV PYTHONPATH /usr/src/app

COPY ./comparison.py /
CMD ["/comparison.py"]
