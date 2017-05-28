FROM hypriot/rpi-python
MAINTAINER Clement Trösa <iomonad@riseup.net>

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y -q transmission-daemon \
    apt-get clean -y 

RUN sed -i "s/127.0.0.1/*.*.*.*/" /etc/transmission-daemon/settings.json

VOLUME /var/lib/transmission-daemon/downloads
VOLUME /etc/transmission-daemon

EXPOSE 9091
EXPOSE 51413
EXPOSE 51413/udp

CMD ["/usr/bin/transmission-daemon", "-f", "-g", "/etc/transmission-daemon"]
