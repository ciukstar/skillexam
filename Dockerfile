FROM ubuntu:22.04
RUN mkdir -p /opt/skillexam \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN
ARG YESOD_TIME_ZONE
ARG YESOD_SUPERUSER_USERNAME=$YESOD_SUPERUSER_USERNAME
ARG YESOD_SUPERUSER_PASSWORD=$YESOD_SUPERUSER_PASSWORD

WORKDIR        /opt/skillexam
COPY skillexam /opt/skillexam
COPY static    /opt/skillexam/static
COPY config    /opt/skillexam/config
COPY demo      /opt/skillexam/demo

ENV YESOD_PORT=8080
ENV DEMO_LANG=${YESOD_DEMO_LANG}
ENV YESOD_TIME_ZONE=${YESOD_TIME_ZONE}
ENV YESOD_SUPERUSER_USERNAME=${YESOD_SUPERUSER_USERNAME}
ENV YESOD_SUPERUSER_PASSWORD=${YESOD_SUPERUSER_PASSWORD}

EXPOSE 8080
CMD ["./skillexam"]
