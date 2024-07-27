FROM ubuntu:22.04
RUN mkdir -p /opt/skillexam \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN

WORKDIR /opt/skillexam
COPY skillexam /opt/skillexam
COPY static /opt/skillexam/static
COPY config /opt/skillexam/config

ENV YESOD_PORT=8080
ENV DEMO_LANG=${YESOD_DEMO_LANG}

EXPOSE 8080
CMD ["./skillexam"]
