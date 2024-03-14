FROM erlang:25-alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    apk add --no-cache libgcc && \
    apk add --no-cache git

RUN mkdir /buildroot
WORKDIR /buildroot

COPY rebar.config rebar.lock ./
RUN rebar3 compile

COPY apps ./apps/
COPY config ./config/
RUN rebar3 as prod release

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    apk add --no-cache libgcc

COPY --from=0 /buildroot/_build/prod/rel/backend /backend

EXPOSE 8080
EXPOSE 8443

CMD ["/backend/bin/backend", "foreground"]