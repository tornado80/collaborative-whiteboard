FROM erlang:25-alpine

RUN mkdir /buildroot
WORKDIR /buildroot

COPY apps backend/apps/
COPY config backend/config/
COPY rebar* backend/

WORKDIR backend

RUN rebar3 as prod release

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    apk add --no-cache libgcc

COPY --from=0 /buildroot/backend/_build/prod/rel/backend /backend

EXPOSE 8080
EXPOSE 8443

CMD ["/backend/bin/backend", "foreground"]