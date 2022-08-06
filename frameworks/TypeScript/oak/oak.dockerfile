FROM denoland/deno

EXPOSE 8080

WORKDIR /app

USER deno

COPY ./src/deps.ts .
RUN deno cache deps.ts

ADD ./src .
CMD [ "run", "--allow-net", "main.ts" ]