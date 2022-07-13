FROM node:latest

RUN npm install -g npm
RUN npm install -g elm elm-test elm-review elm-spa

RUN mkdir /build
RUN cd /build

COPY ./ .

RUN elm-spa gen
RUN elm-spa build


# 2nd stage, use alpine image, copy over static assets from 1st stage. This helps CloudRun scale from 0 to 1 go fast
FROM nginx:1.21-alpine

ENV PORT 80
ENV HOST 0.0.0.0

COPY --from=0 ./nginx.conf /etc/nginx/conf.d/app-nginx.conf.template
COPY --from=0 ./public/ /usr/share/nginx/html/

EXPOSE 80

CMD sh -c "envsubst '\$PORT' < /etc/nginx/conf.d/app-nginx.conf.template > /etc/nginx/conf.d/default.conf && nginx -g 'daemon off;'"
