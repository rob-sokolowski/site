FROM nginx:1.21-alpine

ENV PORT 80
ENV HOST 0.0.0.0

COPY ./nginx.conf /etc/nginx/conf.d/app-nginx.conf.template
COPY ./public/ /usr/share/nginx/html/
EXPOSE 80

CMD sh -c "envsubst '\$PORT' < /etc/nginx/conf.d/app-nginx.conf.template > /etc/nginx/conf.d/default.conf && nginx -g 'daemon off;'"
