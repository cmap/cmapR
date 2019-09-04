FROM nginx
COPY docs/build/html /usr/share/nginx/html
COPY nginx.conf /etc/nginx/
EXPOSE 9080
CMD ["nginx", "-g", "daemon off;"]