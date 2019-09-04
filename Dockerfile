FROM nginx
RUN mkdir -p /usr/share/nginx/html/cmapR
COPY docs/build/html /usr/share/nginx/html/cmapR/
COPY nginx.conf /etc/nginx/
EXPOSE 9080
CMD ["nginx", "-g", "daemon off;"]
