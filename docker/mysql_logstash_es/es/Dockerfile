FROM docker.elastic.co/elasticsearch/elasticsearch:7.14.2
MAINTAINER demo
EXPOSE 9200 9300
ENV HTTP_CORS_ENABLE true
ENV HTTP_CORS_ALLOW_ORIGIN *
ENV HTTP_CORS_ALLOW_METHODS "OPTIONS,HEAD,GET,POST,PUT,DELETE"
ENV HTTP_CORS_ALLOW_HEADERS "X-Requested-With,X-Auth-Token,Content-Type,Content-Length,Authorization"
ENV TRANSPORT_HOST "127.0.0.1"
ENV CLUSTER_NAME "docker-cluster"
ENV DISCOVERY_TYPE "single-node"
ENV ES_JAVA_OPTS "-Xms512m -Xmx512m"
ENV TZ "Asia/Shanghai"

