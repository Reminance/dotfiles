##### STEP 1: GET KAFKA
```
wget https://mirror.bit.edu.cn/apache/kafka/2.6.0/kafka_2.13-2.6.0.tgz
tar -zxvf kafka_2.13-2.6.0.tgz
```
#### STEP 2: START THE KAFKA ENVIRONMENT
```
cd kafka_2.13-2.6.0
./bin/zookeeper-server-start.sh config/zookeeper.properties
vim bin/kafka-server-start.sh # modify vm options Xms Xmx
nohup bin/kafka-server-start.sh config/server.properties &
```
#### STEP 3: CREATE A TOPIC TO STORE YOUR EVENTS
```
bin/kafka-topics.sh --create --topic quickstart-events --bootstrap-server localhost:9092
bin/kafka-topics.sh --describe --topic quickstart-events --bootstrap-server localhost:9092
```
#### STEP 4: WRITE SOME EVENTS INTO THE TOPIC
```
bin/kafka-console-producer.sh --topic quickstart-events --bootstrap-server localhost:9092
```
#### STEP 5: READ THE EVENTS
```
bin/kafka-console-consumer.sh --topic quickstart-events --from-beginning --bootstrap-server localhost:9092
```

#### STEP 6: IMPORT/EXPORT YOUR DATA AS STREAMS OF EVENTS WITH KAFKA CONNECT

#### STEP 7: PROCESS YOUR EVENTS WITH KAFKA STREAMS

#### STEP 8: TERMINATE THE KAFKA ENVIRONMENT
```
$ rm -rf /tmp/kafka-logs /tmp/zookeeper
```
