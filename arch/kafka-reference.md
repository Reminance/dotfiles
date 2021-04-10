## kafka相关使用

1. ##### 查看消息队列、创建与删除topic

   ```shell
   使用命令:kafka-topics.sh
   参数:
   --create: 创建topic
   --delete: 删除topic
   --if-exists: 如果存在则执行(用于脚本不报错)
   --bootstrap-server: 指定kafka
   --list: 查看
   --replication-factor: 用来设置主题的副本数。每个主题可以有多个副本，副本位于集群中不同的broker上，也就是说副本的数量不能超过broker的数量，否则创建主题时会失败。
   --partions: 主题分区数, kafka通过分区策略，将不同的分区分配在一个集群中的broker上，一般会分散在不同的broker上，当只有一个broker时，所有的分区就只分配到该Broker上。　　
   用法:
   #查看已创建主题
   kafka-topics.sh --zookeeper localhost:2181 --list
   kafka-topics.sh --bootstrap-server localhost:9092 --list
   #列出集群里特定主题的详细信息
   kafka-topics.sh --zookeeper localhost:2181 --describe  --topic [topic_name]
   kafka-topics.sh --bootstrap-server [ip]:9092 --describe  --topic [topic_name] 
   #在zookeeper集群中创建副本为1, 分区为1的主题
   kafka-topics.sh --create --zookeeper [ip1:port1,ip2:port2...] --replication-factor 1 --partitions 1 --topic [topic_name]
   #在zookeeper集群中创建删除主题
   kafka-topics.sh --delete --zookeeper [ip1:port1,ip2:port2...] --topic [topic_name]
   ```

2. ##### 查看组、消息队列与重置偏移量

   ```shell
   使用命令: kafka-consumer-groups.sh
    --all-groups: 所有组
    --all-topics: 所有消息队列
    --delete: 删除
    --offsets: 偏移量
     --reset-offsets: 重置偏移量到最新位置
    --delete-offsets: 删除
    --describe:
    --dry-run:
    --group:
    --topic:
   确定位移重设策略——当前支持8种设置规则：
    --to-earliest：把位移调整到分区当前最小位移
    --to-latest：把位移调整到分区当前最新位移
    --to-current：把位移调整到分区当前位移
    --to-offset <offset>： 把位移调整到指定位移处
    --shift-by N： 把位移调整到当前位移 + N处，注意N可以是负数，表示向前移动
    --to-datetime <datetime>：把位移调整到大于给定时间的最早位移处，datetime格式是yyyy-MM-ddTHH:mm:ss.xxx，比如2017-08-04T00:00:00.000
    --by-duration <duration>：把位移调整到距离当前时间指定间隔的位移处，duration格式是PnDTnHnMnS，比如PT0H5M0S
    --from-file <file>：从CSV文件中读取调整策略
    ###########################################################################
    --execute：执行真正的位移调整
    --export：把位移调整方案按照CSV格式打印，方便用户成csv文件，供后续直接使用
   查看组情况
   ```




 
