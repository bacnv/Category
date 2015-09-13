import java.util.Properties

import org.apache.kafka.clients.producer.{ProducerRecord, KafkaProducer}

/**
 * Created by phong on 7/24/2015.
 */
object ProducerKafka {
  def main(args: Array[String]) {
    val props = new Properties()
    props.put("metadata.broker.list", "10.15.171.36:9092")
    props.put("serializer.class", "kafka.serializer.StringEncoder")
    props.put("producer.type", "async")
    //    val config = new ProducerConfig(props)
    val producer = new KafkaProducer[String, String](props)
    val data = new ProducerRecord[String, String]("TransactionTopic", "aaaaaaaaaaaaaa");
    producer.send(data);
    producer.close();
  }
}
