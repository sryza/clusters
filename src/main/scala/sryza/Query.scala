package sryza

object Query {
  val ALL_FEATURES = Array("UDFs", "UDAFs", "Window Functions", "Nested Types",
    "CSV", "Avro", "Parquet", "JSON")
}

case class Query(sql: String, cores: Int, memory: Int, duration: Int, features: Seq[String]) {
  lazy val value: Int = {
    math.max(1, duration / 5000) * (cores / 2 + memory / 4) + features.length * 4
  }
}
