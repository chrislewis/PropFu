package propfu

import org.specs._
import PropFu._

object PropsSpec extends Specification {
  
  "get" should {
    val props = new Props(Map("isTrue" -> "true", "minCount" -> "5"))
    
    "yield a String" in {
      props.get[String]("isTrue") must_== Some("true")
    }
    "yield a Boolean" in {
      props.get[Boolean]("isTrue") must_== Some(true)
    }
    "yield an Int" in {
      props.get[Int]("minCount") must_== Some(5)
    }
    "yield a Long" in {
      props.get[Long]("minCount") must_== Some(5l)
    }
    "yield a Float" in {
      props.get[Float]("minCount") must_== Some(5f)
    }
    "yield a Double" in {
      props.get[Double]("minCount") must_== Some(5d)
    }
  }
  
  "props" should {
    "convert to java.util.Properties" in {
      val props = new Props(Map("isTrue" -> "true", "minCount" -> "5")).toJava
      props.size must_== (2)
      props.getProperty("isTrue") must_== "true"
      props.getProperty("minCount") must_== "5"
    }
  }
  
  "props" should {
    "be instantiable from java.util.Properties" in {
      val j = new java.util.Properties
      j.setProperty("1", "one")
      j.setProperty("2", "two")
      j.setProperty("3", "three")
      
      val props = Props.fromJava(j)
      // TODO needs to implement Hash of sorts
      props.get[String]("1") must_== Some("one")
      props.get[String]("2") must_== Some("two")
      props.get[String]("3") must_== Some("three")
    }
  }
  
}