package propfu

import java.util.{Properties => JP}

/* A String => A transformation. */
trait PropType[A] extends ((String) => A) {
  def apply(s: String): A
}

object PropFu {
  
  def wrap[A](f: String => A) = new PropType[A] {
    def apply(s: String) = f(s)
  }
  
  implicit val string: PropType[String] = wrap { _ toString }
  implicit val bool: PropType[Boolean] = wrap {_ toBoolean}
  implicit val int: PropType[Int] = wrap { java.lang.Integer.parseInt(_) }
  implicit val long: PropType[Long] = wrap { java.lang.Long.parseLong(_) }
  implicit val float: PropType[Float] = wrap { java.lang.Float.parseFloat(_) }
  implicit val double: PropType[Double] = wrap { java.lang.Double.parseDouble(_) }
  
}

class Props(private var props: Map[String, String]) {
  /* Get a prop. If one is found, apply a matching transformation. */
  def get[A](name: String)(implicit t: PropType[A]) =
    props.get(name).map(t)
    
  def toJava = {
    val j = new java.util.Properties
    props foreach { case(n, v) => j.setProperty(n, v) }
    j
  }
}
object Props {
  def fromJava(props: java.util.Properties) = {
    new Props(Map({
      for(k <- props.keySet.toArray)
        yield (k.toString, props.getProperty(k.toString).toString)
      }:_*))
  }
}