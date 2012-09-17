package info.hermesnav.android

package object model {
  implicit def fToCB(f:Map[String, String] => Boolean) = new CB(f)
}
