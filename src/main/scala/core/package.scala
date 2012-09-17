package info.hermesnav

package object core {

  implicit def doubleToSpatialBuilder(value:Double) = new SpatialBuilder(value)

  class SpatialBuilder(value:Double) {
    def degrees = new Direction(value)
    def degree = degrees
    def meters = new Distance(value, Metric)
    def meter = meters
    def m = meters
    def kilometers = new Distance(value*1000, Metric)
    def kilometer = kilometers
    def km = kilometers
    def feet = new Distance(value, Imperial)
    def foot = feet
    def ft = feet
    def miles = new Distance(value*5280, Imperial)
    def mile = miles
    def mi = miles
    def kph = Speed(value km, hour)
    def mph = Speed(value miles, hour)
  }

  val hour = Hour
  val hours = Hour
  val minute = Minute
  val minutes = Minute
  val second = Second
  val seconds = Second

  val metric = Metric
  val imperial = Imperial

}
