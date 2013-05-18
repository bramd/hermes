package info.hermesnav

package object android {

  var service:Option[services.LocationService] = None

  implicit def fToRunnable(f: => Unit) = new Runnable {
    def run() { f }
  }

}
