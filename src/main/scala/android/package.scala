package info.hermesnav

package object android {
  implicit def fToRunnable(f: => Unit) = new Runnable {
    def run() { f }
  }
}
