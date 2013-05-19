package info.hermesnav.android

import collection.JavaConversions._

import android.content._
import android.hardware._
import android.os.Handler
import android.util.Log

import info.hermesnav.core._
import events._
import model._

object Sensors extends SensorEventListener {

  private var gravity = new Array[Float](3)
  private var magnetic = new Array[Float](3)
  private var r = new Array[Float](16)
  private var i = new Array[Float](16)
  private var orientation = new Array[Float](3)

  def apply() {
    for(
      s <- service;
      sm <- Option(s.getSystemService(Context.SENSOR_SERVICE).asInstanceOf[SensorManager]);
      a <- Option(sm.getDefaultSensor(Sensor.TYPE_ACCELEROMETER));
      m <- Option(sm.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD))
    ) {
      sm.registerListener(this, a, SensorManager.SENSOR_DELAY_UI)
      sm.registerListener(this, m, SensorManager.SENSOR_DELAY_UI)
    }
  }

  private var _yaw = 0d
  def yaw = _yaw

  private var _pitch = 0d
  def pitch = _pitch

  private var _roll = 0d
  def roll = _roll

  private var _inclination = 0d
  def inclination = _inclination

  private var lastSpeak = System.currentTimeMillis

  private var compassEnabled:Boolean = _

  private var lastCompassToggle = System.currentTimeMillis

  private val CompassPitchThreshold = 60

  private val CompassRollThreshold = 30

  private var accuracies:Map[Sensor, Int] = Map.empty

  def onSensorChanged(e:SensorEvent) {
    accuracies.get(e.sensor).foreach { a =>
      if(a != SensorManager.SENSOR_STATUS_ACCURACY_HIGH && a != SensorManager.SENSOR_STATUS_ACCURACY_MEDIUM) {
        compassEnabled = false
        CompassEnabled(false)
        return
      }
    }
    e.sensor.getType match {
      case Sensor.TYPE_ACCELEROMETER => gravity = e.values
      case Sensor.TYPE_MAGNETIC_FIELD => magnetic = e.values
      case _ =>
    }
    SensorManager.getRotationMatrix(r, i, gravity, magnetic)
    SensorManager.getOrientation(r, orientation)
    _inclination = math.toDegrees(SensorManager.getInclination(i))
    _yaw = math.toDegrees(orientation(0))
    _pitch = math.toDegrees(orientation(1))
    _roll = math.toDegrees(orientation(2))
    if(System.currentTimeMillis-lastCompassToggle >= 2000) {
      val testPitch = pitch/10*10
      val testRoll = roll/10*10
      if(math.abs(testPitch) <= CompassPitchThreshold && !compassEnabled && math.abs(testRoll) <= CompassRollThreshold) {
        compassEnabled = true
        lastCompassToggle = System.currentTimeMillis
        CompassEnabled(compassEnabled)
      } else if((math.abs(testPitch) > CompassPitchThreshold || math.abs(testRoll) > CompassRollThreshold) && compassEnabled) {
        compassEnabled = false
        lastCompassToggle = System.currentTimeMillis
        CompassEnabled(compassEnabled)
      }
    }
    if(compassEnabled)
      DirectionChanged(Some(Direction(yaw)))
    /*if(System.currentTimeMillis-lastSpeak >= 10000) {
      service.foreach(_.sendMessage(yaw.toInt.toString))
      lastSpeak = System.currentTimeMillis
    }*/
  }

  def onAccuracyChanged(sensor:Sensor, accuracy:Int) {
    accuracies += (sensor -> accuracy)
  }

}
