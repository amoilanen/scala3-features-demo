package io.github.antivanov.scala3.examples.new_types

case class HealthStatus(isUp: Boolean = true)

trait HavingHealthStatus:
  def health: HealthStatus =
    new HealthStatus {}

case class Weather(report: String)
trait Location

trait WeatherService:
  def weatherAt(location: Location): Weather

def getWeatherAtLocation(location: Location, registeredServices: List[WeatherService & HavingHealthStatus]): Option[Weather] =
  registeredServices.find(_.health.isUp).map(_.weatherAt(location))

@main def intersectionTypesMain: Unit =
  val location = new Location {}
  val defaultService = new WeatherService with HavingHealthStatus:
    def weatherAt(location: Location): Weather =
      Weather("default-weather-data")
    override def health: HealthStatus = HealthStatus(false)
  val satelliteDataBasedService = new WeatherService with HavingHealthStatus:
    def weatherAt(location: Location): Weather =
      Weather("detailed-weather-data")

  val weather = getWeatherAtLocation(location, List(defaultService, satelliteDataBasedService))
  println(weather)