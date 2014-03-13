package me.rickychang

import scalaj.http.{Http, HttpOptions}
import scala.io.Source
import java.util.Date
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

/**
 * Simple script to import my DailyMile runs into RunKeeper.
 *
 * Assume an input file in the default DailyMile export format,
 * a CSV file with the following fields:
 * (date, activity_type, distance (in meters), duration (in sec)...
 */

object RKSimpleImport extends App {

  case class BasicRun(date: Date, distance: Float, duration: Option[Int]) {

    def toJSON(): String = {
      var fields = Map("type" -> "Running",
        "start_time" -> HGDateFormat.format(date),
        "total_distance" -> distance.toString)
      if (duration.isDefined) fields += ("duration" -> duration.get.toString)
      compact(render(fields))
    }
  }

  def parseLine(s: String): BasicRun = {
    val fields = s.split(",")
    new BasicRun(
      DefaultInputDateFormat.parse(fields(0)),
      fields(2).toFloat,
      if (!fields(3).isEmpty) Some(fields(3).toInt) else None
    )
  }

  def postNewActivity(entry: BasicRun): Option[Int] = {
    println(entry.toJSON())
    val req = Http.postData(FitnessActivitiesEndPoint, entry.toJSON()).headers(hdrs).option(HttpOptions.connTimeout(1000)).option(HttpOptions.readTimeout(5000))
    try {
      val (responseCode, headersMap, resultString) = req.asHeadersAndParse(Http.readString)
      responseCode match {
        case 201 => {
          headersMap.get("Location").map {
            x =>
              val redirect = x.head
              val actId = """.*/(\d+)""".r
              val actId(id) = redirect
              id.toInt
          }
        }
        case _ => {
          println(headersMap.toString)
          println(responseCode)
          None
        }
      }
    } catch {
      case e: Exception =>
        Console.println(e.getMessage)
        None
    }

  }

  // Generate your own token here
  // TODO: Make this a commandline arg.
  val token = ???

  val DefaultInputDateFormat = new java.text.SimpleDateFormat("MM/dd/yyyy")

  // Health Graph API date format
  val HGDateFormat = new java.text.SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss")

  val FitnessActivitiesEndPoint = "https://api.runkeeper.com/fitnessActivities"
  val FitnessActivitesContentType = "application/vnd.com.runkeeper.NewFitnessActivity+json"

  val hdrs = List(
    "Authorization" -> s"Bearer $token",
    "Content-Type" -> FitnessActivitesContentType
  )

  val src = Source.fromFile(args(0))
  val lines = src.getLines().filter(_.length > 0)
  for (l <- lines) {
    val entry = parseLine(l)
    val res = postNewActivity(entry)
    if (res.isDefined) Console.println("Posted new activity: %d".format(res.get))
  }

}
