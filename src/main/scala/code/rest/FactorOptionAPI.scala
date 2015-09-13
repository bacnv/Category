//import code.rest.FactorAPI._
//import net.liftweb.http.LiftRules
//import net.liftweb.http.rest.RestHelper
//import net.liftweb.http.rest.RestHelper.->
//import net.liftweb.http.rest.RestHelper.Options

//package code.rest
//
///**
// * Created by phong on 7/8/2015.
// */
//
//import java.util.UUID
//
//import code.model._
//import com.mongodb.QueryBuilder
//import net.liftweb.http.LiftRules
//import net.liftweb.http.rest.RestHelper
//import net.liftweb.json.JsonAST._
//import net.liftweb.mongodb.BsonDSL._
//import net.liftweb.util.Helpers._
//
//object FactorOptionAPI extends RestHelper {
//
//
//  def init(): Unit = {
//    LiftRules.statelessDispatch.append(FactorOptionAPI)
//  }
//
//  def getFactorOptionJSON(): JValue = {
//
//    val DBList = FactorOption.findAll
//    if(DBList.isEmpty)
//      "ERROR" -> "FactorOption not found" : JValue
//    else
//      {"FactorOptionsList" -> DBList.map(_.asJValue)} : JValue
//
//  }
//
//  def getFactorOptionByIdJSON(id : String): JValue = {
//
//    val qry = QueryBuilder.start("_id").is(id)
//      .get
//
//    val DBList = FactorOption.findAll(qry)
//
//    if(DBList.isEmpty)
//      "ERROR" -> "FactorOption not found" :JValue
//    else
//      {"FactorOptionsList" -> DBList.map(_.asJValue)} : JValue
//
//  }
//
//  def deleteFactorOption(_id : String): JValue = {
//
//    FactorOption.delete(("_id" -> _id))
//
//    { "SUCCESS" -> " DELETED " } : JValue
//
//  }
//
//  def insertFactorOption(factorid : String, name : String, description : String, score : String, fatal : String, status : String): JValue = {
//
//    val FactorOptionin = factoroptionIN.createRecord.factorid(factorid).name(name).description(description).score(score).fatal(fatal).status(status)
//
//
//
//    { "SUCCESS" -> FactorOption.createRecord.id(UUID.randomUUID().toString).factoroption(FactorOptionin).save.asJValue } : JValue
//
//  }
//
//  def updateFactorOption(id : String, factorid : String, name : String, description : String, score : String, fatal : String, status : String): JValue = {
//
//    FactorOption.update(("_id" -> id),
//      ("$set" -> ("factoroption.factorid" -> factorid)
//        ~ ("factoroption.name" -> name)
//        ~ ("factoroption.description" -> description)
//        ~ ("factoroption.score" -> score)
//        ~ ("factoroption.fatal" -> fatal)
//        ~ ("factoroption.status" -> status)))
//
//    { "SUCCESS" -> " UPDATED " } : JValue
//
//  }
//
//
//  serve {
//    case "factoroption" :: "getall"  :: Nil JsonGet req => getFactorOptionJSON() : JValue
//
//    case "factoroption" :: "getbyfactoroptionid" :: Nil Options _ => {"OK" -> "200"} :JValue
//    case "factoroption" :: "getbyfactoroptionid"  :: Nil JsonPost json -> request =>
//      for{JString(id) <- (json \\ "id").toOpt} yield getFactorOptionByIdJSON(id) : JValue
//
//    case "factoroption" :: "update" :: Nil Options _ => {"OK" -> "200"} :JValue
//    case "factoroption" :: "update" :: Nil JsonPost json -> request =>
//      for{JString(id) <- (json \\ "id").toOpt
//          JString(factorid) <- (json \\ "factorid").toOpt
//          JString(description) <- (json \\ "description").toOpt
//          JString(score) <- (json \\ "score").toOpt
//          JString(fatal) <- (json \\ "fatal").toOpt
//          JString(status) <- (json \\ "status").toOpt
//          JString(name) <- (json \\ "name").toOpt
//      } yield updateFactorOption(id, factorid, description, score, fatal, name, status)
//
//    case "factoroption" :: "delete" :: Nil Options _ => {"OK" -> "200"} :JValue
//    case "factoroption" :: "delete" :: Nil JsonPost json -> request =>
//      for{JString(id) <- (json \\ "id").toOpt} yield deleteFactorOption(id)
//
////    case "factoroption" :: "delete" :: id :: Nil JsonDelete req => deleteFactorOption(id)
//
//    case "factoroption" :: "insert" :: Nil Options _ => {"OK" -> "200"} :JValue
//    case "factoroption" :: "insert" :: Nil JsonPost json -> request =>
//      for{JString(factorid) <- (json \\ "factorid").toOpt
//          JString(description) <- (json \\ "description").toOpt
//          JString(score) <- (json \\ "score").toOpt
//          JString(fatal) <- (json \\ "fatal").toOpt
//          JString(status) <- (json \\ "status").toOpt
//          JString(name) <- (json \\ "name").toOpt
//      } yield insertFactorOption(factorid, description, score, fatal, name, status)
//  }
//
//}

package code.rest

import code.model._
import com.mongodb.QueryBuilder
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{LiftRules, OkResponse}
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.util.Helpers._

object FactorOptionAPI extends RestHelper {


  def init(): Unit = {
    LiftRules.statelessDispatch.append(FactorOptionAPI)

  }


  serve {
    //  case "factoroption" :: "getall" :: Nil JsonGet req => getFactorJSON(): JValue

    case "factoroption" :: "getbymodelid" :: Nil Options _ => OkResponse()
    case "factoroption" :: "getbymodelid" :: Nil JsonPost json -> request => searchFactorForOption(json): JValue
    //    case "factoroption" :: "getbymodelid" :: Nil JsonPut json -> request => searchFactorForOption(json): JValue

    case "factoroption" :: "getoptionbymodelid" :: Nil Options _ => OkResponse()
    case "factoroption" :: "getoptionbymodelid" :: Nil JsonPost json -> request => searchOptionForModel(json): JValue


  }

  def searchOptionForModel(q: JValue): JValue = {

    val json = q.asInstanceOf[JObject]

    val modelid = json.values.apply("modelid").toString

    val dbFind = Factor.findAll(QueryBuilder.start("ModelId").is(modelid).get)
    var lista: List[String] = List()

    var i = 0
    for (i <- 0 to dbFind.size - 1) {

      //     for {
      //       JString(parent) <- (dbFind(i).asJValue \ "Parentid").toOpt
      //       item = parent.toString
      //     } yield {
      if (!lista.contains(dbFind(i).Parentid.toString())) {
        val listb: List[String] = List(dbFind(i).Parentid.toString())

        lista = lista ::: listb
        //       }

      }

    }
    val dbin = QueryBuilder.start("ModelId").is(modelid).and("_id").notIn(lista.toArray).get

    val db = Factor.findAll(dbin)


    var msg = {
      "ERROR" -> "Not found"
    }: JValue

    if (db.size > 0) {
      var listOption:List[FactorOptionIN] = List()

      for(factor<-db){
        for(option<-factor.FactorOption.value) {
          listOption = listOption ::: List(option)
        }
      }
      msg = {
        "SUCCESS" -> listOption.map((_.asJValue))
      }: JValue
    } else {
      msg = {
        "ERROR" -> "No Record"
      }: JValue
    }
    msg
    //val a :List[Factor] = Factor.findAll("ModelId" -> "")
  }

  def searchFactorForOption(q: JValue): JValue = {

    val json = q.asInstanceOf[JObject]

    val modelid = json.values.apply("modelid").toString

    val dbFind = Factor.findAll(QueryBuilder.start("ModelId").is(modelid).get)
    var lista: List[String] = List()

    var i = 0
    for (i <- 0 to dbFind.size - 1) {

      //     for {
      //       JString(parent) <- (dbFind(i).asJValue \ "Parentid").toOpt
      //       item = parent.toString
      //     } yield {
      if (!lista.contains(dbFind(i).Parentid.toString())) {
        val listb: List[String] = List(dbFind(i).Parentid.toString())

        lista = lista ::: listb
        //       }

      }

    }
    val dbin = QueryBuilder.start("ModelId").is(modelid).and("_id").notIn(lista.toArray).get

    val db = Factor.findAll(dbin)


    var msg = {
      "ERROR" -> "Not found"
    }: JValue

    if (db.size > 0) {

      msg = {
        "SUCCESS" -> db.map((_.asJValue))
      }: JValue
    } else {
      msg = {
        "ERROR" -> "No Record"
      }: JValue
    }
    msg
//val a :List[Factor] = Factor.findAll("ModelId" -> "")
  }




}
