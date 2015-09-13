package code.rest

/**
 * Created by phong on 7/10/2015.
 */

import java.util.UUID

import code.common.Message
import code.model._
import com.mongodb.{BasicDBObject, QueryBuilder}
import net.liftweb.common.Full
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{JsonResponse, LiftRules, OkResponse, S}
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.mongodb.{Limit, Skip}
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import scala.collection.immutable.::

object ModelInfoAPI extends RestHelper {

//  def init(): Unit = {
//    LiftRules.statelessDispatch.append(ModelInfoAPI)
//
//
//  }
//  serve {
//
//    case "modelinfo" :: "search"  :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "search" :: q JsonGet req => search(q)
//
//    case "modelinfo" :: "factor"::q  :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "factor" :: q :: Nil JsonGet req => getmodelfactor(q): JValue
//
//    case "modelinfo" :: "getall" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "getall" :: Nil JsonGet req => getModelInfoJSON(): JValue
//
//    case "modelinfo" :: "getbymodelinfoid" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "getbymodelinfoid" :: Nil JsonPost json -> request =>
//      for {JString(id) <- (json \\ "_id").toOpt} yield getModelInfoByIdJSON(id): JValue
//
//    case "modelinfo" :: "getbymodelinfostatus" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "getbymodelinfostatus" :: Nil JsonPost json -> request =>
//      for {JString(status) <- (json \\ "status").toOpt} yield getModelInfoByStatusJSON(status): JValue
//
//    //    case "modelinfo" :: "getbymodelinfoid" ::q:: Nil JsonGet req => getModelInfoByIdJSON(q) : JValue
//
//    case "modelinfo" :: "update" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "update" :: Nil JsonPost json -> request => updateModelInfo(json)
//
//    case "modelinfo" :: "delete" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "delete" :: Nil JsonPost json -> request =>
//      for {JString(id) <- (json \\ "_id").toOpt} yield deleteModelInfo(id)
//
//    //    case "modelinfo" :: "delete" :: id :: Nil JsonDelete req => deleteModelInfo(id)
//
//    case "modelinfo" :: "insert" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "insert" :: Nil JsonPost json -> request => insertModelInfo(json)
//
//    case "modelinfo" :: "view" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "view" :: Nil JsonPost json -> request =>
//      for {JString(id) <- (json \\ "_id").toOpt} yield viewModelInfo(id)
//
//    case "modelinfo" :: "range" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "range" :: Nil JsonPost json -> request =>
//      for {JString(id) <- (json \\ "_id").toOpt} yield range(id)
//
//    case "modelinfo" :: "rangeandupdate" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "rangeandupdate" :: Nil JsonPost json -> request =>
//      for {JString(id) <- (json \\ "_id").toOpt} yield rangeAndUpdate(id)
//
//    case "modelinfo" :: "copymodel" :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "copymodel" :: Nil JsonPost json -> request =>  {
//      S.respondAsync {
//        val s =  coppyModel(json)
//        Full(JsonResponse(s))
//      }
//    }
//    case "modelinfo" :: "delall"  :: Nil Options _ => OkResponse()
//    case "modelinfo" :: "delall" :: Nil JsonPost json -> request =>  {
//      S.respondAsync {
//        val s =  dellallmodel(json)
//        Full(JsonResponse(s))
//      }
//    }
//
//  }
//
//  def getModelInfoJSON(): JValue = {
//
//    val DBList = ModelInfo.findAll
//    if (DBList.isEmpty)
//      return code.common.Message.returnMassage("getModelInfoJSON", "1", "ModelInfo not found", null)
//    else
//      return code.common.Message.returnMassage("getModelInfoJSON", "0", "SUCCESS", DBList.map(_.asJValue))
//
//  }
//
//  def getModelInfoByIdJSON(id: String): JValue = {
//
//    val qry = QueryBuilder.start("_id").is(id)
//      .get
//
//    val DBList = ModelInfo.findAll(qry)
//
//    if (DBList.isEmpty)
//      return code.common.Message.returnMassage("getModelInfoByIdJSON", "1", "ModelInfo not found", null)
//    else
//      return code.common.Message.returnMassage("getModelInfoByIdJSON", "0", "SUCCESS", DBList.map(_.asJValue))
//
//  }
//
//  def getModelInfoByStatusJSON(status: String): JValue = {
//
//    val qry = QueryBuilder.start("status").is(status)
//      .get
//
//    val DBList = ModelInfo.findAll(qry)
//
//    if (DBList.isEmpty)
//      return code.common.Message.returnMassage("getModelInfoByStatusJSON", "1", "ModelInfo not found", null)
//    else
//      return code.common.Message.returnMassage("getModelInfoByStatusJSON", "0", "SUCCESS", DBList.map(_.asJValue))
//
//  }
//
//  def deleteModelInfo(_id: String): JValue = {
//    val qryM = QueryBuilder.start("_id").is(_id)
//      .get
//    val DBM = ModelInfo.findAll(qryM)
//    if (DBM == Nil)
//      return code.common.Message.returnMassage("deleteModelInfo", "1", "Model not found", null)
//    if (DBM(0).status.toString().equals("draft") || DBM(0).status.toString().equals("")) {
//      val qry = QueryBuilder.start("ModelId").is(_id)
//        .get
//      val DBListCheck = Factor.findAll(qry)
//      if (DBListCheck.size == 0) {
//        ModelInfo.delete(("_id" -> _id))
//        return code.common.Message.returnMassage("deleteModelInfo", "0", "SUCCESS", " DELETED ")
//      } else
//        return code.common.Message.returnMassage("deleteModelInfo", "1", " Factor is exixts ", null)
//    } else
//      return code.common.Message.returnMassage("deleteModelInfo", "1", " Model can't delete (not draft) ", null)
//  }
//
//  def insertModelInfo(q: JValue): JValue = {
//    val mess = code.common.Message.CheckNullReturnMess(q, List("name"))
//    if (mess.equals("OK")) {
//      val json = q.asInstanceOf[JObject].values
//      return code.common.Message.returnMassage("insertModelInfo", "0", "SUCCESS",
//        ModelInfo.createRecord.id(UUID.randomUUID().toString)
//          .name(json.apply("name").toString)
//          .description(if (json.exists(j => j._1.toString.equals("description"))) json.apply("description").toString.toLowerCase else "")
//          .status(if (json.exists(j => j._1.toString.equals("status"))) json.apply("status").toString.toLowerCase else "")
//          .save.asJValue)
//    } else
//      return code.common.Message.returnMassage("insertModelInfo", "1", mess, null)
//  }
//
//  def updateModelInfo(q: JValue): JValue = {
//    val json = q.asInstanceOf[JObject].values
//    val qryM = QueryBuilder.start("_id").is(json.apply("_id").toString)
//      .get
//    val DBM = ModelInfo.findAll(qryM)
//    if (DBM.equals("publish")) {
//      return code.common.Message.returnMassage("updateModelInfo", "1", "Model can't update (was published)", null)
//    } else {
//      val mess = code.common.Message.CheckNullReturnMess(q, List("_id", "name", "description", "status"))
//      if (mess.equals("OK")) {
//
//        val qry = QueryBuilder.start("_id").is(json.apply("_id").toString)
//          .get
//
//        val update = ModelInfo.findAll(qry)
//
//        return code.common.Message.returnMassage("updateModelInfo", "0", "SUCCESS",
//          update(0).update
//            .name(json.apply("name").toString)
//            .description(if (json.exists(j => j._1.toString.equals("description"))) json.apply("description").toString.toLowerCase else update(0).description.toString())
//            .status(if (json.exists(j => j._1.toString.equals("status"))) json.apply("status").toString.toLowerCase else update(0).status.toString())
//            .save.asJValue)
//
//      } else
//        return code.common.Message.returnMassage("updateModelInfo", "1", mess, null)
//    }
//  }
//
//  def viewModelInfo(id: String): JValue = {
//
//    val qry = QueryBuilder.start("ModelId").is(id).get
//
//    val DBList = Factor.findAll(qry).sortWith(_.Parentid.toString() < _.Parentid.toString())
//
//    if (DBList.isEmpty)
//      return code.common.Message.returnMassage("viewModelInfo", "1", "ModelInfo not found", null)
//    else
//      return code.common.Message.returnMassage("viewModelInfo", "0", "SUCCESS", DBList.map(_.asJValue))
//
//  }
//
//  def range(id: String): JValue = {
//
//    val range = code.rest.FactorAPI.ScoringRange(id)
//
//    val json = ("min" -> range(0).toString) ~ ("max" -> range(1).toString)
//
//    return code.common.Message.returnMassage("range", "0", "SUCCESS", json)
//  }
//
//  def rangeAndUpdate(id: String): JValue = {
//
//    var range = code.rest.FactorAPI.ScoringRange(id)
//
//    val qry = QueryBuilder.start("_id").is(id).get
//
//    val DBList = ModelInfo.findAll(qry)
//
//    return code.common.Message.returnMassage("rangeAndUpdate", "0", "SUCCESS",
//      DBList(0).update.min(range(0).toDouble).max(range(1).toDouble).save.asJValue)
//  }
//
//  def getmodelfactor(q: String): JValue = {
//
//    val dbmodel = ModelInfo.findAll("_id" -> q)
//    if (dbmodel.size == 1) {
//
//      val dbfactor = Factor.findAll(("ModelId" -> q) ~ ("Parentid" -> ""))
//
//      return Message.returnMassage("modelinfo", "0", "Success", ("model" -> dbmodel(0).asJValue) ~ ("factor" -> dbfactor.map(_.asJValue)))
//
//
//    } else if (dbmodel.size == 0) {
//      return Message.returnMassage("modelinfo", "0", "No record found", ("model" -> "") ~ ("factor" -> "[]"))
//    } else {
//      return Message.returnMassage("modelinfo", "1", "Many model found", null)
//    }
//
//
//  }
//
//  def search(q: List[String]): JValue = {
//
//
//    var pageIndex: Int = 1
//    var pageSize: Int = 5
//
//    //    val jsonMap: Map[String, String] = q.values.asInstanceOf[Map[String, String]]
//    //
//    //    for ((key, value) <- jsonMap) {
//    //      if(key.toString.equals("pageIndex")){
//    //        pageIndex = value.toString.toInt
//    //      }else if(key.toString.equals("pageIndex")){
//    //        pageSize = value.toString.toInt
//    //      }
//    //    }
//
//    //  val req =  S.request.toList
//    //    for(x<- req){
//    //      for(y<-x.paramNames){
//    //        if(y.toString.equals("pageIndex")){
//    //          pageIndex=S.param(y).toString.toInt
//    //        }else if(y.toString.equals("pageIndex")){
//    //          pageSize=S.param(y).toString.toInt
//    //        }
//    //      }
//    //    }
//
//    for (req <- S.request.toList) {
//      for (paramName <- req.paramNames) {
//        val Full(a) = S.param(paramName)
//        if (paramName.toLowerCase.equals("pageindex")) {
//          pageIndex = a.toString.toInt
//        } else if (paramName.toLowerCase.equals("pagesize")) {
//          pageSize = a.toString.toInt
//        }
//      }
//    }
//
//    //   val  a = for{
//    //      req <- S.request.toList
//    //      paramName <- req.paramNames
//    //      if(paramName.toString.equals("pageIndex"))
//    //        value <- S.param(paramName)
//    //    } yield value
//    //
//    //    val  b = for{
//    //      req <- S.request.toList
//    //      paramName <- req.paramNames
//    //      if(paramName.toString.equals("pageSize"))
//    //      value <- S.param(paramName)
//    //    } yield value
//    //    if(a.size == 1){
//    //      pageIndex = a(0).toInt
//    //    }
//    //    if(b.size ==1){
//    //      pageSize = b(0).toInt
//    //    }
//
//    val qry = QueryBuilder.start().get()
//    val db = ModelInfo.findAll(qry, new BasicDBObject("_id", -1), Skip(pageSize * (pageIndex - 1)), Limit(pageSize))
//    val count = ModelInfo.count(qry)
//
//    Message.returnMassage("modelinfo", "0", "Success", db.map(_.asJValue), count)
//  }
//
////  def coppyModel(q: JValue): JValue = {
////
////    val jsonmap: Map[String, String] = q.values.asInstanceOf[Map[String, String]]
////
////    var _id = ""
////    var access_token = ""
////    for ((key, value) <- jsonmap) {
////
////      if (key.toLowerCase.equals("_id")) {
////        _id = value.toString
////      } else if (key.toLowerCase.equals("access_token")) {
////        access_token = value.toString
////      }
////
////    }
////    if (_id == "") {
////      return Message.returnMassage("copymodel", "1", "_id params not found", null)
////    }
////    val dbmodel = ModelInfo.findAll("_id" -> _id)
////    if (dbmodel.size > 1 || dbmodel.size == 0) {
////      return Message.returnMassage("copymodel", "2", if (dbmodel.size > 1) "Found many model" else "Model not found", null)
////    }
////    val idnew = UUID.randomUUID().toString
////    var i : Int = 0
////    var namenew = ""
////    var count : Long = 1
////    while (count > 0){
////      i = i +1
////      namenew =  dbmodel(0).name.toString()+i
////      count = ModelInfo.count("name" ->namenew)
////    }
////
////    val modelinfonew = ModelInfo.createRecord.id(idnew).name(namenew).description(dbmodel(0).description.toString()).max(dbmodel(0).max.toString().toDouble)
////      .min(dbmodel(0).min.toString().toDouble).status("draft").save
////
////    val th = new Thread(saveFactor(_id,idnew))
////    th.start()
//////    Thread.sleep(3000)
////
////
////return Message.returnMassage("copymodel","0","Success",modelinfonew.asJValue)
////
////  }
////  def saveFactor (_id : String,idnew:String)   = new Thread(){
////    override def run: Unit = {
////
////      var mapid: Map[String, String] = Map()
////      val dbfactor = Factor.findAll("ModelId" -> _id)
////      val dbrate = Rating.findAll("modelid" -> _id)
////      if(dbrate.size > 0){
////        for(rate <- dbrate){
////          Rating.createRecord.id(UUID.randomUUID().toString).modelid(idnew)
////          .modelname(rate.modelname.toString()).codein(rate.codein.value).save
////
////        }
////
////
////      }
////      if (dbfactor.size > 0) {
////        var lstFactor :List[Factor] = List()
////
////        for (ft <- dbfactor) {
////          val factoridnew = UUID.randomUUID().toString
////          var listFactorOptionnew: List[FactorOptionIN] = List()
////          if (ft.FactorOption != null) {
////
////            val fop: List[FactorOptionIN] = ft.FactorOption.value
////
////            for (i <- 0 to fop.size - 1) {
////
////              listFactorOptionnew = listFactorOptionnew ::: List(
////                FactorOptionIN.createRecord.FactorOptionId(UUID.randomUUID().toString).FactorOptionName(fop(i).FactorOptionName.toString())
////                  .Description(fop(i).Description.toString()).Fatal(fop(i).Fatal.toString()).Score(fop(i).Score.toString().toDouble)
////                  .Status(fop(i).Status.toString())
////              )
////
////            }
////
////          }
////          lstFactor =lstFactor ::: List(Factor.createRecord.id(factoridnew).ModelId(idnew).Description(ft.Description.toString())
////            .FactorName(ft.FactorName.toString()).FactorOption(listFactorOptionnew).Note(ft.Note.toString())
////            .Ordinal(ft.Ordinal.toString().toInt).Parentid(ft.Parentid.toString()).ParentName(ft.ParentName.toString())
////            .PathFactor(ft.PathFactor.value).PercentTotal(ft.PercentTotal.toString().toDouble).Status(ft.Status.toString())
////            .Weight(ft.Weight.toString().toDouble))
////
////          mapid = mapid + (ft.id.toString() -> factoridnew)
////
////        }
////
////        for(i<- 0 to lstFactor.size -1){
////
////          var a = lstFactor(i).asJSON.toString()
////          var y :Int= 0
////          for((idold,idnew) <- mapid){
////            a = a.replaceAll(idold,idnew)
////          }
////
////          val Full(b) =   Factor.fromJsonString(a)
////          b.save
////
////        }
////
////
////
////      }
////    }
////  }
//
//  def dellallmodel(q:JValue) : JValue = {
//
//    val msg = Message.returnMassage("dellallmodel", "0", "Modelinfo deleted all success", null)
//    val jsonmap: Map[String, String] = q.values.asInstanceOf[Map[String, String]]
//
//    var _id = ""
//
//    for ((key, value) <- jsonmap) {
//      if (key.toLowerCase.equals("_id")) {
//        _id = value.toString
//      }
//    }
//    if (_id == "" || _id.isEmpty) {
//      return Message.returnMassage("dellallmodel", "1", "_id params not found", null)
//    }
//
//    val dbmodel = ModelInfo.findAll("_id" -> _id)
//
//    if(dbmodel.size > 0){
//      for(model <- dbmodel){
//        if(model.status.toString().equals(Props.props.apply("modelinfo.status.publish"))){
//          return Message.returnMassage("dellallmodel", "3", "Modelinfo publish", null)
//        }else if(model.status.toString().equals(Props.props.apply("modelinfo.status.publish"))){
//          return Message.returnMassage("dellallmodel", "4", "Modelinfo active", null)
//        }else{
//          val th = new Thread(thredelallmodel(_id))
//          th.start()
//          Thread.sleep(3000)
//
//
//          return Message.returnMassage("dellallmodel", "0", "Modelinfo deleted all success", null)
//        }
//
//      }
//
//    }else{
//      return Message.returnMassage("dellallmodel", "2", "Modelinfo not found", null)
//    }
//    msg
//  }
//
////  def thredelallmodel (_id : String)   = new Thread() {
////    override def run: Unit = {
////        Factor.delete("ModelId" -> _id)
////
////        Rating.delete("modelid" -> _id)
////
////        ModelInfo.delete("_id" -> _id)
////    }
////  }



}
