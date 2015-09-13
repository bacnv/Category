package code.rest

/**
 * Created by phong on 7/8/2015.
 */

import java.util.UUID

import code.model._
import com.mongodb.{QueryBuilder, BasicDBObject}
import net.liftweb.http.{OkResponse, LiftRules}
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.util.Helpers._

object FactorAPI extends RestHelper {


  def init(): Unit = {
    LiftRules.statelessDispatch.append(FactorAPI)
  }
  serve {
    case "factor" :: "getall" :: Nil Options _ => OkResponse()
    case "factor" :: "getall" :: Nil JsonGet req => getFactorJSON(): JValue

    case "factor" :: "getbyfactorid" :: Nil Options _ => OkResponse()

    case "factor" :: "getbyfactorid" :: Nil JsonPost json -> request =>
      for {JString(id) <- (json \\ "_id").toOpt} yield getFactorByIdJSON(id): JValue

    case "factor" :: "update" :: Nil Options _ => OkResponse()
    case "factor" :: "update" :: Nil JsonPost json -> request => updateFactor(json)

    case "factor" :: "delete" :: Nil Options _ => OkResponse()
    case "factor" :: "delete" :: Nil JsonPost json -> request =>
      for {JString(id) <- (json \\ "_id").toOpt} yield deleteFactor(id)

    case "factor" :: "insert" :: Nil Options _ => OkResponse()
    case "factor" :: "insert" :: Nil JsonPost json -> request => insertFactor(json)

    //--------------------------------------------------------------------------------------------------------------------

    case "factoroption" :: "getbyfactoroptionid" :: Nil Options _ => OkResponse()
    case "factoroption" :: "getbyfactoroptionid" :: Nil JsonPost json -> request =>
      for {JString(factorId) <- (json \\ "FactorId").toOpt
           JString(factorOptionId) <- (json \\ "_id").toOpt
      } yield getFactorOptionByIdJSON(factorId, factorOptionId): JValue

    case "factoroption" :: "deleteoption" :: Nil Options _ => OkResponse()
    case "factoroption" :: "deleteoption" :: Nil JsonPost json -> request =>
      for {JString(idFactor) <- (json \\ "FactorId").toOpt
           JString(idFactorOption) <- (json \\ "_id").toOpt
      } yield deleteOptionFactor(idFactor, idFactorOption)

    case "factoroption" :: "insertoption" :: Nil Options _ => OkResponse()
    case "factoroption" :: "insertoption" :: Nil JsonPost json -> request => insertFactorOption(json)

    case "factoroption" :: "updateoption" :: Nil Options _ => OkResponse()
    case "factoroption" :: "updateoption" :: Nil JsonPost json -> request => updateFactorOption(json)

  }

  def getFactorJSON(): JValue = {
    val DBList = Factor.findAll
    if (DBList.isEmpty)
      code.common.Message.returnMassage("getall", "1", "Factor not found", null)
    else code.common.Message.returnMassage("getall", "0", "SUCCESS", DBList.map(_.asJValue))



  }

  def getFactorByIdJSON(id: String): JValue = {

    val qry = QueryBuilder.start("_id").is(id).get

    val DBList = Factor.findAll(qry)

    if (DBList.isEmpty)
      code.common.Message.returnMassage("getfactorbyid", "1", "Factor not found", null)
    else code.common.Message.returnMassage("getfactorbyid", "0", "SUCCESS", DBList.map(_.asJValue))
  }

  def getFactorOptionByIdJSON(factorId: String, factorOptionId: String): JValue = {

    val qry = QueryBuilder.start("_id").is(factorId).get

    val DBList = Factor.findAll(qry)

    if(DBList == Nil)
      return code.common.Message.returnMassage("getFactorOptionByIdJSON", "1", "Factor not found", null)
    val factor_option = DBList(0).FactorOption.value.find(fo => fo.FactorOptionId.toString() == factorOptionId.toString)
    if (DBList(0).FactorOption.value.isEmpty || factor_option == None)
      return code.common.Message.returnMassage("getFactorOptionByIdJSON", "1", "FactorOption not found", null)
    else
      return code.common.Message.returnMassage("getFactorOptionByIdJSON", "0", "SUCCESS",
        factor_option.map(_.asJValue))
  }

  def deleteFactor(_id: String): JValue = {
    val qryM = QueryBuilder.start("_id").is(_id)
      .get
    val DBM = ModelInfo.findAll(qryM)
    if(DBM.equals("publish") || DBM.equals("active")){
      return code.common.Message.returnMassage("deleteFactor", "1", "Factor can't delete (model is not draft) !", null)
    }else {
      var msg = code.common.Message.returnMassage("deleteFactor", "2", "Can not delete Factor", null)
      val qry: QueryBuilder = new QueryBuilder
      qry.or(QueryBuilder.start("PathFactor").elemMatch(new BasicDBObject("FactorPathId", _id)).get(),
        QueryBuilder.start("Parentid").is(_id).get()
      )

      val db = Factor.findAll(qry.get())

      if (db.size == 0) {

        var req = Factor.delete(("_id" -> _id))

        msg = code.common.Message.returnMassage("deleteFactor", "0", "SUCCESS", "Deleted")
      }
      msg
    }
  }

  def deleteOptionFactor(IdFactor: String, IdFactorOption: String): JValue = {
    val qry = QueryBuilder.start("_id").is(IdFactor).get
    val DBListOp = Factor.findAll(qry)

    val qryM = QueryBuilder.start("_id").is(DBListOp(0).ModelId.toString())
      .get
    val DBM = ModelInfo.findAll(qryM)
    if(DBM.equals("publish") || DBM.equals("active")){
      return code.common.Message.returnMassage("deleteOptionFactor", "1", "FactorOption can't delete (model is not draft) !", null)
    }else {

      val size = DBListOp(0).FactorOption.value.size
      val factorOption = DBListOp(0).FactorOption.value.filterNot(ftO => ftO.FactorOptionId.toString().equals(IdFactorOption.toString()))
      if (size == factorOption.size)
        return code.common.Message.returnMassage("deleteOptionFactor","2", "DELETE ERROR", null)
      else
        return code.common.Message.returnMassage("deleteOptionFactor","0", "SUCCESS",
                          DBListOp(0).update.FactorOption(factorOption).save.asJValue)
    }
  }

  def updateFactorOption(q: JValue): JValue = {

    val mess = code.common.Message.CheckNullReturnMess(q, List("FactorId", "_id", "FactorOptionName", "Score"))

    if(mess.equals("OK")) {
      val json = q.asInstanceOf[JObject].values
      if (json.exists(p => p._1 == "FactorId")) {
        val qry = QueryBuilder.start("_id").is(json.apply("FactorId").toString).get
        val DBLista = Factor.findAll(qry)

        val qryM = QueryBuilder.start("_id").is(DBLista(0).ModelId.toString())
          .get
        val DBM = ModelInfo.findAll(qryM)
        if(DBM.equals("publish") || DBM.equals("active")){
          return code.common.Message.returnMassage("updateFactorOption", "1", "FactorOption can't update (model is not draft !)", null)
        }

        var factorOptionUpdate: List[FactorOptionIN] = List()
        if(DBLista(0).FactorOption.value.size == 0)
          return code.common.Message.returnMassage("updateFactorOption","3", "update failed, factor have not factor option", null)
        var check = false
        for (i <- 0 to DBLista(0).FactorOption.value.size - 1) {
          if (DBLista(0).FactorOption.value(i).FactorOptionId.toString().equals(json.apply("_id").toString)) {
            var factorOption: FactorOptionIN = FactorOptionIN.FactorOptionId(json.apply("_id").toString)
              .Fatal(if(json.exists(j => j._1.toString.equals("Fatal"))) json.apply("Fatal").toString else DBLista(0).FactorOption.value(i).Fatal.toString())
              .Description(if(json.exists(j => j._1.toString.equals("Description"))) json.apply("Description").toString else DBLista(0).FactorOption.value(i).Description.toString())
              .FactorOptionName(json.apply("FactorOptionName").toString)
              .Score(json.apply("Score").toString.toDouble)
              .Status(if(json.exists(j => j._1.toString.equals("Status"))) json.apply("Status").toString else DBLista(0).FactorOption.value(i).Status.toString())
            var listFactorOptionDelete : List[FactorOptionIN] = List()
            for(fd <- DBLista(0).FactorOption.value){
              if(!fd.FactorOptionId.toString().equals(json.apply("_id").toString))
                listFactorOptionDelete = listFactorOptionDelete ::: List(fd)
            }
            factorOptionUpdate = listFactorOptionDelete ::: List(factorOption)
            check = true
          }
        }
        if(check == false)
          return code.common.Message.returnMassage("updateFactorOption","3", "_id factor option not found", null)
        return code.common.Message.returnMassage("updateFactorOption", "0", "SUCCESS"
                    , DBLista(0).update.FactorOption(factorOptionUpdate).save.asJValue)
      } else
        return code.common.Message.returnMassage("updateFactorOption","2", code.common.Message.ErrorFieldExixts("FactorId"), null)
    }else
      return code.common.Message.returnMassage("updateFactorOption","3", mess, null)
  }

  def ScoringRange(id: String): List[String] = {

    val qry = QueryBuilder.start("ModelId").is(id).get

    val DBList = Factor.findAll(qry)

    var listDBCuoi: List[Factor] = List()

    for (factor <- DBList) {
      if (factor.FactorOption.value.size != 0)
        listDBCuoi = listDBCuoi ::: List(factor)
    }

    var min: Double = 0
    var max: Double = 0

    for (factor <- listDBCuoi.distinct) {
      val list = factor.FactorOption.value.distinct.sortWith(_.Score.toString().toDouble < _.Score.toString().toDouble)

            var minIn = list(0).Score.toString().toDouble * (factor.Weight.toString().toDouble / 100)
            for (path <- factor.PathFactor.value) {
              minIn = minIn * (path.Weight.toString().toDouble / 100)
            }

            var maxIn = list(list.size - 1).Score.toString().toDouble * (factor.Weight.toString().toDouble / 100)
            for (path <- factor.PathFactor.value) {
              maxIn = maxIn * (path.Weight.toString().toDouble / 100)
            }

      min = min + minIn
      max = max + maxIn
    }

//    println("min : " + min + " - max : " + max)

    List(f"$min%1.2f", f"$max%1.2f")

  }

  def ScoringRangeAlwaysTrue(id: String): List[String] = {

    val qry = QueryBuilder.start("ModelId").is(id).get

    val DBList = Factor.findAll(qry)

    var listDBCuoi: List[Factor] = List()

    for (factor <- DBList) {
      if (factor.FactorOption.value.size != 0)
        listDBCuoi = listDBCuoi ::: List(factor)
    }

    for(factorC <- listDBCuoi){
      var pathChild : List[FactorPath] = List[FactorPath]()
      var percentTotal : Double = 0
      var check : Boolean = true
      var i = 0
      var factorTemp : Factor = Factor
      while (check == true){
        if(i == 0){
          if(factorC.Parentid.toString().equals("")){
            check = false
            factorTemp = Factor
          }else{
            val FC = Factor.findAll(QueryBuilder.start("_id").is(factorC.Parentid.toString()).get)
            val t = FC(0).copy
            factorTemp = t
            pathChild = pathChild .::: (List(FactorPath.FactorPathId(factorTemp.copy.Parentid.toString()).Weight(factorTemp.copy.Weight.toString().toDouble)))
            percentTotal = factorTemp.copy.Weight.toString().toDouble / 100
            i = i + 1
          }
        }else{
          if(factorTemp.Parentid.toString().equals("")){
            check = false
            factorTemp = Factor.createRecord
          }else{
            val FC2 = Factor.findAll(QueryBuilder.start("_id").is(factorTemp.copy.Parentid.toString()).get)
            val t = FC2(0).copy
            factorTemp = t
            pathChild = pathChild .::: (List(FactorPath.FactorPathId(factorTemp.copy.Parentid.toString()).Weight(factorTemp.copy.Weight.toString().toDouble)))
            percentTotal = percentTotal * factorTemp.copy.Weight.toString().toDouble/100
          }
        }
      }
      for(a <- pathChild){
        print(a.Weight + " " + a.FactorPathId + " ")
      }
      percentTotal = percentTotal * factorC.Weight.toString().toDouble/100
      factorC.update.PercentTotal(percentTotal).save
    }

    var min: Double = 0
    var max: Double = 0

    for (factor <- listDBCuoi.distinct) {
      val list = factor.FactorOption.value.distinct.sortWith(_.Score.toString().toDouble < _.Score.toString().toDouble)
      val minIn = list(0).Score.toString().toDouble * factor.PercentTotal.toString().toDouble
      val maxIn = list(list.size - 1).Score.toString().toDouble * factor.PercentTotal.toString().toDouble

      min = min + minIn
      max = max + maxIn
    }

        println("min : " + min + " - max : " + max)

    List(f"$min%1.2f", f"$max%1.2f")

  }

  def UpdateRangeModel(range: List[Double], ModelId: String) = {
    val qry = QueryBuilder.start("ModelId").is(ModelId).get
    val DBList = ModelInfo.findAll(qry)
    DBList(0).update.min(range(0)).max(range(1)).save
  }

  def insertFactorOption(q: JValue): JValue = {
    val mess = code.common.Message.CheckNullReturnMess(q, List("FactorId", "FactorOptionName", "Score"))
    if(mess.equals("OK")) {
      val json = q.asInstanceOf[JObject].values
      var listFactorOption: List[FactorOptionIN] = List()
      if (json != null) {
        val qry = QueryBuilder.start("_id").is(json.apply("FactorId").toString).get
        val DBList = Factor.findAll(qry)
        if(DBList == Nil)
          return code.common.Message.returnMassage("insertFactorOption", "1", "FactorOption can't insert (factor not found)!",null)

        val qryChild = QueryBuilder
          .start("ModelId").is(DBList(0).ModelId.toString())
          .and("PathFactor").elemMatch(new BasicDBObject("FactorPathId", DBList(0).id.toString()))
          .get

        val DBChild = Factor.findAll(qryChild)
        if(DBChild.size != 0)
          return code.common.Message.returnMassage("insertFactorOption", "1", "FactorOption can't insert (factor had Children)!",null)

        val qryM = QueryBuilder.start("_id").is(DBList(0).ModelId.toString())
          .get
        val DBM = ModelInfo.findAll(qryM)
        if(DBM.equals("publish") || DBM.equals("active")){
          return code.common.Message.returnMassage("insertFactorOption", "1", "FactorOption can't insert (model is not draft)!",null)
        }

        val factorOption = FactorOptionIN
          .FactorOptionId(UUID.randomUUID().toString)
          .Description(if(json.exists(j => j._1.toString.equals("Description"))) json.apply("Description").toString else "")
          .FactorOptionName(json.apply("FactorOptionName").toString)
          .Fatal(if(json.exists(j => j._1.toString.equals("Fatal"))) json.apply("Fatal").toString.toLowerCase else "no")
          .Score(json.apply("Score").toString.toDouble)
          .Status(if(json.exists(j => j._1.toString.equals("Status"))) json.apply("Status").toString.toLowerCase else "")

        listFactorOption = listFactorOption ::: DBList(0).FactorOption.value

        listFactorOption = listFactorOption ::: List(factorOption)

        val updateFactor = DBList(0).update.FactorOption(listFactorOption).save

        return code.common.Message.returnMassage("insertFactorOption","0", "SUCCESS", factorOption.asJValue)
      } else
        return code.common.Message.returnMassage("insertFactorOption", "2", "INSERT FAILED", null)
    }else
      return code.common.Message.returnMassage("insertFactorOption", "3", mess, null)
  }

  def insertFactor(q: JValue): JValue = {
      val json = q.asInstanceOf[JObject].values

    if (json.exists(p => p._1 == "ModelId")) {
      var modelId: String = ""
      if (json.apply("ModelId").toString != "") {
        val qryM = QueryBuilder.start("_id").is(json.apply("ModelId").toString)
          .get
        val DBM = ModelInfo.findAll(qryM)
        if (DBM.equals("publish") || DBM.equals("active")) {
          return code.common.Message.returnMassage("insertFactor", "1", "Factor can't insert (model is not draft) !", null)
        }
      }
    }
    else
      return code.common.Message.returnMassage("insertFactor", "2", code.common.Message.ErrorFieldNull("ModelId"), null)
    var parentName: String = ""
      if (json != null) {
        var listPathFactor: List[FactorPath] = List()
        if (json.exists(p => p._1 == "Parentid")) {
          if (json.apply("Parentid").toString != "") {
            val qry = QueryBuilder.start("_id").is(json.apply("Parentid").toString).get
            val DBList = Factor.findAll(qry)
                parentName = DBList(0).FactorName.toString()
            if (DBList != Nil) {
              listPathFactor = listPathFactor ::: DBList(0).PathFactor.value
              val factorPath = FactorPath.createRecord
                .FactorPathId(DBList(0).id.toString())
                .Weight(DBList(0).Weight.toString().toDouble)
              val x: List[FactorPath] = List(factorPath)
              listPathFactor = listPathFactor ::: x
            }

          }
        } else
          return code.common.Message.returnMassage("insertFactor", "3", code.common.Message.ErrorFieldExixts("Parentid"), null)
        var saveItem: Factor = Factor.createRecord
        val listFactorOption: List[FactorOptionIN] = List()

        if (json.exists(p => p._1 == "ModelId")) {
          var modelId: String = ""
          if (json.apply("ModelId").toString != "") {
            modelId = json.apply("ModelId").toString
            saveItem = Factor.ModelId(modelId)
          }
          else
            return code.common.Message.returnMassage("insertFactor", "3", code.common.Message.ErrorFieldNull("ModelId"), null)
        } else
          return code.common.Message.returnMassage("insertFactor", "3", code.common.Message.ErrorFieldExixts("ModelId"), null)
        if (json.exists(p => p._1 == "Parentid")) {
          var parentid: String = ""
          if (json.apply("Parentid").toString != "") {
            parentid = json.apply("Parentid").toString
          }
          saveItem = Factor.Parentid(parentid).ParentName(parentName)
        } else
          saveItem = Factor.Parentid("").ParentName("")

        if (json.exists(p => p._1 == "Name")) {
          var name: String = ""
          if (json.apply("Name").toString != "") {
            name = json.apply("Name").toString
            saveItem = Factor.FactorName(name)
          } else
            return code.common.Message.returnMassage("insertFactor", "3", code.common.Message.ErrorFieldNull("Name"), null)
        } else
          return code.common.Message.returnMassage("insertFactor", "3", code.common.Message.ErrorFieldExixts("Name"), null)

        if (json.exists(p => p._1 == "Description")) {
          var description: String = ""
          if (json.apply("Description").toString != "")
            description = json.apply("Description").toString
          saveItem = Factor.Description(description)
        } else
          saveItem = Factor.Description("")

        if (json.exists(p => p._1 == "Weight")) {
          var weight: Double = 0
          if (json.apply("Weight").toString != "")
            weight = json.apply("Weight").toString.toDouble
          saveItem = Factor.Weight(weight)
        } else
          return code.common.Message.returnMassage("insertFactor", "3", code.common.Message.ErrorFieldExixts("Weight"), null)

        if (json.exists(p => p._1 == "Ordinal")) {
          var ordinal: Int = 0
          if (json.apply("Ordinal").toString != "")
            ordinal = json.apply("Ordinal").toString.toInt
          saveItem = Factor.Ordinal(ordinal)
        } else
          saveItem = Factor.Ordinal(0)

        if (json.exists(p => p._1 == "Status")) {
          var status: String = ""
          if (json.apply("Status").toString.toLowerCase() != "")
            status = json.apply("Status").toString.toLowerCase()
          saveItem = Factor.Status(status)
        } else
          saveItem = Factor.Status("")

        if (json.exists(p => p._1 == "Note")) {
          var note: String = ""
          if (json.apply("Note").toString != "")
            note = json.apply("Note").toString
          saveItem = Factor.Note(note)
        } else
          saveItem = Factor.Note("")

        saveItem = Factor
          .id(UUID.randomUUID().toString)
          .PathFactor(listPathFactor)
          .FactorOption(listFactorOption)
          .save

        return code.common.Message.returnMassage("insertFactor", "0", "SUCCESS", saveItem.asJValue)
      } else
        return code.common.Message.returnMassage("insertFactor", "4", "INSERT FAILED", null)
  }

  def updateFactor(q: JValue): JValue = {
    val mess = code.common.Message.CheckNullReturnMess(q, List("_id"))
    if(mess.equals("OK")) {
      val json = q.asInstanceOf[JObject].values
      if(json.exists(j => j._1.toString.equals("Parentid")) && json.apply("_id").toString.equals(json.apply("Parentid").toString))
        return code.common.Message.returnMassage("updateFactor", "1", "Itself can not be a father !", null)
      val qry = QueryBuilder.start("_id").is(json.apply("_id").toString).get
      val DBUpdate = Factor.findAll(qry)

      val qryM = QueryBuilder.start("_id").is(DBUpdate(0).ModelId.toString())
        .get
      val DBM = ModelInfo.findAll(qryM)
      if(DBM == null)
        return code.common.Message.returnMassage("updateFactor", "1", "ModelInfo not found", null)
      if(DBM.equals("publish") || DBM.equals("active")){
        return code.common.Message.returnMassage("updateFactor", "1", "Factor can't insert (model is not draft)", null)
      }

      //Get path moi theo ParentID
      var listPathFactor: List[FactorPath] = List()
      if (json.exists(j => j._1.toString.equals("Parentid")) && json.apply("Parentid").toString != "") {
        val qry = QueryBuilder.start("_id").is(json.apply("Parentid").toString).get
        val DBList = Factor.findAll(qry)
        if (DBList != null) {
          if(DBList(0).FactorOption.value.size != 0)
            return code.common.Message.returnMassage("updateFactor", "1", "Factor parent had factor option !", null)
          listPathFactor = listPathFactor ::: DBList(0).PathFactor.value
          val factorPath = FactorPath.createRecord
            .FactorPathId(DBList(0).id.toString())
            .Weight(DBList(0).Weight.toString().toDouble)
          val x: List[FactorPath] = List(factorPath)
          listPathFactor = listPathFactor ::: x
        }
      }
      val saveItem = DBUpdate(0).update
      if(json.exists(j => j._1.toString.equals("Parentid")) != null && json.apply("Parentid").toString != ""){
        saveItem
          .Parentid(json.apply("Parentid").toString)
      }else{
        saveItem
          .Parentid("")
      }

      //Updaet factor
      saveItem
        .ParentName(if(json.exists(j=>j._1.toString.equals("ParentName"))) json.apply("ParentName").toString else saveItem.ParentName.toString())
        .FactorName(if(json.exists(j=>j._1.toString.equals("Name"))) json.apply("Name").toString else saveItem.FactorName.toString())
        .Weight(if(json.exists(j=>j._1.toString.equals("Weight"))) json.apply("Weight").toString.toDouble else saveItem.Weight.toString().toDouble)
        .Ordinal(if(json.exists(j=>j._1.toString.equals("Ordinal"))) json.apply("Ordinal").toString.toInt else saveItem.Ordinal.toString().toInt)
        .Status(if(json.exists(j=>j._1.toString.equals("Status"))) json.apply("Status").toString else saveItem.Status.toString())
        .Note(if(json.exists(j=>j._1.toString.equals("Note"))) json.apply("Note").toString else saveItem.Note.toString())
        .Description(if(json.exists(j=>j._1.toString.equals("Description"))) json.apply("Description").toString else saveItem.Description.toString())
        .PathFactor(listPathFactor)


      //Update factor con chau

      val qryChild = QueryBuilder
        .start("ModelId").is(DBUpdate(0).ModelId.toString())
        .and("PathFactor").elemMatch(new BasicDBObject("FactorPathId", DBUpdate(0).id.toString()))
        .get

      val DBChild = Factor.findAll(qryChild)

      for (factor <- DBChild) {
        if (factor.Parentid.toString().equals(json.apply("_id").toString)) {
          factor.update.ParentName(json.apply("ParentName").toString)
        }

        var listPathFactorchild: List[FactorPath] = List()
        listPathFactorchild = listPathFactorchild ::: listPathFactor
        var j: Int = -1
        for (i <- 0 to factor.PathFactor.value.size - 1) {

          if (factor.PathFactor.value(i).FactorPathId.toString().equals(json.apply("_id").toString)) {
            val newPath: FactorPath = FactorPath.Weight(json.apply("Weight").toString.toDouble)
              .FactorPathId(json.apply("_id").toString)
            listPathFactorchild = listPathFactorchild ::: List(newPath)
            j = i
          }
          if (j != -1 && i > j) {
            listPathFactorchild = listPathFactorchild ::: List(factor.PathFactor.value(i))
          }
        }
        factor.update.PathFactor(listPathFactorchild).save
      }

      return code.common.Message.returnMassage("updateFactor", "0", "SUCCESS", saveItem.save.asJValue)
    }else
      return code.common.Message.returnMassage("updateFactor", "1", mess, null)
  }




}
