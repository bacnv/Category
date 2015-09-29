package code.model

import java.util.UUID

import code.common.Message
import net.liftweb.json.JsonAST.JValue
import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.StringPk
import net.liftweb.record.field.{LongField, DoubleField, StringField}
import com.mongodb.{BasicDBObject, QueryBuilder}
import net.liftweb.common.Full
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{JsonResponse, LiftRules, OkResponse, S}
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.mongodb.{Limit, Skip}
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

/**
 * Created by bacnv on 9/12/15.
 */
class Category private () extends MongoRecord[Category] with StringPk[Category] {

  override def meta = Category

  // An embedded document:
  //  object modelinfo extends BsonRecordField(this, modelinfoIN)
  object name extends StringField(this, 1024)
  object description extends StringField(this, 1024)
  object status extends StringField(this, 1024)
  object code_map extends StringField(this,20)
  object created_date extends LongField(this)
}

object Category extends Category with MongoMetaRecord[Category] {
  override def collectionName = "category"


  def getall():JValue = {
    val DBList = Category.findAll
    if (DBList.isEmpty)
      return code.common.Message.returnMassage("category", "1", "Category not found", null)
    else
      return code.common.Message.returnMassage("category", "0", "SUCCESS", DBList.map(_.asJValue))


  }
  def getbyid(q:String):JValue ={
    val f = Category.find("_id" -> q)

    f match {
      case Full(s) => return  Message.returnMassage("category","0","success",s.asJValue)
      case _ => return  Message.returnMassage("category","1","Not found",""->"")
    }
  }

  def searh(q: List[String]): JValue = {
    var pageIndex: Int = 1
    var pageSize: Int = 5
    var id = ""
    var code_map = ""
    var description = ""
    var name = ""
    var status = ""
    var orderby = "created_date"
    val qry = QueryBuilder.start().get()
    //    var qry1: JObject = ("" -> "")
    var jmap : Map[String,String] = Map()
    var order = ("created_date" -> -1)
    for (req <- S.request.toList) {
      for (paramName <- req.paramNames) {
        val Full(a) = S.param(paramName)
        if (paramName.toLowerCase.equals("pageindex")) {
          pageIndex = a.toString.toInt
        } else if (paramName.toLowerCase.equals("pagesize")) {
          pageSize = a.toString.toInt
        } else if (paramName.toLowerCase.equals("id")) {
          id = a.toString
        } else if (paramName.toLowerCase.equals("name")) {
          name = a.toString
        } else if (paramName.toLowerCase.equals("status")) {
          status = a.toString
        }else if (paramName.toLowerCase.equals("code_map")) {
          code_map = a.toString
        } else if (paramName.toLowerCase.equals("description")) {
          description = a.toString
        }  else if (paramName.toLowerCase.equals("order_by")) {
          orderby = a.toString
        }
      }
    }
    if (!id.isEmpty && id != "") {
      jmap += "_id" -> id
    }
    if (!name.isEmpty && name != "") {
      jmap += "name" -> name
    }
    if (!status.isEmpty && status != "") {
      jmap += "status" -> status
    }
    if (!code_map.isEmpty && status != "") {
      jmap += "code_map" -> code_map
    }
    if (!description.isEmpty && description != "") {
      jmap += "description" -> description
    }
    

    if (!orderby.isEmpty && orderby != "") {
      order = (orderby -> -1)
    }
    val db = Category.findAll(jmap, order, Skip(pageSize * (pageIndex - 1)), Limit(pageSize))
    val count = Category.count(qry)

    Message.returnMassage("category", "0", "Success", db.map(_.asJValue), count)
  }

  def insert(q: JValue): JValue = {
    val jsonmap: Map[String, String] = q.values.asInstanceOf[Map[String, String]]
    val id = UUID.randomUUID().toString
    var name = ""
    var description = ""
    var status = ""
    var code_map = ""

   
    var created_date = System.currentTimeMillis() / 1000
   

    for ((key, value) <- jsonmap) {
      if (key.toString.equals("name")) {
        name = value
      } else if (key.toString.equals("description")) {
        description = value
      } else if (key.toString.equals("status")) {
        status = value
      }
      else if (key.toString.equals("code_map")) {
        code_map = value
      }


    }
    if (name.isEmpty || name == "") {
      return Message.returnMassage("category", "1", "Name must be exist", ("" -> ""))
    }
    if (status.isEmpty || status == "") {
      return Message.returnMassage("category", "2", "Status must be exist", ("" -> ""))
    }

    val category = Category.createRecord.id(id).created_date(created_date).description(description)
     .name(name).status(status).save(true)

    Message.returnMassage("category", "0", "Success", category.asJValue)

  }

  def update(q: JValue): JValue = {
    val jsonmap: Map[String, Any] = q.values.asInstanceOf[Map[String, Any]]
    var qry1: Map[String, String] = Map()
    var modified_date = System.currentTimeMillis() / 1000
    var id = ""

    //    val bu = QueryBuilder.start("_id").is("55cc53aae4b0fb6acad9a144").get
    //    val avc = Category.findAll("_id" -> "a468451b-5faf-4c14-b389-bc1898dcaa87")
    //
    //    println(avc.size)
    for ((key, value) <- jsonmap) {
      if (key.toString.equals("_id")) {
        id = value.toString

      }
      else if (key.toString.equals("name")) {
        if (value.toString.isEmpty || value == "") {
          return Message.returnMassage("category", "1", "Name must be exist", ("" -> ""))
        }
        qry1 += key -> value.toString
      } else if (key.toString.equals("description")) {

        qry1 += key -> value.toString
      } else if (key.toString.equals("status")) {

        if (value.toString.isEmpty || value == "") {
          return Message.returnMassage("category", "2", "Status must be exist", ("" -> ""))
        }
        qry1 += key -> value.toString
      }else if (key.toString.equals("code_map")) {

        qry1 += key -> value.toString
      }


    }
//    qry1 += "modified_date" -> modified_date.toString

    if (id.isEmpty || id == "") {
      return Message.returnMassage("category", "3", "Id must be exist", ("" -> ""))
    }
    val count = Category.findAll("_id" -> id)
    if (count.size == 0) {
      return Message.returnMassage("category", "4", "category not found", ("" -> ""))
    }


    Category.update(("_id" -> id), ("$set" -> qry1))
    val application = Category.findAll("_id" -> id)

    Message.returnMassage("category", "0", "Success", application(0).asJValue)

  }

  def delete(q: String): JValue = {
    Category.delete(("_id" -> q))
    Message.returnMassage("category", "0", "Success", ("" -> ""))
  }
}