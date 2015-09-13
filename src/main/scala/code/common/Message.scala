package code.common

import net.liftweb.json.JsonAST.{JObject, JValue}
import net.liftweb.json.JsonDSL._

import scala.util.control.Breaks

/**
 * Created by phong on 7/16/2015.
 */
object Message {
  def ErrorFieldExixts(field : String) : String = {
    if(field.equals("OK"))
      return "OK"
    else
      return "Field " + field + " is not exists !"
  }
  def ErrorFieldNull(field : String) : String = {
    "Field " + field + " must be not null !"
  }
  def CheckNullReturnMess(q: JValue, listField : List[String]) : String = {
    val json = q.asInstanceOf[JObject].values
    var mess = ""
    val loop = new Breaks;
    loop.breakable {
      for (field <- listField) {
        if (json.exists(p => p._1 == field))
          mess = "OK"
        else {
          mess = ErrorFieldExixts(field)
          loop.break()
        }
      }
    }
    return mess
  }

  def returnMassage(api : String,code:String, msgerr :String,body:JValue) : JValue={

    {api -> (("header" ->(("code"-> code)~("message" -> msgerr)))~("body" -> body))} :JValue
  }
  def returnMassage(api : String,code:String, msgerr :String,body:JValue,count:Long) : JValue={

    {api -> (("header" ->(("code"-> code)~("message" -> msgerr)))~("body" -> body)~("count" -> count))} :JValue
  }
  def returnMassage(api : String,code:String, msgerr :String,body:JValue,count:Long,countss:Long,countf:Long) : JValue={

    {api -> (("header" ->(("code"-> code)~("message" -> msgerr)))~("body" -> body)~("count" -> count)~("ok" -> countss)~("fail" -> countf))} :JValue
  }
}
