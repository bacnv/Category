package code.rest


import net.liftweb.common.Full
import net.liftweb.http.{JsonResponse, S, OkResponse, LiftRules}
import net.liftweb.http.rest.RestHelper
import code.model.Category

/**
 * Created by bacnv on 9/12/15.
 */
object CategoryAPI extends RestHelper{

  def init(): Unit = {
    LiftRules.statelessDispatch.append(CategoryAPI)
  }

  serve{
    case "category" ::"search":: Nil  Options _ => OkResponse()
    case "category" ::"getall":: Nil  Options _ => OkResponse()
    case "category" ::"insert":: Nil Options _ => OkResponse()
    case "category" ::"update":: Nil Options _ => OkResponse()
    case "category" ::"delete":: q :: Nil Options _ => OkResponse()
    case "category" :: "id" :: q :: Nil Options _ => OkResponse()

    case "category" :: "id" :: q :: Nil JsonGet req => {
      S.respondAsync {
        val s = Category.getbyid(q)
        Full(JsonResponse(s))
      }
    }
    case "category" :: "getall" :: Nil JsonGet req => Category.getall()
    case "category" :: "search" :: q Post req => Category.searh(q)
    case "category" :: "insert" :: Nil JsonPost json -> request => Category.insert(json)
    case "category" :: "update" :: Nil JsonPost json -> request => Category.update(json)
    case "category" :: "delete" :: q :: Nil JsonDelete req => Category.delete(q)

  }

}
