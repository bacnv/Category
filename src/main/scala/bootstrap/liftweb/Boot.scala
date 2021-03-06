package bootstrap.liftweb

import java.net.URI

import code.common.Utils
import code.rest._
import com.mongodb.{Mongo, ServerAddress}
import net.liftmodules.JQueryModule
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.jquery._
import net.liftweb.http.provider.HTTPParam
import net.liftweb.mongodb.{DefaultMongoIdentifier, MongoDB, MongoIdentifier}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Props

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  val MONGODBPROPSNAME = "default.props"
  Props.whereToLook = () => Utils.propsWheretoLook(MONGODBPROPSNAME)


  def boot {


    //MongoDB
    MongoUrl.defineDb(DefaultMongoIdentifier, Props.props.apply("mongodburl"))

    MongoUrl.defineDb(UsersDb, Props.props.apply("mongodbuserurl"))



    // where to search snippet
    LiftRules.addToPackages("code")

    LiftRules.supplimentalHeaders = s => s.addHeaders(
      List(
        HTTPParam("X-Lift-Version", LiftRules.liftVersion),
        HTTPParam("Access-Control-Allow-Origin", "*"),
        HTTPParam("Access-Control-Allow-Credentials", "true"),
        HTTPParam("Access-Control-Allow-Methods", "POST, GET, OPTIONS, DELETE, PUT"),
        HTTPParam("Access-Control-Allow-Headers", "X-API-KEY, x-xsrf-token,origin, authorization, Authorization, accept, client-security-token, " +
                        "Access-Control-Request-Method, WWW-Authenticate,Keep-Alive,User-Agent,X-Requested-With,Cache-Control,Content-Type")
      ))

    // Build SiteMap
    val entries = List(
      Menu.i("Home") / "index", // the simple way to declare a menu
      Menu.i("Test") / "test",

      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
	       "Static Content")))

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))


    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery191
    JQueryModule.init()


//    GroupUsersAPI.init()

//    FactorAPI.init()

//    ModelInfoAPI.init()



//    FactorOptionAPI.init()

      CategoryAPI.init()
    ProductAPI.init()

  }


}
object UsersDb extends MongoIdentifier {
  val jndiName = "UsersDB"
}
object MongoUrl {

  def defineDb(id: MongoIdentifier, url: String) {

    val uri = new URI(url)

    val db = uri.getPath drop 1
    val server = new Mongo(new ServerAddress(uri.getHost, uri.getPort))

    Option(uri.getUserInfo).map(_.split(":")) match {
      case Some(Array(user, pass)) => MongoDB.defineDbAuth(id, server, db, user, pass)
      case _ => MongoDB.defineDb(id, server, db)
    }
  }

}