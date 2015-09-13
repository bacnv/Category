package code.rest

import java.util.UUID

import code.model._
import com.mongodb.{QueryBuilder, BasicDBObject}
import net.liftweb.http.{OkResponse, LiftRules}
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.util.Helpers._
/**
 * Created by bacnv on 9/12/15.
 */
object ProductAPI extends RestHelper{

  def init(): Unit = {
    LiftRules.statelessDispatch.append(ProductAPI)


  }
  serve {
    case "product" :: "getall" :: Nil Options _ => OkResponse()
    case "product" :: "getall" :: Nil JsonGet req => getproductJSON(): JValue


    case "product" :: "getbyproductid" :: Nil Options _ => OkResponse()

    case "product" :: "getbyproductid" :: Nil JsonPost json -> request =>
      for {JString(id) <- (json \\ "_id").toOpt} yield getproductByIdJSON(id): JValue

    case "product" :: "update" :: Nil Options _ => OkResponse()
    case "product" :: "update" :: Nil JsonPost json -> request => updateProduct(json)

    case "product" :: "delete" :: Nil Options _ => OkResponse()
    case "product" :: "delete" :: Nil JsonPost json -> request =>
      for {JString(id) <- (json \\ "_id").toOpt} yield deleteProduct(id)

    case "product" :: "insert" :: Nil Options _ => OkResponse()
    case "product" :: "insert" :: Nil JsonPost json -> request => insertProduct(json)

    case "productdetail" :: "getbyproductdetailid" :: Nil Options _ => OkResponse()
    case "productdetail" :: "getbyproductdetailid" :: Nil JsonPost json -> request =>
      for {JString(productId) <- (json \\ "ProductId").toOpt
           JString(productdetailId) <- (json \\ "ProductdetailId").toOpt
      } yield getFactorOptionByIdJSON(productId, productdetailId): JValue

    case "productdetail" :: "deleteproductdetail" :: Nil Options _ => OkResponse()
    case "productdetail" :: "deleteproductdetail" :: Nil JsonPost json -> request =>
      for {JString(idFactor) <- (json \\ "ProductId").toOpt
           JString(idFactorOption) <- (json \\ "ProductdetailId").toOpt
      } yield deleteOptionFactor(idFactor, idFactorOption)

    case "productdetail" :: "insertproductdetail" :: Nil Options _ => OkResponse()
    case "productdetail" :: "insertproductdetail" :: Nil JsonPost json -> request => insertFactorOption(json)

    case "productdetail" :: "updateproductdetail" :: Nil Options _ => OkResponse()
    case "productdetail" :: "updateproductdetail" :: Nil JsonPost json -> request => updateFactorOption(json)

  }



  def getproductJSON(): JValue = {
    val DBList = Product.findAll
    if (DBList.isEmpty)
      code.common.Message.returnMassage("product", "1", "product not found", null)
    else code.common.Message.returnMassage("product", "0", "SUCCESS", DBList.map(_.asJValue))



  }
  def getproductByIdJSON(id: String): JValue = {

    val qry = QueryBuilder.start("_id").is(id).get

    val DBList = Product.findAll(qry)

    if (DBList.isEmpty)
      code.common.Message.returnMassage("product", "1", "product not found", null)
    else code.common.Message.returnMassage("product", "0", "SUCCESS", DBList.map(_.asJValue))
  }


  def updateProduct(q: JValue): JValue = {
    val mess = code.common.Message.CheckNullReturnMess(q, List("_id"))
    if(mess.equals("OK")) {
      val json = q.asInstanceOf[JObject].values
      if(json.exists(j => j._1.toString.equals("Parentid")) && json.apply("_id").toString.equals(json.apply("Parentid").toString))
        return code.common.Message.returnMassage("product", "1", "Itself can not be a father !", null)
      val qry = QueryBuilder.start("_id").is(json.apply("_id").toString).get
      val DBUpdate = Product.findAll(qry)

      val qryM = QueryBuilder.start("_id").is(DBUpdate(0).CategoryId.toString())
        .get
      val DBM = Category.findAll(qryM)
      if(DBM == null)
        return code.common.Message.returnMassage("product", "1", "Category not found", null)
      if(DBM.equals("publish") || DBM.equals("active")){
        return code.common.Message.returnMassage("product", "1", "product can't insert (model is not draft)", null)
      }

      //Get path moi theo ParentID
      var listPathproduct: List[ProductPath] = List()
      if (json.exists(j => j._1.toString.equals("Parentid")) && json.apply("Parentid").toString != "") {
        val qry = QueryBuilder.start("_id").is(json.apply("Parentid").toString).get
        val DBList = Product.findAll(qry)
        if (DBList != null) {
          if(DBList(0).ProductOption.value.size != 0)
            return code.common.Message.returnMassage("product", "1", "product parent had product option !", null)
          listPathproduct = listPathproduct ::: DBList(0).PathProduct.value
          val productPath = ProductPath.createRecord
            .ProductPathId(DBList(0).id.toString())
          val x: List[ProductPath] = List(productPath)
          listPathproduct = listPathproduct ::: x
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

      //Updaet product
      saveItem
        .ParentName(if(json.exists(j=>j._1.toString.equals("ParentName"))) json.apply("ParentName").toString else saveItem.ParentName.toString())
        .ProductName(if(json.exists(j=>j._1.toString.equals("ProductName"))) json.apply("ProductName").toString else saveItem.ProductName.toString())
        .Ordinal(if(json.exists(j=>j._1.toString.equals("Ordinal"))) json.apply("Ordinal").toString.toInt else saveItem.Ordinal.toString().toInt)
        .Status(if(json.exists(j=>j._1.toString.equals("Status"))) json.apply("Status").toString else saveItem.Status.toString())
        .Note(if(json.exists(j=>j._1.toString.equals("Note"))) json.apply("Note").toString else saveItem.Note.toString())
        .Description(if(json.exists(j=>j._1.toString.equals("Description"))) json.apply("Description").toString else saveItem.Description.toString())
        .PathProduct(listPathproduct)


      //Update product con chau

//      val qryChild = QueryBuilder
//        .start("CategoryId").is(DBUpdate(0).CategoryId.toString())
//        .and("PathProduct").elemMatch(new BasicDBObject("ProductPathId", DBUpdate(0).id.toString()))
//        .get

//      val DBChild = Product.findAll(qryChild)
//
//      for (Product <- DBChild) {
//        if (Product.Parentid.toString().equals(json.apply("_id").toString)) {
//          Product.update.ParentName(json.apply("ParentName").toString)
//        }
//
//        var listPathproductchild: List[ProductPath] = List()
//        listPathproductchild = listPathproductchild ::: listPathproduct
//        var j: Int = -1
//        for (i <- 0 to Product.PathProduct.value.size - 1) {
//
//          if (Product.PathProduct.value(i).ProductPathId.toString().equals(json.apply("_id").toString)) {
//            val newPath: ProductPath = ProductPath.Weight(json.apply("Weight").toString.toDouble)
//              .ProductPathId(json.apply("_id").toString)
//            listPathproductchild = listPathproductchild ::: List(newPath)
//            j = i
//          }
//          if (j != -1 && i > j) {
//            listPathproductchild = listPathproductchild ::: List(Product.PathProduct.value(i))
//          }
//        }
//        Product.update.PathProduct(listPathproductchild).save
//      }

      return code.common.Message.returnMassage("product", "0", "SUCCESS", saveItem.save.asJValue)
    }else
      return code.common.Message.returnMassage("product", "1", mess, null)
  }

  def deleteProduct(_id: String): JValue = {
    val qryM = QueryBuilder.start("_id").is(_id)
      .get
    val DBM = Category.findAll(qryM)
    if(DBM.equals("publish") || DBM.equals("active")){
      return code.common.Message.returnMassage("product", "1", "Product can't delete (model is not draft) !", null)
    }else {
      var msg = code.common.Message.returnMassage("product", "2", "Can not delete Product", null)
      val qry: QueryBuilder = new QueryBuilder
      qry.or(QueryBuilder.start("PathProduct").elemMatch(new BasicDBObject("ProductPathId", _id)).get(),
        QueryBuilder.start("Parentid").is(_id).get()
      )

      val db = Product.findAll(qry.get())

      if (db.size == 0) {

        var req = Product.delete(("_id" -> _id))

        msg = code.common.Message.returnMassage("product", "0", "SUCCESS", "Deleted")
      }
      msg
    }
  }

  def insertProduct(q: JValue): JValue = {
    val json = q.asInstanceOf[JObject].values

    if (json.exists(p => p._1 == "CategoryId")) {
      var CategoryId: String = ""
      if (json.apply("CategoryId").toString != "") {
        val qryM = QueryBuilder.start("_id").is(json.apply("CategoryId").toString)
          .get
        val DBM = Category.findAll(qryM)
        if (DBM.equals("publish") || DBM.equals("active")) {
          return code.common.Message.returnMassage("product", "1", "Product can't insert (model is not draft) !", null)
        }
      }
    }
    else
      return code.common.Message.returnMassage("product", "2", code.common.Message.ErrorFieldNull("CategoryId"), null)
    var parentName: String = ""
    if (json != null) {
      var listPathProduct: List[ProductPath] = List()
      if (json.exists(p => p._1 == "Parentid")) {
        if (json.apply("Parentid").toString != "") {
          val qry = QueryBuilder.start("_id").is(json.apply("Parentid").toString).get
          val DBList = Product.findAll(qry)
          parentName = DBList(0).ProductName.toString()
          if (DBList != Nil) {
            listPathProduct = listPathProduct ::: DBList(0).PathProduct.value
            val productPath = ProductPath.createRecord
              .ProductPathId(DBList(0).id.toString())
            val x: List[ProductPath] = List(productPath)
            listPathProduct = listPathProduct ::: x
          }

        }
      } else
        return code.common.Message.returnMassage("product", "3", code.common.Message.ErrorFieldExixts("Parentid"), null)
      var saveItem: Product = Product.createRecord
      val listProductOption: List[Productdetail] = List()

      if (json.exists(p => p._1 == "CategoryId")) {
        var CategoryId: String = ""
        if (json.apply("CategoryId").toString != "") {
          CategoryId = json.apply("CategoryId").toString
          saveItem = Product.CategoryId(CategoryId)
        }
        else
          return code.common.Message.returnMassage("product", "3", code.common.Message.ErrorFieldNull("CategoryId"), null)
      } else
        return code.common.Message.returnMassage("product", "3", code.common.Message.ErrorFieldExixts("CategoryId"), null)
      if (json.exists(p => p._1 == "Parentid")) {
        var parentid: String = ""
        if (json.apply("Parentid").toString != "") {
          parentid = json.apply("Parentid").toString
        }
        saveItem = Product.Parentid(parentid).ParentName(parentName)
      } else
        saveItem = Product.Parentid("").ParentName("")

      if (json.exists(p => p._1 == "ProductName")) {
        var name: String = ""
        if (json.apply("ProductName").toString != "") {
          name = json.apply("ProductName").toString
          saveItem = Product.ProductName(name)
        } else
          return code.common.Message.returnMassage("product", "3", code.common.Message.ErrorFieldNull("Name"), null)
      } else
        return code.common.Message.returnMassage("product", "3", code.common.Message.ErrorFieldExixts("Name"), null)

      if (json.exists(p => p._1 == "Description")) {
        var description: String = ""
        if (json.apply("Description").toString != "")
          description = json.apply("Description").toString
        saveItem = Product.Description(description)
      } else
        saveItem = Product.Description("")

      if (json.exists(p => p._1 == "Ordinal")) {
        var ordinal: Int = 0
        if (json.apply("Ordinal").toString != "")
          ordinal = json.apply("Ordinal").toString.toInt
        saveItem = Product.Ordinal(ordinal)
      } else
        saveItem = Product.Ordinal(0)

      if (json.exists(p => p._1 == "Status")) {
        var status: String = ""
        if (json.apply("Status").toString.toLowerCase() != "")
          status = json.apply("Status").toString.toLowerCase()
        saveItem = Product.Status(status)
      } else
        saveItem = Product.Status("")

      if (json.exists(p => p._1 == "Note")) {
        var note: String = ""
        if (json.apply("Note").toString != "")
          note = json.apply("Note").toString
        saveItem = Product.Note(note)
      } else
        saveItem = Product.Note("")

      saveItem = Product
        .id(UUID.randomUUID().toString)
        .PathProduct(listPathProduct)
        .ProductOption(listProductOption)
        .save

      return code.common.Message.returnMassage("product", "0", "SUCCESS", saveItem.asJValue)
    } else
      return code.common.Message.returnMassage("product", "4", "INSERT FAILED", null)
  }
  def getFactorOptionByIdJSON(ProductId: String, ProductdetailId: String): JValue = {

    val qry = QueryBuilder.start("_id").is(ProductId).get

    val DBList = Product.findAll(qry)

    if(DBList == Nil)
      return code.common.Message.returnMassage("productdetail", "1", "Product not found", null)
    val factor_option = DBList(0).ProductOption.value.find(fo => fo.ProductdetailId.toString() == ProductdetailId.toString)
    if (DBList(0).ProductOption.value.isEmpty || factor_option == None)
      return code.common.Message.returnMassage("productdetail", "1", "Productdetail not found", null)
    else
      return code.common.Message.returnMassage("productdetail", "0", "SUCCESS",
        factor_option.map(_.asJValue))
  }
  def deleteOptionFactor(IdFactor: String, IdFactorOption: String): JValue = {
    val qry = QueryBuilder.start("_id").is(IdFactor).get
    val DBListOp = Product.findAll(qry)

    val qryM = QueryBuilder.start("_id").is(DBListOp(0).CategoryId.toString())
      .get
    val DBM = Category.findAll(qryM)
    if(DBM.equals("publish") || DBM.equals("active")){
      return code.common.Message.returnMassage("productdetail", "1", "Productdetail can't delete (Category is not draft) !", null)
    }else {

      val size = DBListOp(0).ProductOption.value.size
      val factorOption = DBListOp(0).ProductOption.value.filterNot(ftO => ftO.ProductdetailId.toString().equals(IdFactorOption.toString()))
      if (size == factorOption.size)
        return code.common.Message.returnMassage("productdetail","2", "DELETE ERROR", null)
      else
        return code.common.Message.returnMassage("productdetail","0", "SUCCESS",
          DBListOp(0).update.ProductOption(factorOption).save.asJValue)
    }
  }
  def insertFactorOption(q: JValue): JValue = {
    val mess = code.common.Message.CheckNullReturnMess(q, List("ProductId", "ProductdetailName"))
    if(mess.equals("OK")) {
      val json = q.asInstanceOf[JObject].values
      var listFactorOption: List[Productdetail] = List()
      if (json != null) {
        val qry = QueryBuilder.start("_id").is(json.apply("ProductId").toString).get
        val DBList = Product.findAll(qry)
        if(DBList == Nil)
          return code.common.Message.returnMassage("productdetail", "1", "Productdetail can't insert (factor not found)!",null)

        val qryChild = QueryBuilder
          .start("CategoryId").is(DBList(0).CategoryId.toString())
          .and("PathProduct").elemMatch(new BasicDBObject("ProductPathId", DBList(0).id.toString()))
          .get

        val DBChild = Product.findAll(qryChild)
        if(DBChild.size != 0)
          return code.common.Message.returnMassage("productdetail", "1", "FactorOption can't insert (factor had Children)!",null)

        val qryM = QueryBuilder.start("_id").is(DBList(0).CategoryId.toString())
          .get
        val DBM = Category.findAll(qryM)
        if(DBM.equals("publish") || DBM.equals("active")){
          return code.common.Message.returnMassage("productdetail", "1", "FactorOption can't insert (model is not draft)!",null)
        }

        val factorOption = Productdetail
          .ProductdetailId(UUID.randomUUID().toString)
          .Description(if(json.exists(j => j._1.toString.equals("Description"))) json.apply("Description").toString else "")
          .ProductdetailName(json.apply("ProductdetailName").toString)
          .Status(if(json.exists(j => j._1.toString.equals("Status"))) json.apply("Status").toString.toLowerCase else "" )
          .Attribute(if(json.exists(j => j._1.toString.equals("Attribute"))) json.apply("Attribute").toString.toLowerCase else "" )

        listFactorOption = listFactorOption ::: DBList(0).ProductOption.value

        listFactorOption = listFactorOption ::: List(factorOption)

        val updateFactor = DBList(0).update.ProductOption(listFactorOption).save

        return code.common.Message.returnMassage("productdetail","0", "SUCCESS", updateFactor.asJValue)
      } else
        return code.common.Message.returnMassage("productdetail", "2", "INSERT FAILED", null)
    }else
      return code.common.Message.returnMassage("productdetail", "3", mess, null)
  }
  def updateFactorOption(q: JValue): JValue = {

    val mess = code.common.Message.CheckNullReturnMess(q, List("ProductId", "ProductdetailId", "ProductdetailName"))

    if(mess.equals("OK")) {
      val json = q.asInstanceOf[JObject].values
      if (json.exists(p => p._1 == "ProductId")) {
        val qry = QueryBuilder.start("_id").is(json.apply("ProductId").toString).get
        val DBLista = Product.findAll(qry)

        val qryM = QueryBuilder.start("_id").is(DBLista(0).CategoryId.toString())
          .get
        val DBM = Category.findAll(qryM)
        if(DBM.equals("publish") || DBM.equals("active")){
          return code.common.Message.returnMassage("productdetail", "1", "Productdetail can't update (Category is not draft !)", null)
        }

        var factorOptionUpdate: List[Productdetail] = List()
        if(DBLista(0).ProductOption.value.size == 0)
          return code.common.Message.returnMassage("productdetail","3", "update failed, factor have not Productdetail", null)
        var check = false
        for (i <- 0 to DBLista(0).ProductOption.value.size - 1) {
          if (DBLista(0).ProductOption.value(i).ProductdetailId.toString().equals(json.apply("ProductdetailId").toString)) {
            var factorOption: Productdetail = Productdetail.ProductdetailId(json.apply("ProductdetailId").toString)
              .Description(if(json.exists(j => j._1.toString.equals("Description"))) json.apply("Description").toString else DBLista(0).ProductOption.value(i).Description.toString())
              .ProductdetailName(json.apply("ProductdetailName").toString)
              .Status(if(json.exists(j => j._1.toString.equals("Status"))) json.apply("Status").toString else DBLista(0).ProductOption.value(i).Status.toString())
              .Attribute(if(json.exists(j => j._1.toString.equals("Attribute"))) json.apply("Attribute").toString else DBLista(0).ProductOption.value(i).Attribute.toString())
            var listFactorOptionDelete : List[Productdetail] = List()
            for(fd <- DBLista(0).ProductOption.value){
              if(!fd.ProductdetailId.toString().equals(json.apply("ProductdetailId").toString))
                listFactorOptionDelete = listFactorOptionDelete ::: List(fd)
            }
            factorOptionUpdate = listFactorOptionDelete ::: List(factorOption)
            check = true
          }
        }
        if(check == false)
          return code.common.Message.returnMassage("productdetail","3", "_id productdetail not found", null)
        return code.common.Message.returnMassage("productdetail", "0", "SUCCESS"
          , DBLista(0).update.ProductOption(factorOptionUpdate).save.asJValue)
      } else
        return code.common.Message.returnMassage("productdetail","2", code.common.Message.ErrorFieldExixts("ProductId"), null)
    }else
      return code.common.Message.returnMassage("productdetail","3", mess, null)
  }
}
