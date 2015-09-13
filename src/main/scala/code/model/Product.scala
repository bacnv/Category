package code.model

import net.liftweb.common.Full
import net.liftweb.mongodb.record.{BsonMetaRecord, BsonRecord, MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.{BsonRecordListField, StringRefField, StringPk}
import net.liftweb.record.field.{IntField, DoubleField, StringField}
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
class Product private () extends MongoRecord[Product] with StringPk[Product] {

  override def meta = Product

  object CategoryId extends StringRefField(this, Category, 512){
    override def options = Category.findAll.map(rd => (Full(rd.id.is), rd.name.is) )
  }
  object Parentid extends StringRefField(this, Product, 512){
    override def options = Product.findAll.map(rd => (Full(rd.id.is), rd.ProductName.is) )
  }
  //  object Parentid extends MongoRefField(this)
  object ParentName extends StringField(this, 512)
  object ProductName extends StringField(this, 512)
  object Description extends StringField(this, 512)
  object Ordinal extends IntField(this)
  object Status extends StringField(this, 512)
  object Note extends StringField(this, 512)
  object PathProduct extends BsonRecordListField(this, ProductPath)

  object ProductOption extends BsonRecordListField(this, Productdetail)

  // An embedded document:
  //  object factor extends BsonRecordListField(this, factorIN)

}

object Product extends Product with MongoMetaRecord[Product] {
  override def collectionName = "product"
}

class ProductPath private () extends BsonRecord[ProductPath] {
  def meta = ProductPath
  object ProductPathId extends StringRefField(this, Product, 512){
    override def options = Product.findAll.map(rd => (Full(rd.id.is), rd.ProductName.is) )
  }
  //  object FactorPathId extends StringField(this, 512)

}

object ProductPath extends ProductPath with BsonMetaRecord[ProductPath]

class Productdetail private () extends BsonRecord[Productdetail] {
  def meta = Productdetail
  object ProductdetailId extends StringField(this, 50)
  object ProductdetailName extends StringField(this, 512)
  object Description extends StringField(this, 512)
  object Attribute extends StringField(this,512)
  object Status extends StringField(this, 512)

}

object Productdetail extends Productdetail with BsonMetaRecord[Productdetail]
