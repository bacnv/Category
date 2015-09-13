package code.model

import net.liftweb.common.Full
import net.liftweb.mongodb.record.{BsonMetaRecord, BsonRecord, MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.{BsonRecordListField, StringRefField, StringPk}
import net.liftweb.record.field.{IntField, DoubleField, StringField}

class Factor private () extends MongoRecord[Factor] with StringPk[Factor] {

  override def meta = Factor

  object ModelId extends StringRefField(this, ModelInfo, 512){
    override def options = ModelInfo.findAll.map(rd => (Full(rd.id.is), rd.name.is) )
  }
  object Parentid extends StringRefField(this, Factor, 512){
    override def options = Factor.findAll.map(rd => (Full(rd.id.is), rd.FactorName.is) )
  }
  //  object Parentid extends MongoRefField(this)
  object ParentName extends StringField(this, 512)
  object FactorName extends StringField(this, 512)
  object Description extends StringField(this, 512)
  object Weight extends DoubleField(this)
  object Ordinal extends IntField(this)
  object Status extends StringField(this, 512)
  object Note extends StringField(this, 512)
  object PercentTotal extends DoubleField(this)
  object PathFactor extends BsonRecordListField(this, FactorPath)

  object FactorOption extends BsonRecordListField(this, FactorOptionIN)

  // An embedded document:
  //  object factor extends BsonRecordListField(this, factorIN)

}

object Factor extends Factor with MongoMetaRecord[Factor] {
  override def collectionName = "factor"
}

class FactorPath private () extends BsonRecord[FactorPath] {
  def meta = FactorPath
  object FactorPathId extends StringRefField(this, Factor, 512){
    override def options = Factor.findAll.map(rd => (Full(rd.id.is), rd.FactorName.is) )
  }
  //  object FactorPathId extends StringField(this, 512)
  object Weight extends DoubleField(this)

}

object FactorPath extends FactorPath with BsonMetaRecord[FactorPath]

class FactorOptionIN private () extends BsonRecord[FactorOptionIN] {
  def meta = FactorOptionIN
  object FactorOptionId extends StringField(this, 50)
  object FactorOptionName extends StringField(this, 512)
  object Description extends StringField(this, 512)
  object Score extends DoubleField(this)
  object Fatal extends StringField(this, 512)
  object Status extends StringField(this, 512)

}

object FactorOptionIN extends FactorOptionIN with BsonMetaRecord[FactorOptionIN]