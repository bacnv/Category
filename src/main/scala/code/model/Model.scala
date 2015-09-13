package code.model

import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.StringPk
import net.liftweb.record.field.{DoubleField, StringField}

class ModelInfo private () extends MongoRecord[ModelInfo] with StringPk[ModelInfo] {

  override def meta = ModelInfo

  // An embedded document:
  //  object modelinfo extends BsonRecordField(this, modelinfoIN)
  object name extends StringField(this, 1024)
  object description extends StringField(this, 1024)
  object status extends StringField(this, 1024)
  object min extends DoubleField(this)
  object max extends DoubleField(this)

}

object ModelInfo extends ModelInfo with MongoMetaRecord[ModelInfo] {
  override def collectionName = "modelinfo"
}