package code.common

import java.io.FileInputStream
import java.lang.{Throwable, Exception}
import java.util

import net.liftweb.common.{Box, Full}
import net.liftweb.http._
import net.liftweb.http.provider.HTTPResponse
import net.liftweb.json.JsonAST.{JObject, JValue}
import net.liftweb.util.Props
import org.apache.http.NameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{HttpGet, CloseableHttpResponse, HttpPost}
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.BasicNameValuePair
import authentikat.jwt._


import scala.concurrent.Future
import scala.io.Source
import net.liftweb.mongodb.BsonDSL._

/**
 * Created by bacnv on 31/07/2015.
 */
object Utils {

  def propsWheretoLook(filename: String): List[(String, () => Full[FileInputStream])] = {
    println(Props.mode)
    println(Props.hostName)
    Props.mode match {
      case Props.RunModes.Test => ((getClass.getResource("/props/test/" + filename).getPath, () => Full(new FileInputStream(getClass.getResource("/props/test/" + filename).getPath))) :: Nil)
      case Props.RunModes.Production => ((getClass.getResource("/props/production/" + filename).getPath, () => Full(new FileInputStream(getClass.getResource("/props/production/" + filename).getPath))) :: Nil)
      case Props.RunModes.Development => ((getClass.getResource("/props/dev/" + filename).getPath, () => Full(new FileInputStream(getClass.getResource("/props/dev/" + filename).getPath))) :: Nil)
    }
  }

  def propsWheretoLook(filename: String, any: Any): List[(String, () => Full[FileInputStream])] = {
    println(Props.mode)
    ((getClass.getResource("/props/" + filename).getPath, () => Full(new FileInputStream(getClass.getResource("/props/" + filename).getPath))) :: Nil)
  }

  def checkuserAPI(): Unit = {
    val url = "http://api.hostip.info/get_json.php?ip=12.215.42.19"
  }

  def httppost(url: String, method_name: String, header: Map[String, String], param: Map[String, String], namevaluepair: Map[String, String]): CloseableHttpResponse = {

    val client = new DefaultHttpClient
    val post = new HttpPost(url)
    //    val post = if (url_type.toLowerCase.equals("post")) new HttpPost(url) else if (url_type.toLowerCase.equals("get")) new HttpPost(url)
    try {

      if (header != null && header.size > 0) {
        for ((key, value) <- header) {
          post.addHeader(key.toString, value.toString)
        }
      }


      if (param != null && param.size > 0) {
        val params = client.getParams
        for ((key, value) <- param) {
          params.setParameter(key.toString, value.toString)
        }
        client.setParams(params)
      }


      if (namevaluepair != null && namevaluepair.size > 0) {
        val nameValuePairs = new util.ArrayList[NameValuePair](1)
        for ((key, value) <- namevaluepair) {
          nameValuePairs.add(new BasicNameValuePair(key.toString, value.toString))
        }
        post.setEntity(new UrlEncodedFormEntity(nameValuePairs))
      }
      // send the post request

      client.execute(post)
    }
    finally {
      client.close()
    }

  }
  def httpget(url: String, method_name: String, header: Map[String, String], param: Map[String, String]): CloseableHttpResponse = {

    val client = new DefaultHttpClient
    val get = new HttpGet(url)
    //    val post = if (url_type.toLowerCase.equals("post")) new HttpPost(url) else if (url_type.toLowerCase.equals("get")) new HttpPost(url)
    try {

      if (header != null && header.size > 0) {
        for ((key, value) <- header) {
          get.addHeader(key.toString, value.toString)
        }
      }


      if (param != null && param.size > 0) {
        val params = client.getParams
        for ((key, value) <- param) {
          params.setParameter(key.toString, value.toString)
        }
        client.setParams(params)
      }


//      if (namevaluepair != null && namevaluepair.size > 0) {
//        val nameValuePairs = new util.ArrayList[NameValuePair](1)
//        for ((key, value) <- namevaluepair) {
//          nameValuePairs.add(new BasicNameValuePair(key.toString, value.toString))
//        }
//        get.setEntity(new UrlEncodedFormEntity(nameValuePairs))
//      }
      // send the post request

      client.execute(get)
    }
    finally {
      client.close()
    }

  }

  def validateJWT(): Option[JValue] = {
    val Authorization = S.getRequestHeader("Authorization")
    if (Authorization.isEmpty) {
      val ret = {
        ("status" -> "false") ~ ("errortype" -> "invail_authorization") ~ ("description" -> "Invalid Authorization") ~ ("code" -> "401")
      }: JValue
      return Option(ret)
    }
    val Full(auth) = Authorization


    val claims: Option[JValue] = auth match {
      case JsonWebToken(header, claimsSet, signature) =>
        net.liftweb.json.parseOpt(claimsSet.asJsonString)
      case x =>
        None
    }
    claims
  }




}
