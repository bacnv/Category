package code.common

import java.util

import net.liftweb.common.Full
import net.liftweb.http.{S, OkResponse}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.{JsonDSL, JsonAST, JObject, Extraction}
import org.apache.http.NameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{HttpGet, HttpPost}
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

/**
 * Created by bacnv on 07/08/2015.
 */
object Oauth2users {
  def getAccessToken() :JValue ={


    val url = "http://localhost:9000/oauth2/access_token"

    val post = new HttpPost(url)
    post.addHeader("Content-Type","application/x-www-form-urlencoded")
    val client = new DefaultHttpClient
//    val params = client.getParams
//    params.setParameter("grant_type", "password")
//    params.setParameter("client_id", "0455123f-39a6-4e42-9a9e-5b2e57de582a")
//    params.setParameter("client_secret", "2c9af814-91c7-4fe3-8861-f03a9b6e7fcc")
//    params.setParameter("username", "user1")
//    params.setParameter("password", "password")

        val nameValuePairs = new util.ArrayList[NameValuePair](1)
        nameValuePairs.add(new BasicNameValuePair("grant_type", "password"))
        nameValuePairs.add(new BasicNameValuePair("client_id", "0455123f-39a6-4e42-9a9e-5b2e57de582a"))
        nameValuePairs.add(new BasicNameValuePair("client_secret", "2c9af814-91c7-4fe3-8861-f03a9b6e7fcc"))
        nameValuePairs.add(new BasicNameValuePair("username", "user1"))
        nameValuePairs.add(new BasicNameValuePair("password", "password"))
        post.setEntity(new UrlEncodedFormEntity(nameValuePairs))

    // send the post request
    val response = client.execute(post)
    println("--- HEADERS ---")
    response.getAllHeaders.foreach(arg => println(arg))

   val entity = EntityUtils.toString(response.getEntity)

  net.liftweb.json.parse(entity)


  }
  def validate():JValue={
    val Authorization = S.getRequestHeader("Authorization")
    if(Authorization.isEmpty){
      val ret = {("status" -> "false") ~ ("errortype" -> "invail_authorization") ~ ("description" -> "Invalid Authorization") ~ ("code" -> "401")} :JValue
      return ret
    }
    val Full(auth) = Authorization

    val url = "http://localhost:9000/checkapi"
    val httpget = new HttpGet(url)
    httpget.addHeader("Authorization",auth)
    val client = new DefaultHttpClient
    val response = client.execute(httpget)

    val entity = EntityUtils.toString(response.getEntity)

    net.liftweb.json.parse(entity)


  }
//  def createOrLoginGG(code:String) :JValue={
//
//
//  }

}
