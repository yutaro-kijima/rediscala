package main

import scala.collection._

class Store(data: concurrent.Map[String, Any]) {

  def set(key: String, value: String): String = {
    data += (key -> toLongIfAble(value))
    "OK"
  }

  def setNX(key: String, value: String): Int = {
    if (data.contains(key)) {
      0
    } else {
      set(key, value)
      1
    }
  }

  def setXX(key: String, value: String): Int = {
    if (data.contains(key)) {
      set(key, value)
      1
    } else {
      0
    }
  }

  def get(key: String): String = {
    data.getOrElse(key, "$-1").toString
  }

  def getNumber(key: String): Option[Long] = {
    data.getOrElse(key, null) match {
      case null      => null
      case i: String => tryToLong(i)
      case i: Long   => Option(i)
      case i: Int    => Option(i)
      case _         => null
    }
  }

  def incrBy(key: String, by: Long): Any = {

    def pass(i: Long): Any = {
      data.replace(key, (i + by))
      getNumber(key).getOrElse("$-1")
    }

    getNumber(key).getOrElse(null) match {
      case null    => "$-1"
      case i: Long => pass(i)
    }
  }

  def decrBy(key: String, by: Long): Any = {

    def pass(i: Long): Any = {
      data.replace(key, (i - by))
      getNumber(key).getOrElse("$-1")
    }

    getNumber(key).getOrElse(null) match {
      case null    => "$-1"
      case i: Long => pass(i)
    }
  }

  def delete(keys: List[String]): Long = {
    val preSize = data.size
    data --= keys
    preSize - data.size
  }

  def exists(keys: List[String]): Long = {
    data.size - (data -- keys).size
  }
}

