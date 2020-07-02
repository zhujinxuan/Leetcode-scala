package Q146

import scala.collection.mutable.HashMap

trait LRU[K, V] {
  val _capacity: Int

  def safeGet(key: K): Option[V] = {
    for (entry <- table get key) yield {
      updateHead(key, entry.v)
      entry.v
    }
  }

  def updateHead(key: K, value: V) : Unit = {
    if (Some(key) == head) {
      table.updateWith(key) {
        _.map {
          case Entry(_, n, _) => Entry(None, n, value)
        }
      }
    } else {
      disconnect(key)
      head match {
        case None => {
          table.update(key, Entry(None, None, value))
          head = Some(key)
          tail = Some(key)
        }
        case Some(oldKey) => {
          table.updateWith(oldKey)  {
            _.map {
              case Entry(_, n, vh) => Entry(Some(key), n, vh)
            }
          }
          table update (key, Entry(None, Some(oldKey), value))
          head = Some(key)
        }
      }

    }
  }

  protected def disconnect(key : K) : Unit = {
    for (entry <- table get key) {

      if (Some(key) == head) {
        head = entry.next
      }

      if (Some(key) == tail) {
        tail = entry.prev
      }

      for (prev <- entry.prev) {
        table.updateWith(prev) {
          _.map {
            case Entry(p, _, v) => Entry(p, entry.next, v)
          }
        }
      }

      for (next <- entry.next) {
        table.updateWith(next) {
          _.map {
            case Entry(prev, n, v) => Entry(entry.prev, n, v)
          }
        }
      }
      table.remove(key)
    }

  }

  protected def removeTail() : Unit = {
    if (table.size > _capacity) {
      tail.foreach(disconnect)
    }
  }

  def put(key: K, value: V) : Unit = {
    if (_capacity > 0) {
      updateHead(key, value)
      removeTail()
    }
  }

  case class Entry(prev: Option[K], next: Option[K], v : V)
  val table : HashMap[K, Entry] = HashMap.empty
  var head : Option[K] = None
  var tail : Option[K] = None
}

class LRUCache(val _capacity: Int) extends LRU[Int, Int] {
  def get(key : Int) : Int = safeGet(key).getOrElse(-1)
}
