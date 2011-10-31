import scala.io._
import scala.actors._
import Actor._
import scala.util.matching.Regex

val linkPattern = new Regex("""<a +href=\"([^\"]+)\"[^>]*>""", "link")

object PageLoader {
  def getPageSize(url : String, withLinks : Boolean): Int = { 
    //println("Reading " + url)
    import java.net._
    val urlCon = new URL(url).openConnection
    urlCon.setConnectTimeout(5000)
    urlCon.setReadTimeout(5000)
    
    val text = 
      try {
        Source.fromInputStream(urlCon.getInputStream).mkString
      } catch {
        case e: Exception => {
          Console.err.println("Could not read [" + url + "]") 
          ""
        }
      }
    val size = text.length
    if (withLinks) {
      val links = ((Set(): Set[String]) /: linkPattern.findAllIn(text).matchData) { 
      (s, md) => 
        val link = md.group("link")
        s + (if (link.indexOf("http") != 0) {
              if (link(0) == '/')
                (url + link)
              else
                (url + '/' + link)
            } else
              link
            )
      }

      (size /: links) { (s, link) => s + PageLoader.getPageSize(link, false) }
    } else
      size
  }
}

val urls = List("http://www.amazon.com",
                "http://www.twitter.com",
                "http://www.google.com",
                "http://www.cnn.com" )

def timeMethod(method : () => Unit) = {
  val start = System.nanoTime
  method()
  val end = System.nanoTime

  println("Method took " + (end - start) / 1000000000.0 + " seconds.")
}

def getPageSizeSequentially() = {
  for(url <- urls) {
    println("Size for " + url + ": " + PageLoader.getPageSize(url, true))
  }
}

println("Sequential run:")
timeMethod { getPageSizeSequentially }

