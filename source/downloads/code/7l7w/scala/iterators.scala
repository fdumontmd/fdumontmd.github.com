import scala.util.matching.Regex

val re = new Regex("a")

val str = "abracadabra"

val matches = re.findAllIn(str).matchData

matches foreach {md => println(md)}
println(matches.size)
