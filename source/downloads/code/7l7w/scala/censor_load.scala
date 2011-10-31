trait Censor {
	var words = Map(): Map[String, String]
	
	def load(file: String) {
		import scala.io.Source
		words = Source.fromFile(file).getLines.foldLeft(words) { (map, line) =>
			if (line.trim.length > 0) {
				val pair = line.split("\\s+")
				map + ((pair(0), pair(1)))
			} else map
		}
	}
	
	def censor(text: String): String = {
		(text /: words) { (t, p) => t.replaceAll(mm(c(p._1)), c(p._2)).replaceAll(mm(l(p._1)), l(p._2)).replaceAll(imm(u(p._1)), u(p._2)) }
	}
	
	/* capitalize */
	def c(str: String): String = str(0).toUpper + str.substring(1).toLowerCase
	
	/* lowercase */
	def l(str: String): String = str.toLowerCase
	
	/* uppercase */
	def u(str: String): String = str.toUpperCase
	
	/* make matcher method */
	def mm(str: String): String = "\\b" + str + "\\b"
	/* make case insensitive matcher method */
	def imm(str: String): String = "(?i)\\b" + str + "\\b"
}

/* testing */
class Test(val text: String) extends Censor {
	load("censor.txt")
	def getText() = text
	def getCensoredText() = censor(text)
}

val test = new Test("Phil Wenneck: God damn it!\n" +
"Alan Garner: Gosh darn it!\n"+
"Phil Wenneck: Shit!\n"+
"Alan Garner: Shoot!")

println("\nOriginal text:")
println(test.getText)

println("\nCensored text:")
println(test.getCensoredText)

println("\nImproved censored text:")
test.load("censor2.txt")
println(test.getCensoredText)
