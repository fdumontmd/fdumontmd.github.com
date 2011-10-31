trait Censor {
  val words = Map("Shoot" -> "Pucky", "Darn" -> "Beans")
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

class Test(val text: String) extends Censor {
	def getText() = text
	def getCensoredText() = censor(text)
}

val test = new Test("Phil Wenneck: God damn it!\n" +
"Alan Garner: Gosh darn it!\n"+
"Phil Wenneck: Shit!\n"+
"Alan Garner: Shoot!")

println("Original text:")
println(test.getText)

println("Censored text:")
println(test.getCensoredText)
