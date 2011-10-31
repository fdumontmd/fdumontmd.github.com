trait Censor {
  val words = Map("Shoot" -> "Pucky", "Darn" -> "Beans")
	def censor(text: String): String = {
		var result = text
		words.foreach( p => 
			result = result.replaceAll(mm(c(p._1)), c(p._2)).replaceAll(mm(l(p._1)), l(p._2)).replaceAll(imm(u(p._1)), u(p._2))
		)
		result
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

