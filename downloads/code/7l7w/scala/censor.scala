trait Censor {
  val words = Map("Shoot" -> "Pucky", "Darn" -> "Beans")
	def censor(text: String) = {
		var result = text
		words.foreach( p => 
			result = result.replaceAll(mm(c(p._1)), c(p._2)).replaceAll(mm(l(p._1)), l(p._2)).replaceAll(imm(u(p._1)), u(p._2))
		)
		result
	}
	
	/* capitalize */
	def c(str: String) = str(0).toUpper + str.substring(1).toLowerCase
	
	/* lowercase */
	def l(str: String) = str.toLowerCase
	
	/* uppercase */
	def u(str: String) = str.toUpperCase
	
	/* make matcher method */
	def mm(str: String) = "\\b" + str + "\\b"
	/* make case insensitive matcher method */
	def imm(str: String) = "(?i)\\b" + str + "\\b"
}

