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
