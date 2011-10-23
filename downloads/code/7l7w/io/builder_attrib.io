Builder := Object clone
Builder depth := 0
Builder indent := method(buf := "" asMutable; depth repeat(buf appendSeq("  ")); buf)
Builder nest := method(depth = depth + 1)
Builder unnest := method(depth = depth - 1)

Builder forward := method(
	buf := "" asMutable
	args := call message arguments
	buf appendSeq(indent, "<", call message name)
	if(args size > 0 and doMessage(args at(0)) type == "Map", 
		h := doMessage(args removeFirst)
		h keys foreach(k,
		buf appendSeq(" ", k, "=\"", h at(k), "\"")))
	buf appendSeq(">\n")
	nest
	args foreach(
		arg,
		content := self doMessage(arg);
		if(content isMutable, buf appendSeq(content), buf appendSeq(indent, content, "\n")))
	unnest
	buf appendSeq(indent, "</", call message name, ">\n")
	buf)


OperatorTable addAssignOperator(":", "atPutColon")

Map atPutColon := method(
	self atPut(
		call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""), 
		call evalArgAt(1))
)

curlyBrackets := method(
	r := Map clone
	call message arguments foreach(arg,
		r doMessage(arg))
	r)
