Builder := Object clone
Builder depth := 0
Builder indent := method(depth repeat("  " print))
Builder nest := method(depth = depth + 1)
Builder unnest := method(depth = depth - 1)
Builder forward := method(
	indent
	writeln("<", call message name, ">") 
	nest
	call message arguments foreach(
		arg,
		content := self doMessage(arg);
		if(content type == "Sequence", indent; writeln(content)))
	unnest
	indent
	writeln("</", call message name, ">"))
Builder ul(
	li("Io"), 
	li("Lua"), 
	li("JavaScript"))