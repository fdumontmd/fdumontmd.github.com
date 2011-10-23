getGuess := method(
	in := File standardInput
    "Enter your guess" println
    in readLine asNumber)

runGame := method(
	target := Random value(1, 101) floor
    "I am thinking of a number between 1 and 100. Can you guess it?" println
	found := false
	for(i, 1, 10, 
		g := getGuess
		if(target == g, found = true; break, "Nope, try again" println)
		if(i > 1, if(g > target, "smaller" println, "bigger" println)))
	if(found,
		"You guessed it!" println,
		"Sorry, the number was " print; target println))
