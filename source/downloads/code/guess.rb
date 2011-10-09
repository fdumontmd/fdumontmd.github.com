def play()
  target = rand(10)
  guess = -1
  while target != guess
    puts 'Guess a number'
    guess = gets().to_i
    puts 'too high' if target < guess
    puts 'too low' if target > guess
  end
  puts 'got it'
end

play()

