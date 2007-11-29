def fib(n)
  if n < 2
    n
  else
    fib(n-1) + fib(n-2)
  end
end

36.times do |i|
  puts "#{i} => #{fib(i)}"
end
