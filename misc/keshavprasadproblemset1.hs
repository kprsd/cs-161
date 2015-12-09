-- Exercise 1.1
rate distance time = distance / time
time distance rate = distance / rate

-- Exercise 1.2
parabola (x, y) (vx, vy) t = (x + vx * t, y + vy * t - 0.5 * 9.80665 * t^2)

-- Exercise 1.3
law_of_cosines a b gamma = sqrt (a^2 + b^2 - 2 * a * b * cos (gamma * pi / 180))

-- Exercise 1.5
specializeSecond f x = 
distance rate time = rate * time
distanceForTime10 = specializeSecond distance 10