square x = x * x

discr a b c = (b ** 2 - (4 * a * c)) ** (1 / 2)

quadratic :: Float -> Float -> Float -> (Float, Float)
quadratic a b c = ((-b + discr a b c) / (2 * a), (-b - discr a b c) / (2 * a))