# 1 : 최댓값 구하기 1
a = int(input())
b = int(input())
c = int(input())

maximum = a
if b > maximum: maximum = b
if c > maximum: maximum = c

print(f'최댓값은 {maximum} 입니다.')

# 2 : 최댓값 구하기 2
def max3(a, b, c):
  maximum = a
  if b > maximum: maximum = b
  if c > maximum: maximum = c
  return(maximum)

print(f'max3(3, 2, 1) = {max3(3, 2, 1)}')

# 3 : 중앙값 구하기 1
def med3(a, b, c):
  if a >= b:
    if b >= c:
      return b
    elif a <= c:
      return a
    else:
      return c
  elif a > c:
    return a
  elif b > c:
    return c
  else:
    return b

a = int(input())
b = int(input())
c = int(input())
print(f'중앙값은 {med3(a, b, c)} 입니다.')

# 4 : 중앙값 구하기 2
def med3(a, b, c):
  if (b >= a and c <= a) or (b <= a and c >= a):
    return a
  elif (a > b and c < b) or (a < b and c > b):
    return b
  return c

