## 1부터 n까지 정수의 합 구하기
n = int(input())

sum = 0
i = 1
while i <= n:
  sum += i
  i += 1

print(f'1부터 {n}까지 정수의 합은 {sum}입니다.')

## 1부터 n까지 정수의 합 구하기 2
n = int (input())

sum = 0
for i in range(1, n + 1):
  sum += i
  
print(f'1부터 {n}까지 정수의 합은 {sum}입니다.')

## a부터 b까지 정수의 합 구하기
a = int(input())
b = int(input())

# 오름차순 정렬
if a > b:
  a, b = b, a 

sum = 0
for i in range(a, b + 1):
  sum += i

print(f'{a}부터 {b}까지 정수의 합은 {sum}입니다.')

## a부터 b까지 정수의 합 구하기 1
a = int(input())
b = int(input())

if a > b:
  a, b = b, a

sum = 0
for i in range(a, b + 1):
  if i < b:
    print(f'{i} + ', end='')
  else:
    print(f'{i} = ', end='') 
  sum += i

print(sum)

## a부터 b 까지 정수의 합 구하기 2
a = int(input())
b = int(input())

if a > b:
  a, b = b, a

sum = 0
for i in range(a, b):
  print(f'{i} + ', end='')
  sum += i

print(f'{b} = ', end='')
sum += b

print(sum)

## +와 -를 번갈아 출력하기 1
n = int(input())

for i in range(n):
  if i % 2 :
    print('-', end='')
  else:
    print('+', end='')
    
print()
  
## +와 -를 번갈아 출력하기 1
n = int(input())

for i in range(1, n + 1):
  if i % 2 :
    print('-', end='')
  else:
    print('+', end='')
  
print()

# +와 -를 번갈아 출력하기 2
n = int(input())
for _ in range(n // 2):
  print('+-', end='')
  
if n % 2:
  print('+', end='')

print()

## *를 n개 출력하되 w개마다 줄바꿈 하기 1
n = int(input())
w = int(input())

for i in range(n):
  print('*', end='')
  if i % w == w - 1:
    print()
    
if n % w:
  print()
  
## *를 n개 출력하되 w개마다 줄바꿈 하기 2
n = int(input())
w = int(input())

for _ in range(n // w):
  print('*' * w)
  
rest = n % W
if rest:
  print('*' * rest)
