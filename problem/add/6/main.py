
# standard input is already processed
# as the submitted programmed is run when imported

with open("in.txt") as filein:
  for line in filein:
    x,y = [int(x) for x in line.split()]
    print(add(x,y))
