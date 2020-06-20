
with open("in.txt") as filein:
	for line in filein:
		w,h = line.split()
		rectangle = Rectangle(int(w),int(h));
		print("%dx%d rectangle, area = %d, perimeter = %d" %
			(rectangle.width, rectangle.height, rectangle.area(), rectangle.perimeter()))
