
with open("in.txt") as filein:
	for line in filein:
		w,h,u = line.split()
		rectangle = Rectangle(int(w),int(h),u);
		print("The area is %d square %s and the perimeter is %d %s." %
			(rectangle.area(), rectangle.unit, rectangle.perimeter(), rectangle.unit))

rectangle = Rectangle(1, 2, "m")

if rectangle.width != 1:
	print("Rectangle.width field is innaccessible")

if rectangle.height != 2:
	print("Rectangle.height field is innaccessible")
