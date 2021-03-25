import math
import numpy as np

yres = 43
xres = 160
line_length = xres//8+12
assert line_length == 32

fb_bitmap_size = 32 + int(math.ceil(yres/8)) * xres
assert fb_bitmap_size == 992

mapping = np.zeros([yres, xres], dtype=np.int32)
ys = - np.ones([fb_bitmap_size-32], dtype=np.int32)
xs = - np.ones([fb_bitmap_size-32], dtype=np.int32)


pos = 0
for row in range(0, yres, 8):
  for X in range(0, xres, 1):
    for off in range(8):
      Y = row + off
      if Y < yres and X < xres:
        mapping[Y, X] = 8*pos+off
        if ys[pos] < 0: ys[pos] = Y
        if xs[pos] < 0: xs[pos] = X
    pos += 1

