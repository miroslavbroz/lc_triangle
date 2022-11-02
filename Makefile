
f90 = gfortran

opt = -O3
opt = -O3 -pg
opt = -O3 -fopenmp -Jsrc

obj = \
  src/const.o \
  src/vector_product.o \
  src/normalize.o \
  src/normal.o \
  src/centre.o \
  src/read_face.o \
  src/read_node.o \
  src/read_elem.o \
  src/write_face.o \
  src/write_node.o \
  src/write1.o \
  src/write2.o \
  src/surface.o \
  src/volume.o \
  src/planck.o \
  src/intersect_AB_t.o \
  src/shadowing.o \
  src/scattering.o \

src/lc_triangle: src/lc_triangle.f90 $(obj)
	$(f90) $(opt) $(obj) -o $@ $<

$(obj): %.o:%.f90
	$(f90) $(opt) -o $@ -c $<

clean:
	rm $(obj)
	rm src/*.mod

