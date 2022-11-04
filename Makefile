
f90 = gfortran

opt = -O3 -Jsrc
opt = -O3 -Jsrc -pg
opt = -O3 -Jsrc -fopenmp -flto -Ofast

obj = \
  src/const.o \
  src/input.o \
  src/vector_product.o \
  src/normalize.o \
  src/normal.o \
  src/centre.o \
  src/lambert.o \
  src/lommel.o \
  src/hapke.o \
  src/read_input.o \
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
  src/boundingbox.o \
  src/shadowing.o \
  src/scattering.o \

inc = \
  src/integrate_over_S.inc \
  src/integrate_scattered.inc \
  src/integrate_thermal.inc \

src/lc_triangle: src/lc_triangle.f90 $(obj) $(inc)
	$(f90) $(opt) $(obj) -o $@ $<

$(obj): %.o:%.f90
	$(f90) $(opt) -o $@ -c $<

clean:
	rm src/*.mod
	rm $(obj)

