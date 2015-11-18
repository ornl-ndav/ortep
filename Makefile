# Makefile for the ORTEP-III

FC=gfortran
TARGET= ortep3
SRC= *.f Makefile
OBJ= ortep.o
FFLAGS= -O2
LFLAGS= -lpgplot

all: ${OBJ}
	${FC} ${LFLAGS} -o ${TARGET} ${OBJ}

ortep-1.0.3.tar.gz: ${SRC}
	 tar -c -z -f ortep-1.0.3.tar.gz --transform='s,^,ortep-1.0.3/,' ${SRC}

install: ${TARGET}
	install -c -m 755 ${TARGET} ${DESTDIR}/usr/bin/${TARGET}

clean:
	rm -f *.o ${TARGET}
