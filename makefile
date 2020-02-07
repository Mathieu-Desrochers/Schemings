SOURCES = $(wildcard sources/foreign-interfaces/*.scm) \
          $(wildcard sources/units/*.scm)

OBJECTS = $(SOURCES:.scm=.o)

MACROS = $(wildcard sources/macros/*.scm)

TYPES = $(SOURCES:.scm=.types)

TYPES_CHECK = $(SOURCES:.scm=.types.check)

BUILD = build/macros.scm \
        build/object.o \
        build/types

all : $(BUILD) main tags

%.o : %.scm build/macros.scm
	csc5 -I/usr/include -I/usr/local/include \
	-C -Wno-pointer-sign -C '`pkg-config --cflags MagickWand`' \
	-extend build/macros.scm -emit-types-file $(subst .scm,.types,$<) -c $< -o $@

%.types.check : %.scm build/types
	csc5 -I/usr/include -I/usr/local/include \
	-C -Wno-pointer-sign -C '`pkg-config --cflags MagickWand`' \
	-types build/types -c $< -o $@

$(BUILD) : | build-directory

build-directory :
	@mkdir -p build

build/macros.scm : $(MACROS)
	cat $^ > $@

build/object.o : $(OBJECTS)
	ld -r $(OBJECTS) -o build/object.o

build/types : $(OBJECTS)
	cat $(TYPES) > $@

main : build/macros.scm build/object.o build/types sources/main.scm
	csc5 -L '-lb64 -lconfig -lcurl -letpan -lfcgi -lhungarian -licuuc' \
	-L '-licui18n -ljansson -lmsgpackc -lpcre -lsodium -lsqlite3 -lzmq' \
	-L '`pkg-config --libs MagickWand`' \
	-extend build/macros.scm -types build/types build/object.o \
	sources/main.scm -o main

tags : $(OBJECTS)
	ctags -R --languages=scheme

check-types : $(TYPES_CHECK)

install :
	mkdir -p /usr/local/Schemings
	cp build/* /usr/local/Schemings

clean :
	find . -name '*.o' | xargs /bin/rm -f
	find . -name '*.types' | xargs /bin/rm -f
	rm -rf build
	rm -f tags
	rm -f main
