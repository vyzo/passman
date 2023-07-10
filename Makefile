##
# Project Title
#
# @file
# @version 0.1

PROJECT := passman #;$(shell basename $(PWD))
$(eval UID := $(shell id -u))
$(eval GID := $(shell id -g))

default: linux-static-docker

build:
	$(GERBIL_HOME)/bin/gxpkg link $(PROJECT) /src || true
	$(GERBIL_HOME)/bin/gxpkg build $(PROJECT)

linux-static-docker:
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-e PROJECT=$(PROJECT) \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	gerbil/alpine:master \
	make -C /src linux-static

linux-static: build
	$(GERBIL_HOME)/bin/gxc -o passman-bin -static \
	-cc-options "-Bstatic" \
	-ld-options "-static -lpthread -L/usr/lib64 -lssl -ldl -lyaml -lz" \
	-exe passman/main.ss

clean:
	rm -f $(PROJECT)-bin

install:
	mv $(PROJECT)-bin /usr/local/bin/$(PROJECT)

# end
