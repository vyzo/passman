##
# Passman password manager.
#
# @file
# @version 0.1

$(eval UID := $(shell id -u))
$(eval GID := $(shell id -g))

default: linux-static-docker

build:
	$(GERBIL_HOME)/bin/gxpkg link passman /src || true
	$(GERBIL_HOME)/bin/gxpkg build passman

linux-static-docker:
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src:z \
	gerbil/alpine:master \
	make -C /src linux-static

linux-static: build
	$(GERBIL_HOME)/bin/gxc -o passman-bin -static \
	-cc-options "-Bstatic" \
	-ld-options "-static -lpthread -L/usr/lib64 -lssl -ldl -lcrypto -lz" \
	-exe passman/main.ss

clean:
	rm -f passman-bin

install:
	mv passman-bin /usr/local/bin/passman

# end
