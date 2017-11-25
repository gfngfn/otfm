PREFIX=/usr/local/bin

all:
	ocamlbuild -use-ocamlfind otftrip.native
	mv otftrip.native otftrip-local

install:
	sudo install otftrip-local $(PREFIX)
