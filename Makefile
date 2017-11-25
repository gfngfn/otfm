PREFIX=/usr/local/bin

all:
	ocamlbuild -use-ocamlfind otftrip.native
	mv otftrip.native otftrip-local

gsub:
	ocamlbuild -use-ocamlfind gsubtrip.native
	mv gsubtrip.native gsubtrip

install:
	sudo install otftrip-local $(PREFIX)
	sudo install gsubtrip $(PREFIX)

