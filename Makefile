# Specify your environment by changing following entries.

# underlying Lisp program name
LISP	= sbcl --noinform --disable-debugger --script

# installation target directory
BIN	= /usr/local/bin

# A change of the next entry must be reflected on the definition for
# the variable "*R2SYS-DUMP-PATHNAME*" in src/defabclr2.lsp.
# name of dumped image file
IMAGE	= saved_abclr2

# source directory
SRCDIR	= src
# name of shell script
ABCLR2	= $(BIN)/abclr2

all:
	(cd $(SRCDIR); $(LISP) make-abclr2.lsp)

# installation ABCL/R2 as a standalone SBCL executable
install-sbcl: $(SRCDIR)/$(IMAGE)
	cp $(SRCDIR)/$(IMAGE) $(ABCLR2)
	chmod +x $(ABCLR2)

# installation ABCL/R2 on KCl
install-kcl:	$(SRCDIR)/$(IMAGE)
	mv $(SRCDIR)/$(IMAGE) $(BIN)
	echo '#!/bin/sh' > $(ABCLR2)
	echo '$(BIN)/$(IMAGE) $(BIN)' >> $(ABCLR2)
	chmod +x $(ABCLR2)

# installation ABCL/R2 on CMU-CL
install-cmu:	$(SRCDIR)/$(IMAGE)
	mv $(SRCDIR)/saved_abclr2 $(BIN)
	echo '#!/bin/sh' > $(ABCLR2)
	echo 'lisp -core $(BIN)/$(IMAGE)' >> $(ABCLR2)
	chmod +x $(ABCLR2)

# installation ABCL/R2 on Allegro-CL
install-acl:	$(SRCDIR)/$(IMAGE)
	mv $(SRCDIR)/saved_abclr2 $(ABCLR2)

# installation on Allegro-CL 5.0 (beta)
install-acl5:	#$(SRCDIR)/$(IMAGE)
	mv $(SRCDIR)/saved_abclr2 $(BIN)/saved_abclr2.dxl
	echo "#!/bin/sh" > $(ABCLR2)
	echo "exec lisp -I $(BIN)/saved_abclr2.dxl -e '(r2sys::startup)'" >> $(ABCLR2)
	chmod +x $(ABCLR2)

# installation ABCL/R2 on CLISP
CLISP	= /usr/local/lib/clisp/lisp.run	# clisp executable
install-clisp:	$(SRCDIR)/$(IMAGE)
	mv $(SRCDIR)/$(IMAGE) $(BIN)
	echo '#!/bin/sh' > $(ABCLR2)
	echo '$(CLISP) -M $(BIN)/$(IMAGE) -q -x "(r2sys:startup)"' >> $(ABCLR2)
	chmod +x $(ABCLR2)
clean:
	-/bin/rm -f $(SRCDIR)/*.o $(SRCDIR)/*.sparcf $(SRCDIR)/*.fasl \
		$(SRCDIR)/saved_abclr2
