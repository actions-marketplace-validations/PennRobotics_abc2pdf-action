# Engrave Sheet Music from ABC - Github Actions

To create PDF files from ABC, I've created a process using Github Actions:

* linking the **abcm2ps** repo as a submodule
* compiling the **abcm2ps** source and saving the binary inside this repository
* running the compiled program against uploaded ABC inputs


## TODO

- [ ] move the TODO list into "Projects"
- [ ] create a document containing desired ABC engraving options (the immediate obvious issue is "measures per line")
- [ ] update binary iff out-of-date
- [ ] uploading an ABC file (or multiple ABC files) into **/in** should start the PDF generation process
