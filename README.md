# Engrave Sheet Music from ABC - Github Actions

To create PDF files from ABC, I've created a process using Github Actions:

* linking the **abcm2ps** repo as a submodule
* compiling the **abcm2ps** source and saving the binary inside this repository
* running the compiled program against uploaded ABC inputs
