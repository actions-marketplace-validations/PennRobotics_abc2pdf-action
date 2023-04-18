# Engrave Sheet Music from ABC - Github Action

Using the [abc2svg documentation](https://chiselapp.com/user/moinejf/repository/abc2svg/doc/trunk/README.md),
a Github action was prepared on this repository which uses `fossil` (retrieve/sync the **abc2svg** repo), `ninja` (build), and the compiled **abc2svg** scripts to convert an *abc*-format file into a *pdf*.

This worked when copying the remote repository into this one, but this led to quite a bit of clutter and plenty of unneeded files as well as binary-style (aka blob) scripts. Thus, I have gone a new direction: linking the **abcm2ps** repo, compiling (ideally only once per remote update), and running the compiled program against uploaded inputs.

## Input

```
/abc/agora.abc
```

## Output

```
/out/agora.pdf
```

https://raw.githubusercontent.com/PennRobotics/test-actions-for-abc2svg/main/out/agora.pdf

## TODO

- [ ] create a document containing desired ABC engraving options (the immediate obvious issue is "measures per line")
- [ ] take input file as action argument
- [ ] assume output has same name (changed to *pdf* extension) unless explicitly given
- [ ] create action for batch conversion
