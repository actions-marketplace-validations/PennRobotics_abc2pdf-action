# Engrave Sheet Music from ABC - Github Action

Using the [abc2svg documentation](https://chiselapp.com/user/moinejf/repository/abc2svg/doc/trunk/README.md),
a Github action was prepared on this repository which uses `fossil` (retrieve/sync the **abc2svg** repo), `ninja` (build), and the compiled **abc2svg** scripts to convert an *abc*-format file into a *pdf*.

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

- [ ] move `abc2svg` files into own subdirectory (not the workspace top-level)
- [ ] take input file as action argument
- [ ] assume output has same name (changed to *pdf* extension) unless explicitly given
- [ ] create action for batch conversion
- [ ] possible to call GH action from another repo? (e.g. publish the action) caveat: would require syncing with source repo, possibly difficult to automate unless actions can run on schedule
