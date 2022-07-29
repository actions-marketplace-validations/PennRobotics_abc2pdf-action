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
