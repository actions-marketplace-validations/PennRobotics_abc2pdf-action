# abc2pdf

_Github Action for Engraving Sheet Music from ABC_


## Quick start

* Fork, clone, or import the [abc2pdf-action repository](https://github.com/PennRobotics/abc2pdf-action.git)
* Upload **.abc** files into `in/`
* Monitor the status of the document conversion in the GitHub _Actions_ tab
* If an error occurs, [open an issue](https://github.com/PennRobotics/abc2pdf-action/issues/new/choose)
* Generated PDF output shall upload after a short delay into `out/` with the same filename base as each input


## Sample ABC

```
X: 1
T: Good King Wenceslas
M: 4/4
L: 1/4
C: Traditional
K: F
|: FFFG | FF C2 | DCDE | F2 F2 :|
   cBAG | AG F2 | DCDE | F2 F2 |
   CCDE | FF G2 | cBAG | F2 B2 | F4 ||
```
