# abc2pdf

_[Github Action for Engraving Sheet Music from ABC](https://github.com/marketplace/actions/abc-to-pdf)_

> **Warning**  
> Currently, the correct workflow needs to be _de-commented-out_ and run manually through the "Actions" tab.  
> Some of these steps need to move to **action.yml**


## TODO

- [X] `Error: PennRobotics/abc2pdf-action/first-nonworking-draft/action.yml (Line: 18, Col: 7): Required property is missing: shell`
- [ ] `converter/abctopdf: could not find a javascript interpreter - abort`
- [ ] Fix any blockers in the [Eventual quick start](#eventual-quick-start) and delete the other two quick start sections


## Quick start

* Upload **.abc** files into `in/`
* Create a new workflow
* Go to the Marketplace link and click the big button to use the latest version of the Action
* Add the displayed code snippet as a step in your workflow
* Run the workflow, making sure the output is saved as an artifact or committed to the repository


## Eventual quick start

* Fork, clone, or import the [abc2pdf-template repository](https://github.com/PennRobotics/abc2pdf-template.git)
* Upload **.abc** files into `in/`
* The upload should automatically trigger a workflow using this GitHub Action
* Monitor the status of the document conversion in the GitHub _Actions_ tab
* If an error occurs, [open an issue](https://github.com/PennRobotics/abc2pdf-action/issues/new/choose)
* Generated PDF output shall upload after a short delay into `out/` with the same filename base as each input


## Legacy quick start

* Fork, clone, or import the [abc2pdf-action repository](https://github.com/PennRobotics/abc2pdf-action.git)
* Upload **.abc** files into `in/`
* The upload should automatically trigger the GitHub Actions for compiling the latest converter source and converting each input document
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
