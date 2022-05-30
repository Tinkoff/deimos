# deimos

This is a code generation script for [phobos](https://github.com/TinkoffCreditSystems/phobos) library. Example of use:
```
sbt "run xsd destination"
```
Where `xsd` is directory with xsd files and `destination` is destination directory for `.scala` sources.

This project is in alpha version, some features may not work. 
Integration with sbt for code generation during build will be implemented soon.
