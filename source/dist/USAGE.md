Distribution package for self contained desktop R applications
==============================================================

Usage Notes
-----------

The distribution of self-contained desktop R applications requires:

* A distributable R interpreter (R-Portable)
* An application launch framework

Distributable R interpreter
---------------------------
`~/dist/R-Portable`:
  A "vanilla" installation of R that has been made "portable" by contributors to
  the Portable Apps project.  This effectively creates an isolated installation
  of R that can be used along side a system installation.

  It is important to keep R-portable as "clean" as possible.  This will make it
  easy to upgrade without worrying (too much) about upgrading or reinstalling
  package dependencies used by applications.

Application launch framework
----------------------------
An extremely stripped down form of this is built into this app and is called from 
the directory above this one.

`~/run.bat`:
Uses `Rscript.exe` from the bundled R-Portable distribution specified above to run `~/app/app.R`, which launches the shiny app

`~/run-silent.vbs`:
Runs run.bat without showing the batch file window.

