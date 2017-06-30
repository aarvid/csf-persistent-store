WARNING: This project is a WORK IN PROGRESS. 
         It does not (yet) contain working code.

CSF-Persistent-Store has its origins in the ChangeSafe code released by
Joe Marshall aka JRM. The announcement of the code release is here:

  http://funcall.blogspot.com/2013/07/a-little-lisp.html

The code was downloaded from the jrm-code-project here:

  https://code.google.com/p/jrm-code-project/source/browse/#svn%2Ftrunk%2FChangeSafe

This project attempts to extract the code necessary for a persistent-store
library for open-source common lisp implementations and bring it up-to-date
with current conventions and libraries (i.e. asdf, closer-mop, cl-fad, 
local-time, ...).  

The original code is monolithic, written for lispworks and maybe allegro
on windows, and certainly optimized for lispworks.

Current status:
   1. extracted the code and files necessary to compile on sbcl.
      As much as possible I have retained the original names of the
      files.
   2. added asdf definition files for two systems: csf-utility, 
      csf-persistent-store
   3. added package files using defpackage.
   4. compiles on sbcl 1.1.8 with no warnings or errors but with lots of
      notes concerning optimizations.
   5. somethings are marked with !!!. They can be any of the following:
      a. needs to be looked at
      b. changed but not sure the change is correct.
      c. probably just wrong.
   6. Others are marked $$$, these are changes to the code 
      that are probably correct.

To do:
   1. Understand the persistent store object model and have it
      correctly use closer-mop.
   2. fix the lower level stuff that writes to the disk. 
   3. get working on SBCL maybe without the optimizations.
   4. break the monolithic csf-utility into separate packages
      and maybe use other semi-standard portable quicklisp libraries such
      as babel, local-time, ... as replacements.  Local-time was added as 
      a dependency and used in timestamp.lisp but it could possibly 
      replace timestamp.lisp.
   5. get working on CCL
   6. get working on CLisp if possible
   7. get working on ECL
   8. get working on ABCL
   9. get working on Lispworks.

     







