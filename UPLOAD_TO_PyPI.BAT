

@echo off
setlocal

rem Change "do_the_upload=FALSE" to "do_the_upload=TRUE" to run the upload
rem AFTER... satisfying upload check list
set "do_the_upload=FALSE"

if "%do_the_upload%" == "TRUE" (
    echo ...............................................................
    echo doing the upload
    echo ...............................................................
    C:\Python310\Scripts\twine.exe upload --config-file "%HOMEPATh%\.pypirc" --skip-existing dist/*.*
    echo ....................... DONE ..................................
) else (
    echo ...................... CHECK LIST .............................
    echo ...............................................................
    echo Install from local source afer all changes made
    echo pip install .
    echo ...............................................................
    echo RUN pytest
    echo ...............................................................
    echo Update HISTORY.rst    README.rst    AUTHORS.rst
    echo ...............................................................
    echo Make sure that sphinx HTML is built with _BUILD_SPHINX_HTML.BAT
    echo ...............................................................
    echo COMMIT all files in git
    echo ...............................................................
    echo Make sure that "python -m build" has been executed
    echo        AND any "python -m build --wheel"
    echo ...............................................................
    echo Run "twine check" to see if long description will render correctly
    echo twine check dist/*            OR 
    echo twine check dist/*24.tar.gz (where: 24 is current version)
    echo ...............................................................
    echo THEN: set "do_the_upload=TRUE" to run the upload
    echo AFTER UPLOAD: set "do_the_upload=FALSE"
    echo ...............................................................
)

endlocal


rem use:  twine check dist/*  OR twine check dist/*24.tar.gz (where: 24 is current version)
