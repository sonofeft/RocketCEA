
rem set python path variable (Default is Python 3.5 32 bit)
IF "%1"=="" ( SET "MYPYTHONPATH=D:\Python35" ) ELSE ( SET "MYPYTHONPATH=D:\Python%1" )

rem Make sure that PATH is as simple as possible
set PATH=C:\MinGW\mingw32\bin;C:\MinGW\mingw32\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts

pip uninstall -y rocketcea

python setup.py develop 

rem Test the compiled module
python .\rocketcea\examples\quick_chk.py
