
.. quickstart

QuickStart
==========
.. note::
    
    RocketCEA on Windows has become much easier for versions 1.2.0 and above.

    In fact, in most situations, Windows users can skip the step of installing MinGW gfortran 
    and simply install a pre-compiled binary wheel for python 3.7 through 3.12.

    The deprecation of numpy.distutils forced a FORTRAN build conversion to `meson <https://mesonbuild.com/>`_ 
    that has turned out to be a nice improvement. (Many thanks to `joel-martin <https://github.com/joel-martin>`_ for 
    critical help with that conversion.)
    

Install Numpy & Matplotlib
--------------------------

When installing RocketCEA from source code or from a 
`PyPI sdist <https://packaging.python.org/en/latest/specifications/source-distribution-format/>`_,
RocketCEA depends on `f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_ to
compile the FORTRAN `NASA CEA code <https://www1.grc.nasa.gov/research-and-engineering/ceaweb/>`_ 
and `f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_
is part of `numpy <https://numpy.org/>`_. After compilation, `numpy <https://numpy.org/>`_ 
will needed to interface with the compiled code.

RocketCEA also makes use of the `matplotlib <https://matplotlib.org/>`_ package for 
creating plots.

An optional package for some advanced features is `scipy <https://scipy.org/>`_.

To install `numpy <https://numpy.org/>`_, `matplotlib <https://matplotlib.org/>`_ and `scipy <https://scipy.org/>`_, 
give the commands::
    
    pip install numpy matplotlib scipy
    
    OR perhaps...
    
    pip install --upgrade numpy
    pip install --upgrade matplotlib
    pip install --upgrade scipy

Some Linux systems may require::

    sudo pip install numpy matplotlib scipy
    
    OR perhaps...
    
    sudo pip3 install numpy matplotlib scipy


Install Compiler
----------------
.. note::

    Windows users can skip "Install Compiler" and go to 
    :ref:`link_install_rocketcea`


Using `f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_ to compile FORTRAN requires
a FORTRAN compiler. I recommend using `gfortran <https://www.gnu.org/software/gcc/fortran/>`_ 
on all platforms so that there are no FORTRAN incompatibilities between platforms.

Each operating system has its own approach to install `gfortran <https://www.gnu.org/software/gcc/fortran/>`_ 


Click: :ref:`link_installgfortran` to see install instructions for a few platforms that I have tested.
    

.. _link_install_rocketcea:

Install RocketCEA
-----------------

**After** the above installs have been accomplished, the easiest way to install RocketCEA is::

    pip install rocketcea
        OR perhaps
    pip install --prefer-binary rocketcea
    
        OR on Linux
    sudo pip install rocketcea
    sudo pip3 install rocketcea
        OR perhaps
    pip install --user rocketcea
    pip3 install --user rocketcea

Try a quick test of the install by pasting the following into a command terminal::

    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
    
    should result in:
    374.30361765576265

.. _link_windows_bat_file:

Windows Batch File
------------------

**For versions of RocketCEA 1.2.0 and above**

While the above section should work for Windows binary wheel installs, it is now much easier to install from 
source code as well.

It will be necessary to :ref:`link_installgfortran`, however, once FORTRAN is available, the steps to 
compile and install RocketCEA are virtually identical to installing binary wheels.

.. code-block:: batch

    REM =============== Install RocketCEA from Source Code ================

    REM make sure that gfortran is in PATH
    set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%PATH%
    
    REM **OPTIONALLY** install some dependencies
    pip install numpy matplotlib scipy

    REM install source downloaded from PyPI (e.g. rocketcea-1.2.0.tar.gz OR uncompressed source)
    pip install rocketcea-1.2.0.tar.gz
        ... OR ...
    pip install .

    REM Test the compiled module
    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"

    python -c "from rocketcea.cea_obj import __version__;  print( 'RocketCEA Version:', __version__)"



Old Windows Batch File
----------------------

.. note::

    3/23/2024: Ignore the following batch file for RocketCEA versions 1.2.0 and above.


.. note::

    3/14/2024: The deprecation of numpy.distutils has made Windows installs much more problematic.

    In order to support deprecated features in pip and numpy, we need to use old versions of these packages.

    For this reason, you may want to install RocketCEA in a `virtual environment <https://www.freecodecamp.org/news/how-to-setup-virtual-environments-in-python/>`_ rather than your primary installation.

    Further, until `meson <https://mesonbuild.com/>`_ is implemented, RocketCEA only officially supports python 3.7 thru 3.11 on Windows.

.. note::

    for `Anaconda <https://www.anaconda.com/products/individual>`_ on Windows, see "Anaconda Windows Batch File" section below.

RocketCEA on Windows can be problematic. Often the problem is in the PATH environment variable
where the wrong files are found for the intended install, and even more often it is because 
the proper compiler build tools are not available.

The Windows batch file below addresses both of those problems.

Remember that the FORTRAN compiler must also be installed
(see: :ref:`Install MinGW on Windows 10 <link_installmingw>`)

Notice that the batch file makes the PATH as simple as possible so that only the 64 bit MinGW files 
and desired python files are found.

Note that the batch file assumes that python 3.9 64 bit is the python version installed at **C:\\Python39_64**
and that MinGW 64 bit is installed at **C:\\MinGW\\mingw64\\bin** and **C:\\MinGW\\mingw64\\lib**.
*Edit those path names for your situation.*

The batch file starts by uninstalling rocketcea in case bad files are left from previous attempts.
(This would not be necessary in a fresh virtual environment.)

Finally, note that the batch file installs RocketCEA version 1.1.34 which is `likely` 
the last version to depend on numpy.distutils.

**You may need to edit the hard-coded paths to your own location of python and MinGW.**

- Copy and paste the batch file code below into an editor 
- Edit the hard-coded paths to your own location of python and MinGW
  (i.e. perhaps change C:\\Python39_64 and C:\\MinGW\\mingw64)
- Save the edited BAT file (e.g. as RUN_SETUP_BUILD_WIN64.BAT)
- Open a command prompt terminal and navigate to the BAT file directory.
- Give the command RUN_SETUP_BUILD_WIN64.BAT

.. note::

    3/14/2024: BAT File successful on Windows 11 virtual environments with 64 bit python 3.7 thru 3.11

    The virtual environments had the **parent** python version's subdirectories "libs" and "include" copied into them.

.. code-block:: batch

    rem =============== RUN_SETUP_BUILD_WIN64.BAT ================
    
    rem Make sure that PATH is as simple as possible
    SET "MYPYTHONPATH=C:\Python39_64"
    set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts

    rem OR perhaps simply...
    set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%PATH%


    REM The trick for installing RocketCEA on Windows seems to be 
    REM pre-installing the dependency packages (e.g. numpy, scipy and matplotlib) 
    REM so that the pip option (--global-option) for rocketcea only applies to rocketcea.

    rem pip 24.2 will enforce --global-option deprecation
    rem     ... so use an older pip
    python -m pip install pip==23.3.2

    rem future is removed after rocketcea 1.1.34
    pip install future

    pip install build

    pip install wheel==0.38.4

    rem in case of an earlier failed attempt to install rocketcea
    pip uninstall -y rocketcea

    rem setuptools.find_packages is used.
    rem for python 3.7 use:  pip install setuptools==59.8.0
    pip install setuptools==69.1.1

    pip install numpy<=1.26.4
    rem pip install numpy==1.25.2

    pip install pillow

    pip install matplotlib

    pip install scipy

    rem pip install rocketcea, but with global options.
    pip install --global-option build_ext --global-option --compiler=mingw32 rocketcea==1.1.34

    rem Test the compiled module
    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"

    python -c "from rocketcea.cea_obj import __version__;  print( 'RocketCEA Version:', __version__)"

    

.. note::

  The "trick" for installing RocketCEA on Windows seems to be pre-installing the dependency packages 
  (e.g. numpy, scipy and matplotlib) so that the pip options for rocketcea only apply to rocketcea.

  If you are having trouble on Windows, you may want to try :ref:`link_windows_wsl`


Anaconda Windows Batch File
---------------------------

.. note::

    3/14/2024: I am **Guessing** that the deprecation of numpy.distutils has made Anaconda installs **only** viable
    in a `virtual environment <https://www.freecodecamp.org/news/how-to-setup-virtual-environments-in-python/>`_ .
    I'm also **Guessing** that, on Windows, an Anaconda virtual environment would be successful with the 
    batch file shown above in  :ref:`link_windows_bat_file`

    An older install of Anaconda may work with the **old** and **possibly outdated** approach below.


As stated above, the "trick" for installing RocketCEA on Windows seems 
to be pre-installing the dependency packages  (e.g. numpy, scipy and matplotlib) 
so that the pip options for rocketcea only apply to rocketcea.

On Anaconda, many of the required dependencies are preinstalled.

To test the required installs, open an 
Anaconda prompt, launch python and attempt to import each package.
(An example is shown below for Anaconda with python 3.9)

.. image:: ./_static/Anaconda_check_installs.jpg
    
Notice that a fresh install of Anaconda does not include the 
`pillow <https://pypi.org/project/Pillow/>`_ package.

use the command::

    conda install -c anaconda pillow

to install `pillow <https://pypi.org/project/Pillow/>`_ . The results will look
something like the image below.

.. image:: ./_static/Anaconda_install_pillow.jpg

With all the dependencies handled, the batch file can be much more simple 
than the one shown above. Before you can run it, you need to verify the location
of the python folders under Anaconda.

To find your python folder, open an Anaconda prompt, launch python, import sys 
and give the command "sys.executable" as in the image below.

.. image:: ./_static/Anaconda_find_python_exe.jpg

In my case, the path was::

    C:\Users\Charlie\anaconda3

Yours will likely be similar.

For the final step, edit the batch file below to replace **<YourUserName>** with 
the user name shown in the python path above.

.. code-block:: batch

    rem NOTICE: MUST Verify dependency installs prior to running this batch file.

    SET "MYPYTHONPATH=C:\Users\<YourUserName>\anaconda3"

    rem Make sure that PATH is as simple as possible... For Anaconda notice "\Libary\bin"
    set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts;%MYPYTHONPATH%\Library\bin

    pip install --global-option build_ext --global-option --compiler=mingw32 rocketcea

    rem Test the compiled module
    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"

Run the batch file and the results will hopefully be similar to the results 
shown below.


.. image:: ./_static/Anaconda_successful_install.jpg

Notice that the file ran the "quick test" at the bottom of the batch file and 
properly output the Isp of 374.30...

Getting Help
------------

The documentation for RocketCEA is the best place to start.

Go to `RocketCEA ReadTheDocs <https://rocketcea.readthedocs.io/en/latest/index.html>`_ 
for information.


Running RocketCEA
-----------------

Any use of RocketCEA begins with an import statement and an instance of CEA_obj::

    from rocketcea.cea_obj import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2')


In the above example, LOX and LH2 are called out, but any propellants on the :ref:`Propellants <propellants_link>` page can be used.

There are a large number of examples included in this document

For instance, look at :ref:`LOX/LH2 Performance <example_1_link>` on the :ref:`Standard Examples <std_examples_link>` page. 

To run an example, highlight the source code with your mouse, right click the highlighted code and select **Copy**. 
Paste that code into your text editor and save it to a python file.(for example D:\\rocketcea\\example_1.py).

    
Example files can be run with the command::

    python example1.py

Or, in many text editors hitting the **F5** key will execute the code.

Test The Install
----------------

Paste the following code into your text editor and save it to your test folder as basic_cea.py 
(for example, D:\\rocketcea\\basic_cea.py)::

    from rocketcea.cea_obj import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2')
    for mr in range(2,9):
        print(mr, C.get_Isp(Pc=100.0, MR=mr, eps=40.0) )

At the command prompt, give the command::

    python basic_cea.py
    
If you see the following output::

    (2, 424.3597085736007)
    (3, 445.44434236555196)
    (4, 453.13271951921837)
    (5, 453.240429182719)
    (6, 448.190232998362)
    (7, 438.74340042907266)
    (8, 424.6998266323161)

Great... you are good to go.

If not, see the information below.

Google Colaboratory
-------------------

If you are having trouble installing RocketCEA on your system,
RocketCEA can be run on `Google Colaboratory <https://colab.research.google.com/notebooks/welcome.ipynb>`_


`Colaboratory <https://colab.research.google.com/notebooks/welcome.ipynb>`_ 
is a free Jupyter notebook environment that requires no setup and runs entirely in the cloud.

After creating a Colaboratory notebook, install RocketCEA.::

    !pip install RocketCEA


.. image:: ./_static/colab_pip_rocketcea.jpg
    :width: 60%


If Needed, install libgfortran3::

    !apt-get install libgfortran3
    
.. image:: ./_static/colab_apt_libgfortran3.jpg
    :width: 70%

Create a python script to run RocketCEA::

    %%file chk_cea.py
    from rocketcea.cea_obj import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2')
    for mr in range(2,9):
        print(mr, C.get_Isp(Pc=100.0, MR=mr, eps=40.0) )
    
.. image:: ./_static/colab_save_pyfile.jpg
    :width: 60%

And then run the file::

    !python chk_cea.py

.. image:: ./_static/colab_run_chk_cea.jpg
    :width: 50%

Colab plots work with RocketCEA as well.

.. image:: ./_static/colab_cstar_plot_example.jpg
    :width: 80%

.. _link_windows_wsl:

Windows 10 or 11 with WSL
-------------------------

RocketCEA can also be installed on `Windows Subsystem for Linux (WSL) <https://docs.microsoft.com/en-us/windows/wsl/install-win10>`_

After setting up your Linux distribution on WSL, installing RocketCEA is quick and easy.

For example on a fresh Ubuntu distribution, the following
will install python 3.10.12 and rocketcea 1.1.34. 

(Tested March 14, 2024)
   
*  Update Ubuntu
    * sudo apt update && upgrade
   
*  configure Ubuntu for python3 and FORTRAN
    * sudo apt install python3 python3-pip ipython3
    * sudo apt-get install gfortran

*  Install RocketCEA (required libraries should be available)
    * pip install rocketcea==1.1.34
   
*  Test installation with quick example
    * python3 -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
    * SHOULD RETURN:  374.30361765576896 (or perhaps just very close to it.)

WSL GUI
~~~~~~~

Note that, if desired, `VsXsrv <https://sourceforge.net/projects/vcxsrv/files/latest/download>`_ 
can be used to enable Linux-driven graphic windows on WSL.
 
This will enable matplotlib show() command, tkinter programs or any gui-based Linux application
installed on the WSL Linux distribution.

WSLg
~~~~

In the presumably near future, `WSLg <https://github.com/microsoft/wslg>`_ will be available that has GUI support built in.
Windows 11 is expected to come with `WSLg <https://github.com/microsoft/wslg>`_ 
and Windows 10 will be able to install `WSLg <https://github.com/microsoft/wslg>`_.
