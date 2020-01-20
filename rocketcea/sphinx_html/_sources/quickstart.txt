
.. quickstart

QuickStart
==========

Install Numpy & Matplotlib
--------------------------

Because RocketCEA depends on `f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_ to
compile the FORTRAN `NASA CEA code <https://www.grc.nasa.gov/WWW/CEAWeb/ceaHome.htm>`_ and
`f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_
is part of `numpy <https://numpy.org/>`_ ,
`numpy <https://numpy.org/>`_ will need to be installed.

RocketCEA also makes use of the `matplotlib <https://matplotlib.org/>`_ package for 
creating plots.

To install `numpy <https://numpy.org/>`_ and `matplotlib <https://matplotlib.org/>`_, give the commands::
    
    pip install numpy
    pip install matplotlib
    
    OR perhaps...
    
    pip install --upgrade numpy
    pip install --upgrade matplotlib

Some Linux systems may require::

    sudo pip install numpy
    sudo pip install matplotlib


Install Compiler
----------------

Using `f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_ to compile FORTRAN requires
a FORTRAN compiler. I recommend using `gfortran <https://www.gnu.org/software/gcc/fortran/>`_ 
on all platforms so that there are no FORTRAN incompatibilities between platforms.

Each operating system has its own approach to install `gfortran <https://www.gnu.org/software/gcc/fortran/>`_ 


Click: :ref:`link_installgfortran` to see install instructions for a few platforms that I have tested.

.. important::

    Windows users MUST put MinGW into environment PATH variable.
    (see: :ref:`link_windowspath`)
    
    C:\MinGW\mingw64\bin  OR  C:\MinGW\mingw32\bin
    
    and
    
    C:\MinGW\mingw64\lib  OR  C:\MinGW\mingw32\lib



Install RocketCEA
-----------------

**After** the above installs have been accomplished, the easiest way to install RocketCEA is::

    pip install rocketcea
    
        OR on Linux
    sudo pip install rocketcea
        OR perhaps
    pip install --user rocketcea

Try a quick test of the install by pasting the following into a command terminal::

    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
    
    should result in:
    374.30361765576265

Getting Help
------------

After installing with ``pip``, there should be a launch command line program called **rocketcea** or, on Windows, **rocketcea.exe**. 

From a terminal or command prompt window simply type::

    rocketcea

Your browser will launch with these RocketCEA help pages.

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

.. note::
    
    RocketCEA is compiled with the mingw and mingw-w64 gfortran compilers using default f2py options
    giving a "shared" `*.pyd` file that requires mingw libraries at run time.
    
    If you see the error: ``Import Error: DLL load failed: The specified module could not be found``
    You may need to install the MinGW Compiler Suite and perhaps even recompile RocketCEA in order 
    for RocketCEA to work (see below)
    
    On Windows, make sure the environment PATH variable is set properly (see: :ref:`link_windowspath`)

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
(either python3 or python2).

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
    :width: 70%

