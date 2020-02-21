.. traditional_example

.. _`traditional_example_link`:

Traditional Example
===================


For those with `NASA CEA On Line <https://cearun.grc.nasa.gov/>`_ experience
or with desktop application experience,
a more traditional run might consist of an output **file** with multiple values for chamber pressure,
mixture ratio, subsonic area ratio, supersonic area ratio and Pc/Pexit ratio.

That traditional output is created by calling the RocketCEA function **get_full_cea_output**.

When calling **get_full_cea_output**, many of the parameters allow for either a single
value or a list of values to be input.

The following example shows how to run a case with multiple input values in order to
generate that more traditional output.

.. literalinclude:: ./_static/example_scripts/full_output_multiparam.py

.. note:: 

    Notice... the input Pc units and output units are controlled by the parameters **pc_units** and 
    **output** respectivley.

The following is the output that results from the above call to **get_full_cea_output**.

.. literalinclude:: ./_static/example_scripts/full_output_multiparam.txt
