Documentation
=============

PyToolConfig uses sphinx to handle automatic documentation generation.
1. Install the `pytoolconfig[doc]` extra
2. Add the `pytoolconfig.documentation` and `sphinx.ext.autodoc` extensions to your sphinx configuration.

Table Documentation
-------------------
The `autopytoolconfigtable` directive can be used to generate a table for a single pytoolconfig dataclass.

.. code-block:: rst

  .. autopytoolconfigtable:: pytoolconfig.universal_config.UniversalConfig

The supplied argument must be a dataclass. Currently does not support nested dataclasses.
