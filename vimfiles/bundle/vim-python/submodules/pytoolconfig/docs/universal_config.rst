Universal Configuration
=======================

Universal Configuration refers to configuration keys given by pytoolconfig. You can overwrite your own configuration keys with the ones provided by pytoolconfig.

In your configuration model, set the ``universal_config`` value in the `Field` constructor. (This only works for items in the base model). You still need to set a default value.
For example:

.. code-block:: python

    from pytoolconfig import dataclass, field, UniversalKey


    @dataclass
    class NestedModel:
        foo_other: str = field(
            description="w", default="no", universal_config=UniversalKey.min_py_version
        )

The value of this field will be overwritten by pytoolconfig's equivalent universal configuration field.

Available configuration keys:
-----------------------------

.. autopytoolconfigtable:: pytoolconfig.universal_config.UniversalConfig
