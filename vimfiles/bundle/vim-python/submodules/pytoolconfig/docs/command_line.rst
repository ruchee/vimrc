Command Line Overwrites
=======================

PyToolConfig allows you to parse command line overwrites for configuration items in the base table/model.

1. In the Configuration Model, set the ``command_line`` value in the ``Field`` constructor. (This only works for items in the base model)
    For example:

    .. code-block:: python

        from pytoolconfig import dataclass, field


        @dataclass
        class NestedModel:
            foo_other: str = field(description="w", default="no", command_line=("--foo", "-f"))

2. Pass an Argument Parser to the ``PyToolConfig`` constructor
3. (Optional) Pass arguments to the ``parse()`` command
