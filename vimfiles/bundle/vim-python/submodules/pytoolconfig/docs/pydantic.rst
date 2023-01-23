Pydantic
========

PyToolConfig supports optional pydantic validation.
To use it, install pytoolconfig with the validation extra.
Then, use the pydantic dataclass decorator instead of the standard library one.

Fields
------
Furthermore, you can now use pydantic fields instead of pytoolconfig fields. This will allow for you to add validators and other features into the fields.

.. code-block:: python

    from pydantic import Field
    from pydantic.dataclasses import dataclass


    @dataclass
    class Config:
        example: str = Field(
            "I don't know", description="you figure it out", command_line=("-f", "-foo")
        )
