// TODO: foo := -1 does not parse!!
foo := 999; // This one will be hidden all the time and is never touched.

{
    foo := 0; // Shadows foo at 3.
    updateFoo :: () {
        foo = foo + 1;
    }

    {
        foo := 100; // Shadows foo at 6 in current scope.
        updateFoo(); // Which foo?

        // The interpreter manages scope correctly, and the updateFoo updates
        // the foo declared at line 4, which was the visible one when updateFoo
        // was declared.
        assert: foo == 100; // This is unchanged.
    }

    assert: foo == 1; // This is updated by a funcall at 11.
}

assert: foo == 999; // Also unchanged.
